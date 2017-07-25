open Core

open Ast
open IType
open IIntType
open IFloatType
open IArithType
open Int_or_uint_64
open Uint64
open Utils

exception TypeError of string

let is_power_of_2 n =  n <> 0 && (n land (n - 1) = 0)

let signed_integer_types = IIntTypeSet.of_list [
                               Int8Type ;
                               Int16Type ;
                               Int32Type ;
                               Int64Type]
let is_signed_integer = IIntTypeSet.mem signed_integer_types

let unsigned_integer_types = IIntTypeSet.of_list [
                                 BoolType ;
                                 UInt8Type ;
                                 UInt16Type ;
                                 UInt32Type ;
                                 UInt64Type ]
let is_unsigned_integer = IIntTypeSet.mem unsigned_integer_types

let integer_types = IIntTypeSet.of_list IIntType.all

let signed_arith_types = ITypeSet.union
                           (ITypeSet.of_list [
                                A (F FloatType) ;
                                A (F DoubleType) ; ])
                           (ITypeSet.map ~f:iType_of_IntType signed_integer_types)


let arith_types = IArithTypeSet.of_list IArithType.all

let is_integer = function
  | A I _ -> true
  | _ -> false

let is_arith = function
  | A _ -> true
  | _ -> false

let is_signed_arith = function
  | I t -> IIntTypeSet.mem signed_integer_types t
  | F _ -> true

let is_unsigned_arith = function
  | I t -> IIntTypeSet.mem unsigned_integer_types t
  | F _ -> false

let integer_type_rank = function
  | BoolType                -> 0
  | Int8Type  | UInt8Type   -> 1
  | Int16Type | UInt16Type  -> 2
  | Int32Type | UInt32Type  -> 3
  | Int64Type | UInt64Type  -> 4

let int_sizeof = function
  | BoolType | Int8Type  | UInt8Type   -> 1
  | Int16Type | UInt16Type  -> 2
  | Int32Type | UInt32Type  -> 4
  | Int64Type | UInt64Type  -> 8

let arith_sizeof = function
  | I i -> int_sizeof i
  | F FloatType -> 4
  | F DoubleType -> 8

let unsigned_type = function
  | Int8Type -> UInt8Type
  | Int16Type -> UInt16Type
  | Int32Type -> UInt32Type
  | Int64Type -> UInt64Type
  | _ as t -> t

let integer_promotion t =
  let i = if is_signed_integer t then Int32Type else UInt32Type in
  if integer_type_rank t < integer_type_rank i then i else t

(** Usual arithmetic conversions, a.k.a. binary conversions. This function returns the type to which the two operands must be converted. Adopted from http://compcert.inria.fr/doc/html/Cop.html. Reference: C99 Section 6.3.1.8.
 *)
let usual_arithmetic_conversion promote t1 t2 =
  match t1, t2 with
  | F DoubleType, _ | _, F DoubleType -> F DoubleType
  | F FloatType, _ | _, F FloatType -> F FloatType
  | I i1, I i2 ->
     let j1 = if promote then integer_promotion i1 else i1 in
     let j2 = if promote then integer_promotion i2 else i2 in
     if eq_int_type j1 j2 then I j1 else
       (match is_unsigned_integer j1, is_unsigned_integer j2 with
        | true, true | false, false ->
           if integer_type_rank j1 < integer_type_rank j2 then I j2 else I j1
        | true, false ->
           if integer_type_rank j2 <= integer_type_rank j1 then I j1 else
             if int_sizeof j1 < int_sizeof j2 then I j2 else
               I (unsigned_type j2)
        | false, true ->
           if integer_type_rank j1 <= integer_type_rank j2 then I j2 else
             if int_sizeof j2 < int_sizeof j1 then I j1 else
               I (unsigned_type j1)
       )

let is_void = function
  | VoidType -> true
  | _ -> false


(* check if 'r' could be coerced (implictly casted) to 'l' even with possible loss of precision.  Our rules are stricter than in C99 *)
let rec check_coercion tfrom tto =
  match tto, tfrom with
  | VoidType , _         -> true
  | _        , VoidType  -> false
  | A _      , A _       -> true
  | A _      , PtrType _ -> false (* unlike C we do not allow cast between ints and ptr *)
  | A _      , ArrType _ -> false
  | A _      , VecType _ -> false
  | PtrType _, A _       -> false (* unlike C we do not allow cast between ints and ptr *)
  | ArrType _, A _       -> false
  | VecType _, A _       -> false
  | ArrType (lt,ll), ArrType (rt,rl) -> ll = rl && check_coercion rt lt
  | VecType (lt,ll), VecType (rt,rl) ->
     arith_sizeof lt = arith_sizeof rt
     && ll = rl
     && check_coercion (A rt) (A lt)
  | PtrType (lt, la), PtrType (rt, ra) -> lt=rt (* TODO: alignment? *)
  | ArrType (lt,ll), PtrType (rt, ra) -> lt=rt (* TODO: Check with Franz *)
  | PtrType (lt, la), ArrType (rt,rl) -> lt=rt (* TODO: Check with Franz *)
  | VecType _, PtrType _ -> false
  | PtrType _, VecType _ -> false
  | ArrType (lt,ll), VecType (rt,rl) -> ll = rl && is_power_of_2 ll && check_coercion (A rt) lt
  | VecType (lt,ll), ArrType (rt,rl) -> ll = rl && check_coercion rt (A lt)

(* check if 'r' could be explicitly casted to 'l' even with possible loss of precision.
   Our rules may be stricter than in C99 *)
let rec check_cast tfrom tto =
  match tto, tfrom with
  | PtrType _, PtrType _       -> true (* TODO: check alightment? *)
  | VecType (lt,ll), VecType (rt,rl) ->
     (* we allow to convert vectors as long as they are same byte size *)
     ll*(arith_sizeof lt) = rl*(arith_sizeof rt)
  | a ,b ->check_coercion a b

let rec func_type_arith_binop name al =
  let open List in
  if 2 <> length al then
    raise (TypeError (Format.asprintf "Invalid number of arguments for '%s'" name))
  else
    let a0 = nth_exn al 0 in
    let a1 = nth_exn al 1 in
    match a0 , a1 with
    | A ia0 , A ia1 -> A (usual_arithmetic_conversion true ia0 ia1)
    | VecType (vt1,l1), ArrType (A vt2, l2) | ArrType (A vt1,l1), VecType (vt2, l2) | VecType (vt1,l1), VecType (vt2, l2) ->
       if l1=l2 && check_coercion a0 a1 then
         VecType (usual_arithmetic_conversion true vt1 vt2, l1)
       else
         raise (TypeError
                  (Format.asprintf "Incompatible arguments types %a, %a for '%s'"
                                   pr_itype a0 pr_itype a1 name))

    | ArrType (A vt1, l1), ArrType (A vt2, l2) ->
       let va0 = VecType (vt1, l1) in
       let va1 = VecType (vt2, l2) in
       if check_coercion a0 va0 && check_coercion a1 va1 then
         func_type_arith_binop name [va0; va1]
       else
         raise (TypeError (Format.asprintf "Incompatible arguments types %a, %a for '%s'"
                                           pr_itype a0 pr_itype a1 name))
    | _ , _ -> raise (TypeError
                        (Format.asprintf "Incompatible arguments types %a, %a for '%s'"
                                         pr_itype a0 pr_itype a1 name))

let rec func_type_add name al =
  let open List in
  if 2 <> length al then
    raise (TypeError (Format.asprintf "Invalid number of arguments for '%s'" name))
  else
    let a0 = nth_exn al 0 in
    let a1 = nth_exn al 1 in
    match a0 , a1 with
    | A _ , A _ | VecType _, ArrType (A _,_) | ArrType (A _,_), VecType (_, _) | VecType (_,_), VecType (_, _) | ArrType (A _, _), ArrType (A _, _) -> func_type_arith_binop name al
    | PtrType _, A (I _) -> a0
    | A (I _), PtrType _ -> a1
    | _ , _ -> raise (TypeError
                        (Format.asprintf "Incompatible arguments types %a, %a for '%s'"
                                         pr_itype a0 pr_itype a1 name))

let func_type_arith_nop name al =
  let open List in
  if length al < 2 then
    raise (TypeError (Format.asprintf "Invalid number of arguments for '%s'" name))
  else if length al = 2 then func_type_arith_binop name al
  else fold ~init:(hd_exn al)
            ~f:(fun a b -> func_type_arith_binop name [a;b])
            (tl_exn al)

let func_type_bool_arith_binop name al = ignore ( func_type_arith_binop name al) ; A (I BoolType)

let ptr_attr_combine a1 a2 =
  match a1, a2 with
  | None, al2 -> al2
  | al1, None -> al1
  | Some n1, Some n2 -> Some (max n1 n2)

let rec type_combine ty1 ty2 =
  match ty1, ty2 with
  | VoidType, VoidType -> Some VoidType
  | A (F FloatType),  A (F FloatType) -> Some (A (F FloatType))
  | A (F DoubleType),  A (F DoubleType) -> Some (A (F DoubleType))
  | A (I t1), A (I t2) ->
     if is_signed_integer t1 = is_signed_integer t2 && int_sizeof t1 = int_sizeof t2
     then Some (A (I t1)) else None
  | PtrType (t1,a1), PtrType (t2, a2) ->
     (match type_combine t1 t2 with
      | Some t -> Some (PtrType (t, ptr_attr_combine a1 a2))
      | None -> None
     )
  | ArrType (t1,s1), ArrType (t2, s2) ->
     (match type_combine t1 t2 with
      | Some t -> if s1 = s2 then
                    Some (ArrType (t,s1))
                  else
                    None
      | None -> None
     )
  | _, _ -> None

(* Lifted from http://compcert.inria.fr/doc/html/Ctyping.html
 See also C99 section 6.5.15
 *)
let type_conditional ty1 ty2 =
  match ty1, ty2 with
  | A ia0 , A ia1 -> A (usual_arithmetic_conversion true ia0 ia1)
  | PtrType (t1,a1), PtrType (t2, a2) ->
     let t =
       if is_void t1 || is_void t2 then VoidType else
         match type_combine t1 t2 with
         | Some t -> t
         | None -> VoidType
     in PtrType (t, None)
  | PtrType (_,_) as t, A (I _) -> t
  | A (I _), (PtrType (_,_) as t) -> t
  | t1, t2 -> match type_combine t1 t2 with
              | Some t -> t
              | None -> raise (TypeError
                                 (Format.asprintf "Incompatible arguments for conditional operator:  %a and %a" pr_itype t1 pr_itype t2))

let func_type_cond name a =
  let open List in
  if length a <> 3 then
    raise (TypeError (Format.asprintf "Invalid number of arguments for '%s'" name))
  else
    let a0 = hd_exn a in
    (* first argument should be interpretable as boolean *)
    match a0 with
    | VecType _ | ArrType _ | VoidType -> raise (TypeError (Format.asprintf "Could not coerce 1st argument of '%s' to boolean type. Actual types: [%a]." name pr_itype a0))
    | A _ | PtrType _ ->
       (match nth_exn a 1, nth_exn a 2 with
        | _,_ -> func_type_arith_binop name (tl_exn a)
       )

(* 6.5.3.3 Unary arithmetic operators: "The result of the unary + operator is the value of its (promoted) operand. The integer promotions are performed on the operand, and the result has the promoted type". *)
let func_type_neg name a =
  let open List in
  if length a <> 1 then
    raise (TypeError (Format.asprintf "Invalid number of arguments for '%s'" name))
  else
    let a0 = hd_exn a in
    match a0 with
    | VecType _ -> a0
    | A I it -> A (I (integer_promotion it))
    | A _ -> a0 (* floats negated to the same type. Not dealing with signedness *)
    | _ -> raise (TypeError (Format.asprintf "Could not apply negation to non-arithmetic type [%a]." pr_itype a0))

(* 'abs' is polymorphic version of C99 abs, labs, fabsf, fabs*)
let func_type_abs name a =
  let open List in
  if length a <> 1 then
    raise (TypeError (Format.asprintf "Invalid number of arguments for '%s'" name))
  else
    let a0 = hd_exn a in
    match a0 with
    | A I it as t -> if is_signed_integer it then t
                     else raise (TypeError (Format.asprintf "Could not apply 'abs' to unsigned type [%a]." pr_itype a0))
    | A _ as t -> t (* floats negated to the same type. Not dealing with signedness *)
    | _ -> raise (TypeError (Format.asprintf "Could not apply 'abs' to non-arithmetic type [%a]." pr_itype a0))


let func_type_vushuffle name a =
  let open List in
  if 2 <> length a then
    raise (TypeError (Format.asprintf "Invalid number of arguments for '%s'" name))
  else
    let a0 = nth_exn a 0 in
    let a1 = nth_exn a 1 in
    match a0 , a1 with
    | VecType (_, _), A (I _) -> a1
    | VecType (_, vl), ArrType (A (I _), al) ->
       if al <> vl then
         raise (TypeError (Format.asprintf "Unexpected number size of vparam array for '%s'" name))
       else a1
    | _, _ -> raise (TypeError
                       (Format.asprintf "Incompatible arguments types %a, %a for '%s'"
                                        pr_itype a0 pr_itype a1 name))

let func_type_vbinop t name a =
  let open List in
  if 2 <> length a then
    raise (TypeError (Format.asprintf "Invalid number of arguments for '%s'" name))
  else
    let a0 = nth_exn a 0 in
    let a1 = nth_exn a 1 in
    if check_coercion a0 t && check_coercion a1 t then t
    else raise (TypeError
                  (Format.asprintf "Incompatible arguments types %a, %a for '%s'"
                                   pr_itype a0 pr_itype a1 name))

let func_type_vbinop_with_vparam t name a =
  let open List in
  if 3 <> length a then
    raise (TypeError (Format.asprintf "Invalid number of arguments for '%s'" name))
  else
    let a0 = nth_exn a 0 in
    let a1 = nth_exn a 1 in
    let a2 = nth_exn a 2 in
    if check_coercion a0 t && check_coercion a1 t (* TODO: enforce VPRAM type &&
         check_coercion a2  (A (I UInt16Type)) *) then t
    else raise (TypeError
                  (Format.asprintf "Incompatible arguments types %a, %a for '%s'"
                                   pr_itype a0 pr_itype a1 name))

let a_func_type eargs ret name args =
  if List.length eargs <> List.length args then
    raise (TypeError (Format.asprintf "Unexpected number of arguments for '%s'" name))
  else
    if not (List.map2_exn ~f:check_coercion eargs args |>
              List.fold ~f:(&&) ~init:true) then
      raise (TypeError (Format.asprintf "Incompatible arguments for '%s'" name))
    else
      ret

let builtins_map =
  String.Map.Tree.of_alist_exn
    [
      (* polymorphic codnition operator *)
      ("cond", func_type_cond);

      (* polymorphic artihmetic binary operators *)
      ("min", func_type_arith_nop) ;
      ("max", func_type_arith_nop) ;

      ("add", func_type_add ) ;
      ("sub", func_type_arith_binop) ; (* TODO: func_type_sub to handle pointer subtraction *)
      ("mul", func_type_arith_binop) ;
      ("div", func_type_arith_binop) ;

      (* polymorphic binary operators *)
      ("geq", func_type_bool_arith_binop) ;

      (* polimorphic unary operators *)
      ("neg", func_type_neg) ;
      ("abs", func_type_abs) ;

      ("vcvt_64f32f", a_func_type [(VecType (F FloatType, 4))] (VecType (F DoubleType, 2))) ;

      (* non-polymorphic functions *)
      ("addsub_4x32f", func_type_vbinop (VecType (F FloatType, 4))) ;
      ("addsub_2x64f", func_type_vbinop (VecType (F DoubleType, 2))) ;

      ("vushuffle_2x64f", func_type_vushuffle) ;

      ("vshuffle_2x64f" , func_type_vbinop_with_vparam (VecType (F DoubleType, 2))) ;
      ("vshuffle_4x32f" , func_type_vbinop_with_vparam (VecType (F FloatType, 4))) ;
      ("vshuffle_8x32f" , func_type_vbinop_with_vparam (VecType (F FloatType, 8))) ;

      ("vunpacklo_4x32f", func_type_vbinop (VecType (F FloatType, 4))) ;
      ("vunpacklo_8x32f", func_type_vbinop (VecType (F FloatType, 8))) ;
      ("vunpacklo_4x64f", func_type_vbinop (VecType (F DoubleType, 4))) ;

      ("vunpackhi_4x32f", func_type_vbinop (VecType (F FloatType, 4))) ;
      ("vunpackhi_4x64f", func_type_vbinop (VecType (F DoubleType, 4))) ;
      ("vunpackhi_8x32f", func_type_vbinop (VecType (F FloatType, 8))) ;

      ("cmpge_2x64f", a_func_type [VecType (F DoubleType, 2); VecType (F DoubleType, 2)]
                                (VecType (F DoubleType, 2)));

      ("testc_4x32i", a_func_type [VecType (I Int32Type, 4); VecType (I Int32Type, 4)]
                                (A (I Int32Type)));
      ("testnzc_4x32i", a_func_type [VecType (I Int32Type, 4); VecType (I Int32Type, 4)]
                                    (A (I Int32Type)));

      ("vpermf128_4x64f", func_type_vbinop_with_vparam (VecType (F DoubleType, 4))) ;
      ("vpermf128_8x32f", func_type_vbinop_with_vparam (VecType (F FloatType, 8))) ;

      ("vload_2l_4x32f", a_func_type [VecType (F FloatType, 4); PtrType (VecType (F FloatType, 2) , None)] (VecType (F FloatType, 4))); (* TODO: problem with matching Ptr alignment *)
      ("vload_2h_4x32f", a_func_type [VecType (F FloatType, 4); PtrType (VecType (F FloatType, 2) , None)] (VecType (F FloatType, 4))); (* TODO: problem with matching Ptr alignment *)

      ("vloadu_4x32f", a_func_type [PtrType (A (F FloatType), None)] (VecType (F FloatType, 4);))  (* TODO: problem with matching Ptr alignment *)

    ]

let build_var_map l =
  ignore (List.map ~f:(fun (_,x) ->
                     match x with
                     | VecType (_, n) ->
                        if is_power_of_2 n then ()
                        else
                          raise (TypeError ("Size of vector must be power of 2. Got: " ^ (string_of_int n)))
                     | _ -> ()
                   ) l) ;
  match String.Map.Tree.of_alist l with
  | `Duplicate_key k -> raise (TypeError ("duplicate variable '" ^ k ^ "' in 'let'" ))
  | `Ok m -> m

(* Check that all variabels defined in 'let' appear in at least on
   declaraion (decl, data, loop. Prints a warning if some are never
   declared.

    Returns: a set of defined but undeclared variables
 *)
let check_never_decl vmap used =
  let open String.Set.Tree in
  let all = String.Map.Tree.keys vmap |> of_list in
  let unused = diff all used in
  (if not (is_empty unused) then
     msg "Warning: following variables definded in 'let' but never declared: %s\n" (String.concat ~sep:" " (to_list unused)))
  ; unused

let rec check_vars_in_rvalue s (x:rvalue) =
  match x.rnode with
  | FunCall (_,rl) -> ignore (List.map ~f:(check_vars_in_rvalue s) rl)
  | VarRValue v -> var_in_scope s v
  | NthRvalue (r1,r2) ->   (check_vars_in_rvalue s r1) ;
                           (check_vars_in_rvalue s r2)
  | VdupRvalue (a,_) -> (check_vars_in_rvalue s a) ; ()
  | RCast (_,r) -> check_vars_in_rvalue s r
  | RDeref r -> check_vars_in_rvalue s r
  | FConstArr _  | IConstArr _ | FConstVec _  | IConstVec _ | FConst _  | IConst _ | VHex _ -> ()
and check_vars_in_lvalue s (x:lvalue) =
  match x.lnode with
  | VarLValue v -> var_in_scope s v
  | NthLvalue (l, r) -> (check_vars_in_lvalue s l) ;
                        (check_vars_in_rvalue s r)
  | LCast (_, v) -> check_vars_in_lvalue s v
  | LDeref v -> check_vars_in_rvalue s v
and var_in_scope s v =
  if not (String.Set.Tree.mem s v) then
    raise (TypeError ("Variable '" ^ v ^ "' is not in scope" ))
  else ()

let var_type vmap v =
  match (String.Map.Tree.find vmap v) with
  | None -> raise (TypeError ("Unknown variable '" ^ v ^ "'" ))
  | Some t -> t

let func_type n a =
  let open Format in
  msg "Resolving function: @[<h>%s(%a)@]@\n" n
          (pp_print_list ~pp_sep:(fun x _ -> pp_print_text x ", ") pr_itype) a
  ;
    match (String.Map.Tree.find builtins_map n) with
    | None -> raise (TypeError ("Unknown function '" ^ n ^ "'" ))
    | Some bf -> let res = bf n a in
                 msg "\t'%s' return type: %a\n" n pr_itype res
                 ; res

(* inclusive *)
let in_range64 f t x =
  Int64.compare x (Int64.of_string f) >= 0 &&
    Int64.compare x (Int64.of_string t) <= 0

let in_int8_range = in_range64 "-128" "127"
let in_int16_range = in_range64 "-32768" "32767"
let in_int32_range = in_range64 "-2147483648" "2147483647"

(* inclusive *)
let in_rangeU64 f t x =
  Uint64.compare x (Uint64.of_string f) >= 0 &&
    Uint64.compare x (Uint64.of_string t) <= 0

let in_uint8_range = in_rangeU64 "0" "255"
let in_uint16_range = in_rangeU64 "0" "65535"
let in_uint32_range = in_rangeU64 "0" "4294967295"

let uint16_cast : Ast.Int_or_uint_64.t -> int option = function
  | U64 x -> if in_uint16_range x then Some (to_int x) else None
  | I64 x -> if in_range64 "0" "65535" x then Int64.to_int x else None

let rec lvalue_type vmap (x:lvalue) =
  match x.lnode with
  | VarLValue v -> var_type vmap v
  | LCast (t,lv) ->
     let lt = lvalue_type vmap lv in
     if check_cast lt t then t
     else raise (TypeError (Format.asprintf "Illegal lvalue cast from %a to %a."
                                            pr_itype lt
                                            pr_itype t ));
  | LDeref v ->
     (match rvalue_type vmap v with
      | PtrType (t,_) -> t
      | _ as vt ->
         raise (TypeError (Format.asprintf "Dereferencing non-pointer type %a" pr_itype vt)))
  | NthLvalue (v, i) ->
     let it = rvalue_type vmap i in
     if not (is_integer it) then
       raise (TypeError (Format.asprintf "Invalid index type %a in NTH" pr_itype it))
     else
       let vt = lvalue_type vmap v in
       match vt with
       | ArrType (t,_) | PtrType (t,_) -> t
       | _ -> raise (TypeError (Format.asprintf "Invalid type %a in NTH" pr_itype vt))
and rvalue_type vmap rv =
  (let fconst_type (f:fconst) =
    match f.node with
    | FPLiteral (t,_) -> t
    | FloatEPS -> FloatType
    | DoubleEPS -> DoubleType  in
  let iconst_type (i:iconst) =
    match i.node with
    | ILiteral (t,_) -> t  in
  match rv.rnode with
  | VarRValue v -> var_type vmap v
  | FunCall (n,a) ->
     let al = (List.map ~f:(rvalue_type vmap) a) in
     let ft =
       (try
         func_type n al
       with
       | TypeError msg ->
          let open Format in
          eprintf "%a  @[<h>Error resolving rvalue function: @[<h>%s(%a)@]@]\n"
                  pr_err_loc rv.rloc
                  n (type_list_fmt ", ") al
         ; raise (TypeError msg)
       ) in
     ft
  | FConst fc -> A (F (fconst_type fc))
  | IConst ic -> A (I (iconst_type ic))
  | FConstArr (at, fl) ->
     let flt = List.map ~f:fconst_type fl in
     if List.for_all flt (eq_float_type at) then
       ArrType (A (F at) , List.length fl)
     else
       raise (TypeError (Format.asprintf "%a Mismatch between float array type and its value types\n" pr_err_loc rv.rloc))
  | IConstArr (at, il) ->
     let ilt = List.map ~f:iconst_type il in
     if List.for_all ilt (eq_int_type at) then
       ArrType (A (I at) , List.length il)
     else
       raise (TypeError (Format.asprintf "%a Mismatch between int array type and its value types\n" pr_err_loc rv.rloc))
  | FConstVec (at, fl) ->
     let flt = List.map ~f:fconst_type fl in
     if List.for_all flt (eq_float_type at) then
       VecType (F at , List.length fl)
     else
       raise (TypeError (Format.asprintf "%a Mismatch between float vector type and its value types\n" pr_err_loc rv.rloc))
  | IConstVec (at, il) ->
     let ilt = List.map ~f:iconst_type il in
     if List.for_all ilt (eq_int_type at) then
       VecType (I at , List.length il)
     else
       raise (TypeError (Format.asprintf "%a Mismatch between int vector type and its value types\n" pr_err_loc rv.rloc))
  | VHex sl ->
     let tHARDCODED = Int32Type in (* TODO: ask Franz to print type *)
     let iconst_of_hex s : iconst =
       let v =
         if String.is_empty s then
           raise (TypeError (Format.asprintf "Empty hex string in 'vhex'"))
         else
           try
             if String.prefix s 1 = "-" then
               Int_or_uint_64.I64 (Int64.of_string s)
             else
               Int_or_uint_64.U64 (Uint64.of_string s)
           with
           | Failure _ -> raise (TypeError (Format.asprintf "Invalid hex string \"%s\" in 'vhex'" s))
       in {node = ILiteral (tHARDCODED, v); loc = rv.rloc } (* TODO: loc for ech number *)
     in
     rvalue_type vmap
                 { rnode = IConstArr (tHARDCODED, List.map ~f:iconst_of_hex sl);
                   rloc = rv.rloc }
  | RCast (t,rv) ->
     let rt = rvalue_type vmap rv in
     if check_cast rt t then t
     else raise (TypeError (Format.asprintf "Illegal rvalue cast from %a to %a."
                                            pr_itype rt
                                            pr_itype t ));
  | RDeref v -> (match rvalue_type vmap v with
                 | PtrType (t,_) -> t
                 | t -> raise (TypeError (Format.asprintf "Dereferencing non-pointer type %a" pr_itype t)))
  | NthRvalue (v, i) ->
     let it = rvalue_type vmap i in
     if not (is_integer it) then
       raise (TypeError (Format.asprintf "Invalid index type %a in NTH" pr_itype it))
     else
       (match rvalue_type vmap v with
        | ArrType (t,_) | PtrType (t,_) -> t
        | t -> raise (TypeError (Format.asprintf "Invalid value type %a in NTH" pr_itype t)))
  | VdupRvalue (v, i) ->
     match rvalue_type vmap v with
     | A vt -> (match uint16_cast i with
                | Some i -> (* I arbitrary decided that max vector length should fit in 16 bit. *)
                   if is_power_of_2 i then VecType (vt, i)
                   else raise (TypeError ("Size in VDUP must be power of 2. Got: " ^ (string_of_int i)))


                | None -> raise (TypeError "Could invalid size in VDUP"))
     | t -> raise (TypeError (Format.asprintf "Invalid value type %a in VDUP" pr_itype t)))

(*
   Peforms various type and strcutural correctness checks:

   1. Eeach variable wich is used is referenced only once in an
   enclosing lexical scoping statemets, which are: DECL, DATA, LOOP,
   FUNC.

   2. All variabels defined in 'let' appear in at least on
   declaraion (decl, data, loop. Prints a warning if some are never
   declared.

   2. Vairable used in expressions are in scope.

   3. Loop indices are proper non-empty range

   4. Type in assignment are convertable

   5. Compatibilitity of constants in int and float vector intializers

   6. 'nth' index type is int

   7. Permitted and non-permitted casts

   8. Matching function return type to rvalue type in creturn

   9. Checking vdup size to be positive power of 2
 *)
let typecheck vmap prog =
  let open String.Set.Tree in
  let add_var s v =
    if mem s v then raise (TypeError ("duplicate declaration of '" ^ v ^ "'" ))
    else add s v
  in
  let add_vars s vl = List.fold ~init:s ~f:add_var vl in
  let rec typecheck (fstack:(string * Ast.IType.t) list) u x =
    match x.node with
    | Vstore_2l_4x32f (l,r) | Vstore_2h_4x32f (l,r) ->
       (match rvalue_type vmap l, rvalue_type vmap r with
        | PtrType(VecType(F FloatType,2),_), VecType(F FloatType,4) ->
           check_vars_in_rvalue u l;
           check_vars_in_rvalue u r;
           u
        | lt, rt ->
           raise (TypeError (Format.asprintf "Incompatible types in Vstore_2l_4x32f %a=[%a]."
                                             pr_itype lt
                                             pr_itype rt
       )))
    | Vstoreu_4x32f (l,r) ->
       (match rvalue_type vmap l, rvalue_type vmap r with
        | PtrType(A (F FloatType), _), VecType(F FloatType,4) ->
           check_vars_in_rvalue u l;
           check_vars_in_rvalue u r;
           u
        | lt, rt ->
           raise (TypeError (Format.asprintf "Incompatible types in Vstore_2l_4x32f %a=[%a]."
                                             pr_itype lt
                                             pr_itype rt
       )))
    | Function (fn,fr,params,body) ->
       typecheck ((fn,fr)::fstack) (add_vars u params) body
    | Decl (params,body) ->
       typecheck fstack (add_vars u params) body
    | Chain lbody ->
       List.fold ~f:(typecheck fstack) ~init:u lbody
    | Data (v,rl,body) ->
       ignore (List.map ~f:(check_vars_in_rvalue u) rl) ;
       typecheck fstack (add_var u v) body
    | Loop (v,f,t,body) ->
       if f>t then
         raise (TypeError (Printf.sprintf "Invalid loop index range: %s .. %s  " (Int_or_uint_64.to_string f) (Int_or_uint_64.to_string t) ))
       else
         typecheck fstack (add_var u v) body
    | If (r,bt,bf) ->
       check_vars_in_rvalue u r ;
       union
         (typecheck fstack u bt)
         (typecheck fstack u bf)
    | Skip -> u
    | Assign (l,r) ->
       let rt = rvalue_type vmap r in
       let lt = lvalue_type vmap l in
       if not (check_coercion rt lt) then
         raise (TypeError (Format.asprintf "Incompatible types in assignment %a=[%a]."
                                           pr_itype lt
                                           pr_itype rt
               ));
       check_vars_in_lvalue u l;
       check_vars_in_rvalue u r;
       u
    | Return r -> check_vars_in_rvalue u r ;
                  (match List.hd fstack with
                   | None -> raise (TypeError "Return outsude of function")
                   | Some (fn,ft) ->
                      let at = rvalue_type vmap r in
                      if not (check_coercion at ft) then
                        raise (TypeError (Format.asprintf "Incompatible types in return from functon '%s'. Actual: %a. Expected: %a." fn  pr_itype at pr_itype ft))
                      else u)
  in
  (* check top-level program structure *)
  ignore(
      let rec is_func x =
        match x.node with
        | Function _ -> true
        | Chain x -> is_all_func x
        | _ -> false
      and is_all_func l = List.for_all l is_func
      in
      match prog.node with
      | Chain body -> if not (is_all_func body) then
                        raise (TypeError "'program' must contain only function definitions")
      | Function _ -> ()
      | _ -> raise (TypeError "'program' must contain only function definitions")
    );
  (* build list of used variables *)
  let used = typecheck [] String.Set.Tree.empty prog in
  ignore (check_never_decl vmap used)
