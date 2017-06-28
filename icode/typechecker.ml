open Core

open Ast
open IType
open IIntType
open IArithType
open Int_or_uint_64
open Uint64

exception TypeError of string

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
                                A FloatType ;
                                A DoubleType ; ])
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
  | FloatType -> true
  | DoubleType -> true

let is_unsigned_arith = function
  | I t -> IIntTypeSet.mem unsigned_integer_types t
  | FloatType -> false
  | DoubleType -> false

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

let unsigned_type = function
  | Int8Type -> UInt8Type
  | Int16Type -> UInt16Type
  | Int32Type -> UInt32Type
  | Int64Type -> UInt64Type
  | _ as t -> t

let integer_promotion t =
  let i = if is_signed_integer t then Config.intIntType () else Config.uIntIntType () in
  if integer_type_rank t < integer_type_rank i then i else t

(** Usual arithmetic conversions, a.k.a. binary conversions. This function returns the type to which the two operands must be converted. Adopted from http://compcert.inria.fr/doc/html/Cop.html. Reference: C99 Section 6.3.1.8.
*)
let usual_arithmetic_conversion t1 t2 =
  match t1, t2 with
  | DoubleType, _ | _, DoubleType -> DoubleType
  | FloatType, _ | _, FloatType -> FloatType
  | I i1, I i2 ->
     let j1 = integer_promotion i1 in
     let j2 = integer_promotion i2 in
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


(* check if 'r' could be cast to 'l' even with possible loss of precision.
   Our rules are stricter than in C99 *)
let rec check_cast tfrom tto =
  match tto, tfrom with
  | VoidType , _         -> true
  | _        , VoidType  -> false
  | A _      , A _       -> true
  | A _      , PtrType _ -> false (* unlike C we do not allow cast between ints and ptr *)
  | A _      , ArrType _ -> false
  | PtrType _, A _       -> false (* unlike C we do not allow cast between ints and ptr *)
  | ArrType _, A _       -> false
  | ArrType (lt,ll), ArrType (rt,rl) -> ll = rl && check_cast rt lt
  | PtrType (lt, la), PtrType (rt, ra) -> lt=rt (* TODO: alignment? *)
  | ArrType (lt,ll), PtrType (rt, ra) -> lt=rt (* TODO: Check with Franz *)
  | PtrType (lt, la), ArrType (rt,rl) -> lt=rt (* TODO: Check with Franz *)

(* TODO: should be in Std? *)
let constlist a n =  List.map ~f:(fun _ -> a) (List.range 0 n)

let func_type_arith_binop name al =
  let open List in
  if 2 <> length al then
    raise (TypeError ("Invalid number of arguments"))
  else
    let a0 = nth_exn al 0 in
    let a1 = nth_exn al 1 in
    match a0 , a1 with
    | A ia0 , A ia1 -> A (usual_arithmetic_conversion ia0 ia1)
    | _ , _ -> raise (TypeError
                        (Format.asprintf "Incompatible arguments types %a, %a for '%s'"
                                         pr_itype a0 pr_itype a1 name))


let func_type_bool_arith_binop name al = ignore ( func_type_arith_binop name al) ; A (I BoolType)

let ptr_attr_combine a1 a2 =
  match a1, a2 with
  | None, al2 -> al2
  | al1, None -> al1
  | Some n1, Some n2 -> Some (max n1 n2)

let rec type_combine ty1 ty2 =
  match ty1, ty2 with
  | VoidType, VoidType -> Some VoidType
  | A FloatType,  A FloatType -> Some (A FloatType)
  | A DoubleType,  A DoubleType -> Some (A DoubleType)
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
  | A ia0 , A ia1 -> A (usual_arithmetic_conversion ia0 ia1)
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
    raise (TypeError ("Invalid number of arguments for 'cond'" ))
  else
    let a0 = hd_exn a in
    (* first argument should be interpretable as boolean *)
    match a0 with
    | ArrType _ | VoidType -> raise (TypeError (Format.asprintf "Could not coerce 1st argument of '%s' to boolean type. Actual types: [%a]." name pr_itype a0))
    | A _ | PtrType _ ->
       (match nth_exn a 1, nth_exn a 2 with
        | _,_ -> func_type_arith_binop name (tl_exn a)
       )

(* 6.5.3.3 Unary arithmetic operators: "The result of the unary + operator is the value of its (promoted) operand. The integer promotions are performed on the operand, and the result has the promoted type". *)
let func_type_neg _ a =
  let open List in
  if length a <> 1 then
    raise (TypeError ("Invalid number of arguments for negation" ))
  else
    let a0 = hd_exn a in
    match a0 with
    | A I it -> A (I (integer_promotion it))
    | A _ as t -> t (* floats negated to the same type. Not dealing with signedness *)
    | _ -> raise (TypeError (Format.asprintf "Could not apply negation to non-arithmetic type [%a]." pr_itype a0))

(* 'abs' is polymorphic version of C99 abs, labs, fabsf, fabs*)
let func_type_abs _ a =
  let open List in
  if length a <> 1 then
    raise (TypeError ("Invalid number of arguments for 'abs'" ))
  else
    let a0 = hd_exn a in
    match a0 with
    | A I it as t -> if is_signed_integer it then t
                     else raise (TypeError (Format.asprintf "Could not apply 'abs' to unsigned type [%a]." pr_itype a0))
    | A _ as t -> t (* floats negated to the same type. Not dealing with signedness *)
    | _ -> raise (TypeError (Format.asprintf "Could not apply 'abs' to non-arithmetic type [%a]." pr_itype a0))


let builtins_map =
  String.Map.Tree.of_alist_exn
    [
      ("cond", func_type_cond);
      ("max", func_type_arith_binop) ;
      ("add", func_type_arith_binop) ;
      ("sub", func_type_arith_binop) ;
      ("mul", func_type_arith_binop) ;
      ("div", func_type_arith_binop) ;
      ("geq", func_type_bool_arith_binop) ;
      ("neg", func_type_neg) ;
      ("abs", func_type_abs)
    ]

let build_var_map l =
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
     Printf.fprintf stderr "Warning: following variables definded in 'let' but never declared: %s\n" (String.concat ~sep:" " (to_list unused)))
  ; unused

let rec check_vars_in_rvalue s = function
  | FunCall (_,rl) -> ignore (List.map ~f:(check_vars_in_rvalue s) rl)
  | VarRValue v -> var_in_scope s v
  | NthRvalue (r1,r2) ->   (check_vars_in_rvalue s r1) ;
                           (check_vars_in_rvalue s r2)
  | RCast (_,r) -> check_vars_in_rvalue s r
  | RDeref r -> check_vars_in_rvalue s r
  | VParam _ | FConstArr _  | IConstArr _ | FConst _  | IConst _  -> ()
and check_vars_in_lvalue s = function
  | VarLValue v -> var_in_scope s v
  | NthLvalue (l, r) -> (check_vars_in_lvalue s l) ;
                        (check_vars_in_rvalue s r)
  | LCast (_, v) -> check_vars_in_lvalue s v
  | LDeref v -> check_vars_in_lvalue s v
and var_in_scope s v =
  if not (String.Set.Tree.mem s v) then
    raise (TypeError ("Variable '" ^ v ^ "' is not in scope" ))
  else ()

let var_type vmap v =
  match (String.Map.Tree.find vmap v) with
  | None -> raise (TypeError ("Unknown variable '" ^ v ^ "'" ))
  | Some t -> t

let func_type n a =
  let open List in
  Printf.fprintf stderr "*** Resolving function %s %s\n" n (Sexp.to_string
                                                              (sexp_of_list IType.sexp_of_t a));

  match (String.Map.Tree.find builtins_map n) with
  | None -> raise (TypeError ("Unknown function '" ^ n ^ "'" ))
  | Some bf -> bf n a

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

let rec lvalue_type vmap = function
  | VarLValue v -> var_type vmap v
  | LCast (t,lv) ->
     let lt = lvalue_type vmap lv in
     if check_cast lt t then t
     else raise (TypeError (Format.asprintf "Illegal cast from %a to %a."
                                         pr_itype lt
                                         pr_itype t ));
  | LDeref v ->
     (match lvalue_type vmap v with
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
and rvalue_type vmap lv =
  let fconst_type = function
    (* Per C99 6.4.4.2.4 "An unsuffixed floating constant has type double". In i-code we deatult it to default machine size *)
    | FPLiteral _ -> Config.realAType ()
    | FloatEPS -> FloatType
    | DoubleEPS -> DoubleType in
  (* Per c99 spec 6.4.4.1 "The type of an integer constant is the first of the corresponding list in which its value can be represented."*)
  let iconst_type = function
    | I64 x -> if in_int8_range x then I Int8Type
               else if in_int16_range x then I Int16Type
               else if in_int32_range x then I Int32Type
               else I Int64Type
    | U64 x -> if in_uint8_range x then I UInt8Type
               else if in_uint16_range x then I UInt16Type
               else if in_uint32_range x then I UInt32Type
               else I UInt64Type
  in
  let vparam_type = function
    | VParamList l -> ArrType (Config.uIntType (), List.length l)
    | VParamValue _ -> Config.uIntType () (* bit mask *)
  in
  match lv with
  | VarRValue v -> var_type vmap v
  | FunCall (n,a) ->
     let ft = func_type n (List.map ~f:(rvalue_type vmap) a) in
     Format.fprintf Format.err_formatter "*** %s type is %a\n" n pr_itype ft;
     ft
  | FConst fc -> A (fconst_type fc)
  | IConst ic -> A (iconst_type ic)
  | FConstArr fl ->
     let flt = List.map ~f:fconst_type fl in
     let t = A (if List.is_empty flt then Config.realAType ()
                else List.fold ~f:usual_arithmetic_conversion
                            ~init:(List.hd_exn flt) flt) in
     ArrType (t, List.length fl)
  | IConstArr il ->
     let ilt = List.map ~f:iconst_type il in
     let t = A (if List.is_empty ilt then Config.intAType () (* defaultin to signed *)
                else List.fold ~f:usual_arithmetic_conversion
                               ~init:(List.hd_exn ilt) ilt) in
     if not (is_integer t) then
       raise (TypeError (Format.asprintf "Initialize int array witn non-integer constants")) (* maybe warning? *)
     else
       ArrType (t, List.length il)
  | RCast (t,rv) ->
     let rt = rvalue_type vmap lv in
     if check_cast rt t then t
     else raise (TypeError (Format.asprintf "Illegal cast from %a to %a."
                                            pr_itype rt
                                            pr_itype t ));
  | VParam v -> vparam_type v
  | RDeref v -> (match rvalue_type vmap v with
                | PtrType (t,_) -> t
                | t -> raise (TypeError (Format.asprintf "Dereferencing non-pointer type %a" pr_itype t)))
  | NthRvalue (v, i) ->
     let it = rvalue_type vmap i in
     if not (is_integer it) then
       raise (TypeError (Format.asprintf "Invalid index type %a in NTH" pr_itype it))
     else
       match rvalue_type vmap v with
        | ArrType (t,_) | PtrType (t,_) -> t
        | t -> raise (TypeError (Format.asprintf "Invalid value type %a in NTH" pr_itype t))


(*
   Peforms various type and strcutural correctness checks:

   1. Eeach variable wich is used is referenced only once in an
   enclosing lexical scoping statemets, which are: DECL, DATA, LOOP,
   FUNC.

   2. All variabels defined in 'let' appear in at least on
   declaraion (decl, data, loop. Prints a warning if some are never
   declared.

   2. Vairable used in expressions are in scope.

   3. Loop indices are proper non-empty range (TODO: allow empy?)

   4. Type in assignment are convertable

   5. Compatibilitity of constants in int and float vector intializers

   6. 'nth' index type is int

   7. Permitted and non-permitted casts

   8. Matching function return type to rvalue type in creturn

  TODO:
  * Matching argument types in functoin calls
  * Presence of return (may require some branch analysis)
  *)
let typecheck vmap prog =
  let open String.Set.Tree in
  let add_var s v =
    if mem s v then raise (TypeError ("duplicate declaration of '" ^ v ^ "'" ))
    else add s v
  in
  let add_vars s vl = List.fold ~init:s ~f:add_var vl in
  let rec typecheck (fstack:(string * Ast.IType.t) list) u = function
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
       if not (check_cast rt lt) then
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
                      if not (check_cast at ft) then
                        raise (TypeError (Format.asprintf "Incompatible types in return from functon '%s'. Actual: %a. Expected: %a." fn  pr_itype at pr_itype ft))
                      else u)
  in
  let used = typecheck [] String.Set.Tree.empty prog in
  ignore (check_never_decl vmap used)


