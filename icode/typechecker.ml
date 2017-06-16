open Core

exception TypeError of string

open Ast
open IType
open IIntType
open IArithType

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
  | I _ -> true
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

(* If true, 'a' could be casted to 'b' at compile type without loss of precision
We choose stricter casting rules than in C. In particular:
* All pointers must be implicitly casted
*)
let rec subtype a b = true
                        (*
  if a = b then true
  else
    match a with
    | VoidType -> false
    | FloatType -> eq_itype b DoubleType
    | DoubleType -> false
    | I Int8Type  -> List.mem [ FloatType ; DoubleType ; I Int16Type ; I Int32Type ; I Int64Type ] b eq_itype
    | I Int16Type -> List.mem [ FloatType ; DoubleType ; I Int32Type ; I Int64Type ] b eq_itype
    | I Int32Type -> List.mem [ FloatType; DoubleType ; I Int64Type ] b eq_itype
    | I Int64Type -> false
    | I UInt8Type  -> List.mem [ I UInt16Type ; I UInt32Type ; I UInt64Type ] b eq_itype
    | I UInt16Type -> List.mem [ I UInt32Type ; I UInt64Type ] b eq_itype
    | I UInt32Type -> List.mem [ I UInt64Type ] b eq_itype
    | I UInt64Type -> false
    | I BoolType -> is_arith b (* could be cast to any arith type *)
    | VecType (t,l) -> (match b with
                        | VecType (t1,l1) -> l1 = l && subtype t t1
                        | _ -> false)
    | PtrType (t,a) -> false
                         *)

(* TODO: should be in Std? *)
let constlist a n =  List.map ~f:(fun _ -> a) (List.range 0 n)

let arith_binop name al =
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

let func_type_cond name a =
  let open List in
  if length a <> 3 then
    raise (TypeError ("Invalid number of arguments for 'cond'" ))
  else
    let a0 = hd_exn a in
    if not (eq_itype a0 (A (I BoolType))) then
      raise (TypeError (Format.asprintf "Could not coerce 1st argument of '%s' to boolean type. Actual types: [%a]." name pr_itype a0))
    else
      arith_binop name (tl_exn a)


let builtins_map =
  String.Map.Tree.of_alist_exn
    [
      ("cond", func_type_cond);
      ("max", arith_binop) ;
      ("add", arith_binop) ;
      ("sub", arith_binop) ;
      ("mul", arith_binop) ;
      ("div", arith_binop) ;
(*
      ("neg", arith_op 1 signed_arith_types) ;
      ("abs", arith_op 1 arith_types) ;
      ("geq", arith_op_with_rettype (I BoolType) 2 arith_types) (* TODO: extend to non-arith *) ; *)
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
  | VParam _ | FConstVec _  | IConstVec _ | FConst _  | IConst _  -> ()
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

let rec lvalue_type vmap = function
  | VarLValue v -> var_type vmap v
  | LCast (t,_) -> t
  | LDeref v ->
     (match lvalue_type vmap v with
      | PtrType (t,_) -> t
      | _ as vt ->
         raise (TypeError (Format.asprintf "Dereferencing non-pointer type %a" pr_itype vt)))
  | NthLvalue (v, i) ->
     let vt = lvalue_type vmap v in
     match vt with
     | VecType (t,_) | PtrType (t,_) -> t
     | _ -> raise (TypeError (Format.asprintf "Invalid type %a in NTH" pr_itype vt))

let func_type n a =
  let open List in
  Printf.fprintf stderr "*** Resolving function %s %s\n" n (Sexp.to_string
                                                              (sexp_of_list IType.sexp_of_t a));

  match (String.Map.Tree.find builtins_map n) with
  | None -> raise (TypeError ("Unknown function '" ^ n ^ "'" ))
  | Some bf -> bf n a

(* There is ambiguity. We return list of potential types *)
let rec rvalue_type vmap lv =
  let fconst_type = function
    (* Per C99 6.4.4.2.4 "An unsuffixed floating constant has type double". In i-code we deatult it to default machine size *)
    | FPLiteral _ -> Config.realType ()
    | FloatEPS -> A FloatType
    | DoubleEPS -> A DoubleType in
  (* TODO: Per c99 spec 6.4.4.1 "The type of an integer constant is the first of the corresponding list in which its value can be represented."*)
  let iconst_type _ = Config.intType () in
  let vparam_type = function
    | VParamList l -> VecType (Config.uIntType (), List.length l)
    | VParamValue _ -> Config.uIntType () (* bit mask *)
  in
  match lv with
  | VarRValue v -> var_type vmap v
  | FunCall (n,a) ->
     let ft = func_type n (List.map ~f:(rvalue_type vmap) a) in
     Format.fprintf Format.err_formatter "*** %s type is %a\n" n pr_itype ft;
     ft
  | FConst fc -> fconst_type fc
  | IConst ic -> iconst_type ic
  | FConstVec fl ->
     let flt = List.map ~f:fconst_type fl in
     (* TODO:
          1. Check that all types in flt are compatible
          2. Derive common type
     *)
     VecType (Config.realType (), List.length fl)
  | IConstVec il ->
     let ilt = List.map ~f:iconst_type il in
     (* TODO:
           1. Check that all types in flt are compatible
           2. Derive common type
      *)
     VecType (Config.intType (), List.length il)
  | RCast (t,_) ->  t
  | VParam v -> vparam_type v
  | RDeref v -> (match rvalue_type vmap v with
                | PtrType (t,_) -> t
                | t -> raise (TypeError (Format.asprintf "Dereferencing non-pointer type %a" pr_itype t)))
  | NthRvalue (v, i) -> (match rvalue_type vmap v with
                         | VecType (t,_) | PtrType (t,_) -> t
                         | t -> raise (TypeError (Format.asprintf "Invalid type %a in NTH" pr_itype t)))


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

  TODO:
  * 'nth' index rvalue type is int
  * Unifrmity of 'data' initializer value types
  * Uniformity of data in int and float vector intializers
  * Matching argument types in functoin calls
  * Matching function return type to rvalue type in creturn
  * Presence of return (may require some branch analysis)
  * Permitted and non-permitted casts
  *)
let typecheck vmap prog =
  let open String.Set.Tree in
  let add_var s v =
    if mem s v then raise (TypeError ("duplicate declaration of '" ^ v ^ "'" ))
    else add s v
  in
  let add_vars s vl = List.fold ~init:s ~f:add_var vl in
  let rec typecheck u = function
    | Function (_,_,params,body) ->
       typecheck (add_vars u params) body
    | Decl (params,body) ->
       typecheck (add_vars u params) body
    | Chain lbody ->
       List.fold ~f:typecheck ~init:u lbody
    | Data (v,rl,body) ->
       ignore (List.map ~f:(check_vars_in_rvalue u) rl) ;
       typecheck (add_var u v) body
    | Loop (v,f,t,body) ->
       if f>t then
         raise (TypeError (Printf.sprintf "Invalid loop index range: %d .. %d  " f t ))
       else
         typecheck (add_var u v) body
    | If (r,bt,bf) ->
       check_vars_in_rvalue u r ;
       union
         (typecheck u bt)
         (typecheck u bf)
    | Skip -> u
    | Assign (l,r) ->
       let rt = rvalue_type vmap r in
       let lt = lvalue_type vmap l in
       if not (subtype rt lt) then
         raise (TypeError (Format.asprintf "Incompatible types in assignment %a=[%a]."
                                           pr_itype lt
                                           pr_itype rt
               ));
       check_vars_in_lvalue u l;
       check_vars_in_rvalue u r;
       u
    | Return r -> check_vars_in_rvalue u r ; u
  in
  let used = typecheck String.Set.Tree.empty prog in
  ignore (check_never_decl vmap used)


