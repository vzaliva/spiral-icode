open Core

open Ast
open Builtins

exception Error of string

open IType

(* Mapping of generic numeric types to actual machine types. It is hardcoded now, but will be managed via config file or command line options later *)
let specializeRealType () = DoubleType
let specializeIntType () = Int64Type
let specializeUIntType () = UInt64Type

let specialize_machine_types = function
  | IntType -> specializeIntType ()
  | UIntType -> specializeUIntType ()
  | RealType -> specializeRealType ()
  | _ as x -> x

(* If true, 'a' could be casted to 'b' at compile type
We choose stricter casting rules than in C. In particular:
* Bool could not be cast to anything
* Ints could not be cast to floats
* All pointers must be implicitly casted
*)
let rec subtype a b =
  let a = specialize_machine_types a in
  let b = specialize_machine_types b in
  if a = b then true
  else
    match a with
    | VoidType -> false
    | RealType -> false (* should never occur *)
    | FloatType -> eq_itype b DoubleType
    | DoubleType -> false
    | IntType -> false (* should never occur *)
    | Int8Type  -> List.mem [ Int16Type ; Int32Type ; Int64Type ] b eq_itype
    | Int16Type -> List.mem [ Int32Type ; Int64Type ] b eq_itype
    | Int32Type -> List.mem [ Int64Type ] b eq_itype
    | Int64Type -> false
    | UIntType -> false (* should never occur *)
    | UInt8Type  -> List.mem [ UInt16Type ; UInt32Type ; UInt64Type ] b eq_itype
    | UInt16Type -> List.mem [ UInt32Type ; UInt64Type ] b eq_itype
    | UInt32Type -> List.mem [ UInt64Type ] b eq_itype
    | UInt64Type -> false
    | BoolType -> false
    | OtherType _ -> false
    | UnknownType -> false
    | VecType (t,l) -> (match b with
                        | VecType (t1,l1) -> l1 = l && subtype t t1
                        | _ -> false)
    | PtrType (t,a) -> false

let subtype_l (al: IType.t list) (bl: IType.t list) : bool =
  let open List in
  if length al <> length bl then false
  else
    fold ~init:true ~f:(fun p (a,b) -> p && (subtype a b)) (zip_exn al bl)

let subtype_pick (asl:ITypeSet.t list) (bl: IType.t list) : bool =
  let open List in
  if length asl <> length bl then false
  else
    fold ~init:true ~f:(fun p (sa,b) -> p &&
                                          Set.exists ~f:(fun y -> subtype y b) sa
                       ) (zip_exn asl bl)

let build_var_map l =
  match String.Map.Tree.of_alist l with
  | `Duplicate_key k -> raise (Error ("duplicate variable '" ^ k ^ "' in 'let'" ))
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
    raise (Error ("Variable '" ^ v ^ "' is not in scope" ))
  else ()

let var_type vmap v =
  match (String.Map.Tree.find vmap v) with
  | None -> raise (Error ("Unknown variable '" ^ v ^ "'" ))
  | Some t -> t

let rec lvalue_type vmap = function
  | VarLValue v -> var_type vmap v
  | LCast (t,_) -> t
  | LDeref v ->
     (match lvalue_type vmap v with
      | PtrType (t,_) -> t
      | _ as vt ->
         raise (Error (Format.asprintf "Dereferencing non-pointer type %a" pr_itype vt)))
  | NthLvalue (v, i) ->
     let vt = lvalue_type vmap v in
     match vt with
     | VecType (t,_) | PtrType (t,_) -> t
     | _ -> raise (Error (Format.asprintf "Invalid type %a in NTH" pr_itype vt))

let func_type n a =
  let open List in
  let al = map ~f:ITypeSet.to_list a in
  Printf.fprintf stderr "*** Resolving function %s %s\n" n (Sexp.to_string
                                                                 (sexp_of_list
                                                                    (sexp_of_list IType.sexp_of_t) al));

  (* Some built-in functions handling is hardcoded here *)
  if n = "cond" then
    if length a <> 3 then
      raise (Error ("Invalid number of arguments for 'cond'" ))
    else
      let a0 = hd_exn a in
      if not (ITypeSet.mem a0 BoolType) then
        raise (Error (Format.asprintf "Could not coerce 1st argument of 'cond' to boolean type. Actual types: [%a]." type_list_fmt (ITypeSet.to_list a0)))
      else
        ITypeSet.union (nth_exn a 1) (nth_exn a 2)
  else
    (* Others handed via general mechanism *)
    let bm = builtins_map in
    match (String.Map.Tree.find bm n) with
    | None -> raise (Error ("Unknown function '" ^ n ^ "'" ))
    | Some sl ->
       filter ~f:(fun (_,ca) -> subtype_pick a ca) sl
       |> map ~f:fst
       |> ITypeSet.of_list

(* There is ambiguity. We return list of potential types *)
let rec rvalue_type vmap lv =
  let fconst_type = function
    | FPLiteral _ -> ITypeSet.of_list [DoubleType; FloatType]
    | FloatEPS -> ITypeSet.singleton FloatType
    | DoubleEPS -> ITypeSet.singleton DoubleType in
  let iconst_type _ = ITypeSet.of_list [IntType ; UIntType] in
  let vparam_type = function
    | VParamList l -> ITypeSet.singleton (VecType (UIntType, List.length l))
    | VParamValue _ -> ITypeSet.singleton UIntType (* bit mask *)
  in
  match lv with
  | VarRValue v -> ITypeSet.singleton (var_type vmap v)
  | FunCall (n,a) ->
     let ft = func_type n (List.map ~f:(rvalue_type vmap) a) in
     Printf.fprintf stderr "*** %s type is %s\n" n (Sexp.to_string (sexp_of_list IType.sexp_of_t (ITypeSet.to_list ft)));
     ft
  | FConst fc -> fconst_type fc
  | IConst ic -> iconst_type ic
  | FConstVec fl ->
     let flt = List.fold ~f:ITypeSet.union ~init:(ITypeSet.empty)
                         (List.map ~f:fconst_type fl) in
     (* TODO: Check that all types in flt are convertible to float *)
     let fll = List.length fl in
     ITypeSet.map ~f:(fun t -> VecType (t, fll)) flt
  | IConstVec il ->
     let ilt = List.fold ~f:ITypeSet.union ~init:(ITypeSet.empty)
                         (List.map ~f:iconst_type il) in
     (* TODO: Check that all types in flt are convertible to int *)
     let ill = List.length il in
     ITypeSet.map ~f:(fun t -> VecType (t, ill)) ilt
  | RCast (t,_) -> ITypeSet.singleton t
  | VParam v -> vparam_type v
  | RDeref v -> ITypeSet.map
                  ~f:(fun ti ->
                    match ti with
                    | PtrType (t,_) -> t
                    | _ -> raise (Error (Format.asprintf "Dereferencing non-pointer type %a" pr_itype ti)))
                  (rvalue_type vmap v)

  | NthRvalue (v, i) -> ITypeSet.map
                          ~f:(fun ti ->
                            match ti with
                            | VecType (t,_) | PtrType (t,_) -> t
                            | _ -> raise (Error (Format.asprintf "Invalid type %a in NTH" pr_itype ti)))
                          (rvalue_type vmap v)

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
    if mem s v then raise (Error ("duplicate declaration of '" ^ v ^ "'" ))
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
         raise (Error (Printf.sprintf "Invalid loop index range: %d .. %d  " f t ))
       else
         typecheck (add_var u v) body
    | If (r,bt,bf) ->
       check_vars_in_rvalue u r ;
       union
         (typecheck u bt)
         (typecheck u bf)
    | Skip -> u
    | Assign (l,r) ->
       let rts = rvalue_type vmap r in
       let lt = lvalue_type vmap l in
       if not (ITypeSet.exists ~f:(fun x -> subtype x lt) rts) then
         raise (Error (Format.asprintf "Incompatible types in assignment %a=[%a]."
                                       pr_itype lt
                                       type_list_fmt (ITypeSet.to_list rts)
               ));
       check_vars_in_lvalue u l;
       check_vars_in_rvalue u r;
       u
    | Return r -> check_vars_in_rvalue u r ; u
  in
  let used = typecheck String.Set.Tree.empty prog in
  ignore (check_never_decl vmap used)


