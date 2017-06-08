open Core

open Ast

exception Error of string

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
  | None -> raise (Error ("Unknown variable '" ^ v ^ "'e" ))
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

(* TODO: there is ambiguity. Maybe return list? *)
let rec rvalue_type vmap lv =
  let fconst_type = function
    | FPLiteral _ -> DoubleType (* TODO: could be also float! *)
    | FloatEPS -> FloatType
    | DoubleEPS -> DoubleType in
  let iconst_type _ = IntType (* Could be also UInt *) in
  let vparam_type = function
    | VParamList l -> VecType (IntType, List.length l) (* TODO: maybe UNInt and maybe 32/64 *)
    | VParamValue _ -> IntType
  in
  match lv with
  | FunCall (n,a) -> UnknownType (* TODO *)
  | VarRValue v -> var_type vmap v
  | FConst fc -> fconst_type fc
  | IConst ic -> iconst_type ic
  | FConstVec fl -> VecType (fconst_type (List.hd_exn fl), List.length fl) (* always non-empty *)
  | IConstVec il -> VecType (iconst_type (List.hd_exn il), List.length il) (* always non-empty *)
  | RCast (t,_) -> t
  | VParam v -> vparam_type v
  | RDeref v ->
     (match rvalue_type vmap v with
      | PtrType (t,_) -> t
      | _ as vt ->
         raise (Error (Format.asprintf "Dereferencing non-pointer type %a" pr_itype vt)))
  | NthRvalue (v, i) ->
     let vt = rvalue_type vmap v in
     match vt with
     | VecType (t,_) | PtrType (t,_) -> t
     | _ -> raise (Error (Format.asprintf "Invalid type %a in NTH" pr_itype vt))

(*
   Peforms various type and strcutural correctness checks:

   1. Eeach variable wich is used is referenced only once in an
   enclosing lexical scoping statemets, which are: DECL, DATA, LOOP,
   FUNC.

   2. Aall variabels defined in 'let' appear in at least on
   declaraion (decl, data, loop. Prints a warning if some are never
   declared.

   2. Vairable used in expressions are in scope.

   3. Loop indices are proper non-empty range (TODO: allow empy?)

  TODO:
  * 'nth' index rvalue type is int
  * Unifrmity of 'data' initializer value types
  * Uniformity of data in int and float vector intializers
  * Matcing types in ASSIGN
  * Matching argument types in functoin calls
  * Matching function return type to rvalue type in creturn

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
       let lt = lvalue_type vmap l in
       let rt = rvalue_type vmap r in
       if lt <> rt then
         raise (Error (Format.asprintf "Incompatible types in assignment %a %a" pr_itype lt pr_itype rt));
       check_vars_in_lvalue u l;
       check_vars_in_rvalue u r;
       u
    | Return r -> check_vars_in_rvalue u r ; u
  in
  let used = typecheck String.Set.Tree.empty prog in
  ignore (check_never_decl vmap used)


