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
  | Cast (_,r) -> check_vars_in_rvalue s r
  | VParam _ | FConstVec _  | IConstVec _ | FConst _  | IConst _  -> ()
and check_vars_in_lvalue s = function
  | VarLValue v -> var_in_scope s v
  | NthLvalue (l, r) -> (check_vars_in_lvalue s l) ;
                        (check_vars_in_rvalue s r)
and var_in_scope s v =
  if not (String.Set.Tree.mem s v) then
    raise (Error ("Variable '" ^ v ^ "' is not in scope" ))
  else ()

(*
   Peform various type and strcutural correctness checks.

   1. Makes sure each variable wich is used is referenced only once in an
   enclosing lexical scoping statemets, which are: DECL, DATA, LOOP,
   FUNC.

   2. Check that all variabels defined in 'let' appear in at least on
   declaraion (decl, data, loop. Prints a warning if some are never
   declared.

   2. Checks that a vairable used in expressions is in scope.

   Returns: a set of all declared variables.
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
    | Loop (v,_,_,body) ->
       typecheck (add_var u v) body
    | If (r,bt,bf) ->
       check_vars_in_rvalue u r ;
       union
         (typecheck u bt)
         (typecheck u bf)
    | Skip -> u
    | Assign (l,r) ->
       check_vars_in_lvalue u l;
       check_vars_in_rvalue u r;
       u
    | Return r -> check_vars_in_rvalue u r ; u
  in
  let used = typecheck String.Set.Tree.empty prog in
  ignore (check_never_decl vmap used)



    (*
TODO: Checks:
* Int types for loop indices
* Unofrmily of decl, TVect, vparam array types
* Matcing types in ASSIGN
* Matching types in functoin calls
* Matching function return type to rvalue type in creturn
*)

