open Core

open Ast

exception Error of string

let build_var_map l =
  match String.Map.Tree.of_alist l with
  | `Duplicate_key k -> raise (Error ("duplicate variable '" ^ k ^ "' in 'let'" ))
  | `Ok m -> m

(* Makes sure each variable wich is used is referenced only once in an
   enclosing lexical scoping statemets, which are: DECL, DATA, LOOP,
   FUNC.

   Returns: a set of all declared variables.
 *)
let check_decls =
  let open String.Set.Tree in
  let add_var s v =
    if mem s v then raise (Error ("duplicate declaration of '" ^ v ^ "'" ))
    else add s v
  in
  let add_vars s vl = List.fold ~init:s ~f:add_var vl in
  let rec decl_only_once u = function
    | Function (__,_,params,body) ->
       decl_only_once (add_vars u params) body
    | Decl (params,body) ->
       decl_only_once (add_vars u params) body
    | Chain lbody ->
       List.fold ~f:decl_only_once ~init:u lbody
    | Data (v,_,body) ->
       decl_only_once (add_var u v) body
    | Loop (v,_,_,body) ->
       decl_only_once (add_var u v) body
    | If (_,bt,bf) ->
       union
         (decl_only_once u bt)
         (decl_only_once u bf)
    | Skip | Assign _ | Return _ -> u
  in
  decl_only_once String.Set.Tree.empty

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

let typecheck vmap stmt =
  let used = check_decls stmt in
  let _ = check_never_decl vmap used in
  ()


    (*
TODO: Checks:
* Each variable wich is used is referenced in an enclosing DECL
* Each variable mentioned in at most one DECL
* Int types for loop indices
* Unofrmily of decl, TVect, vparam array types
* Matcing types in ASSIGN
* Matching types in functoin calls
* Matching function return type to rvalue type in creturn
*)

