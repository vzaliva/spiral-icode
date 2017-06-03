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

   Returns set of all declared variables.
 *)
let decl_only_once s =
  let open String.Set.Tree in
  let add_var s v =
    if mem s v then
      raise (Error ("duplicate declaration of '" ^ v ^ "'" ))
    else
      add s v
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

let typecheck vmap stmt = ()


(*
Checks:
* Each variable wich is used is referenced in an enclosing DECL
* Each variable mentioned in at most one DECL
* Int types for loop indices
* Unofrmily of decl, TVect, vparam array types
* Matcing types in ASSIGN
* Matching types in functoin calls
* Matching function return type to rvalue type in creturn
*)

(* (string*itype) list -> Map *)

(* let build_var_dict l = *)


      (*      let ast = fix_operator_types ast in
      let types = Typechecker.collect_vars ast in
      pp_print_list ~pp_sep:pp_print_newline Ast.pr_ivar std_formatter types *)


(*
let rec collect_vars = function
  | Function (_, _, args, stmt) -> args @ collect_vars stmt
  | Decl (vars, astmt) -> vars @ collect_vars astmt
  | Chain stmts -> List.concat_map stmts collect_vars
  | Data (v, _, stmt) -> v :: (collect_vars stmt)
  | Loop (v, _, _, stmt) -> v :: (collect_vars stmt)
  | If (_, then_stmt, else_stmt) -> collect_vars then_stmt @ collect_vars else_stmt
  | _ -> []

let var_enforce_int = function
  | Var (n, t) -> (match t with
                   | IntType | UnknownType -> Var (n, IntType)
                   | _ -> raise (Error "Loop variable type mismatch. Must be int"))

let var_enforce_array d = function
  | Var (n, t) -> (match t with
                   | ArrayType (at,ado) ->
                      (match ado with
                      | None -> Var (n, ArrayType (at, Some d))
                      | Some ad -> if ad = d then
                                     Var (n, ArrayType (at, Some d))
                                   else
                                     raise (Error "Array size mismatch"))
                   | UnknownType -> Var (n, ArrayType (UnknownType, Some d))
                   | _ -> raise (Error "Loop variable type mismatch. Must be array"))

let rec fix_operator_types = function
  | Function (n, t, args, stmt) -> Function (n, t, args, fix_operator_types stmt)
  | Decl (vars, stmt) -> Decl (vars, fix_operator_types stmt)
  | Chain stmts ->  Chain (List.map stmts fix_operator_types)
  | Data (v, values, stmt) ->
     Data (
         var_enforce_array (List.length values) v,
         values,
         fix_operator_types stmt)
  | Loop (v, f, t, stmt) -> Loop (var_enforce_int v, f, t, fix_operator_types stmt)
  | If (rv, then_stmt, else_stmt) -> If (rv, fix_operator_types then_stmt, fix_operator_types else_stmt)
  | _ as x -> x
 *)
