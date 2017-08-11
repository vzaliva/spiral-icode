(*
1-st pass: Convert AST to IAst
*)

open Core

open Ast
open Typetools
open IType
open IIntType
open IFloatType
open IArithType
open Ints
open Utils
open Option

exception CompileError1 of (string * Loc.t option)
let raise_CompileError1 msg = raise (CompileError1 (msg,None))

let build_var_index l =
  let il = List.mapi l ~f:(fun i (_,v) -> (i,v)) in
  match Int.Map.Tree.of_alist il with
  | `Duplicate_key k -> raise_CompileError1 ("duplicate variable index '" ^ string_of_int k ^ "' in 'let'" )
  | `Ok m -> m
