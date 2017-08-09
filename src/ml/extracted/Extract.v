Require Icode.Ast.

Extraction Language Ocaml.

(* OCaml pervasive types ---------------------------------------------------- *)
(* Extract Inlined Constant Ollvm_ast.int => "int". *)
Extract Inlined Constant Ast.float => "float".

Set Extraction AccessOpaque.

(* NOTE: assumes that this file is compiled from /src *)
Cd "ml/extracted".

Extraction Library Ast.
