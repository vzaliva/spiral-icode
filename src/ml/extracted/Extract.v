Require Icode.IAst.

Extraction Language Ocaml.

(* OCaml pervasive types ---------------------------------------------------- *)
(* Extract Inlined Constant Ollvm_ast.int => "int". *)
Extract Inlined Constant IAst.float => "float".

Set Extraction AccessOpaque.

(* NOTE: assumes that this file is compiled from /src *)
Cd "ml/extracted".

Extraction Library IAst.
