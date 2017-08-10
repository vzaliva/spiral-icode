Require Icode.IAst.

Require ExtrOcamlBasic.
Require ExtrOcamlString.
Require ExtrOcamlIntConv.

Extraction Language Ocaml.
Extraction Blacklist String List.

(* OCaml pervasive types ---------------------------------------------------- *)
(* Extract Inlined Constant Ollvm_ast.int => "int". *)
Extract Inlined Constant IAst.float => "float".

Set Extraction AccessOpaque.

(* NOTE: assumes that this file is compiled from /src *)
Cd "ml/extracted".

Extraction Library ExtrOcamlIntConv.

Recursive Extraction Library IAst.
