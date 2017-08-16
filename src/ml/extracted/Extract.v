Require Icode.IAst.
Require Icode.ITypes.

Require ExtrOcamlBasic.
Require ExtrOcamlString.
Require ExtrOcamlIntConv.

Extraction Language Ocaml.
Extraction Blacklist String List Nat.

(* OCaml pervasive types ---------------------------------------------------- *)
Extract Inlined Constant IAst.float => "string".
Extract Inlined Constant IAst.funcname => "string".

Set Extraction AccessOpaque.

(* NOTE: assumes that this file is compiled from /src *)
Cd "ml/extracted".

Extraction Library ExtrOcamlIntConv.
Extraction Library BinInt.
Recursive Extraction Library IAst.
Recursive Extraction Library ITypes.
