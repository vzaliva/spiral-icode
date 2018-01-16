
# Language Notes #

* VHEX constants can have 'l' or 'u' suffixes and be positive or negative

# Implementation Notes #

## Representation ##

* ml/Ast - "native AST" produced by parser closely reflecting iCode textual representation. Could represent invalid code
* coq/IAst (ml/extracted/IAst.v) - 2nd level AST, which has 2 purposes:
  1. Being extracted from Coq has both Coq and OCaml representation
  1. Have tighter types, ensuring correctness.  Difference from ml/Ast:
     1. Homogeneous array and vector types
     1. No location information
     1. Variable names are replaced by integer indices
     1. Variable type in Program is a map instead of alist
     1. No `vhex` node as it is translated to const array
     1. No `Decl` nodes
     1. Different representation of float and double constants
     1. Floating-point values are represented as opaque strings (however these are guaranteed to be well-formed)
     1. Built-in-functions are defined by inductive type

## Passes ##

* Pass 0: typecheck checks iCode correctness
* Pass 1: converts "native" AST to IAst. Also:
  1. detecting some errors. 
  1. some type inference is performed (e.g. for integer constants)
  1. All type promotion rules are applied
  1. Function types are checked
