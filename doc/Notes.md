
# Implementation Notes #

## Representation ##

* ml/Ast - "native AST" produced by parser closely reflecting iCode textual representation. Could contain errors
* coq/IAst (ml/extracted/IAst.v) - 2nd level AST, which has 2 puposes:
  1. Being extracted from Coq has both Coq and OCaml representation
  1. Have tighter types, ensureing correctness.  Difference from ml/Ast:
     1. Homogenous array and vector types
     1. No location information
     1. Variable names are replaced by integer indicies
     1. Variable type in Program is a map instead of alist
     1. No `vhex` node as it is translated to const array
     1. No `Decl` nodes
     1. Different representation of float and double constants
     1. Floating-point values are represented as opaque strings (however these are guaranteed to be well-formed)
     1. Builtin-functions are defined by inductive type

## Passes ##

* Pass 0: typecheck checks icode correctness
* Pass 1: convers "native" AST to IAst. Also:
  1. detecting some errors. 
  1. some type inference is performed (e.g. for integer constants)
  1. All type promotion rules are applied
  1. Function types are checked
