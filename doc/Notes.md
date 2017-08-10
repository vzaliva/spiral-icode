
# Implementation Notes #

## Representation ##

* ml/Ast - "native AST" produced by parser closely reflecting iCode textual representation. Could contain errors
* coq/IAst (ml/extracted/IAst.v) - 2nd level AST, which has 2 puposes:
  1. Being extracted from Coq has both Coq and OCaml representation
  2. Have tighter types, ensureing correctness.  Difference from ml/Ast:
     1. Homogenous array and vector types
     2. No location information
     3. Variable names are replaced by integer indicies
     4. Function type in Program is a map instead of alist
     5. No `vhex` node as it is translated to const array
     6. Different representation of float and double constants
     7. Floating-point values are represented as opaque strings (however these are guaranteed to be well-formed)
     8. (TODO) All type promotions are made implicit
     9. Builtin-functions are defined by inductive type
     10. N-nary function like `min` and `max` converted to nested binary

## Passes ##

* typecheck convers "native" AST to IAst. Also:
  1. detecting some errors. 
  2. some type inference is performed (e.g. for integer constants)
  3. All type promotion rules are applied
  3. Function types are checked
