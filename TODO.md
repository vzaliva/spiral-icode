

## OCaml Frontent: ##

* VPARAM
  * Introduce actual VPARAM type similar to vector. Also could have altenative int representation (2 constructors) VParam = | Array | Int
  * Functions can have approriate subtypes
  * INT constant coercion in VPARAM.
* Clarify vec/array casts in `check_coercion`
* Reduce/reduce conflicts in grammar using `parser.conflicts`
* Boolean literals in parser?
* Check float and double constant validity
* FConst range check (as we did for IConst)

## COQ Integration: ##

* Define second-level language (AST with tighter typing) in Coq
* Extract to OCaml
* Convert OCaml AST to Coq AST

## Backend: ##

* Check vpara/vshuffle mask types to be immediate values during code generation (not checked at typecheck step)



