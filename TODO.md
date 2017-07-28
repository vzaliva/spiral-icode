
Frontent TODO:

* VPARAM
  * Introduce actual VPARAM type similar to vector. Also could have altenative int representation (2 constructors) VParam = | Array | Int
  * Functions can have approriate subtypes
  * INT constant coercion in VPARAM.
* More generally maybe introduce notion of immediate values, also maybe needed for DATA
* Clarify vec/array casts in `check_coercion`
* Define in Coq and Extract
* Reduce/reduce conflicts in grammar using `parser.conflicts`
* Matching PTR w/o regard for alighment
* Try to add locations in TypeError exceptions thrown
* Boolean literals in parser?
* VHEX
* Check float and double constant validity

Backend TODO:

* Check vpara/vshuffle mask types to be immediate values during code generation (not checked at typecheck step)

In Progress:
* FConst range check (as we did for IConst)

