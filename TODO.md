
Frontent TODO:

* More generally maybe introduce notion of immediate values, also maybe needed for DATA
* Introduce actual VPARAM type
* Sort out VPARAM subtypes.
* Clarify vec/array casts in `check_coercion`
* Define in Coq and Extract
* Reduce/reduce conflicts in grammar using `parser.conflicts`
* Matching PTR w/o regard for alighment
* Try to add locations in TypeError exceptions thrown
* FConst range check (as we did for IConst)
* Consider https://github.com/andrenth/ocaml-stdint instead of `uint`
* Boolean literals in parser?
* VHEX

Backend TODO:

* Check vpara/vshuffle mask types to be immediate values during code generation (not checked at typecheck step)

In Progress:
* Switching to https://github.com/andrenth/ocaml-stdint instead of `uint`

