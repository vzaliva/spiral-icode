
Frontent TODO:

* More generally maybe introduce notion of immediate values, also maybe needed for DATA
* Introduce actual VPARAM type
* Sort out VPARAM subtypes.
* Clarify vec/array casts in `check_coercion`
* Define in Coq and Extract
* Reduce/reduce conflicts in grammar
* Matching PTR w/o regard for alighment
* Try to add locations in TypeError exceptions thrown
* Consider https://github.com/andrenth/ocaml-stdint instead of `uint`

Backend TODO:

* Check vpara/vshuffle mask types to be immediate values during code generation (not checked at typecheck step)


In progress:
* Check iconst type and U/I64 value range match via `int_const_in_range`
