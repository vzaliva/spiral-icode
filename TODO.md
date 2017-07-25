
* Check vpara/vshuffle mask types to be immediate values during code generation (not checked at typecheck step)
* More generally maybe introduce notion of immediate values, also maybe needed for DATA
* Introduce actual VPARAM type
* Sort out VPARAM subtypes.
* See where Arrays are actually vectors and where they are just arrays
* Clarify vec/array casts in `check_coercion`


TODO:

* Define in Coq and Extract
* Check length in const array/vec type matches number of elemens.
( Check iconst type and U/I64 value match
* Matching PTR w/o regard for alighment
* Generalize function calls for statements, instead of using specific AST nodes
* Reduce/reduce conflicts in grammar
