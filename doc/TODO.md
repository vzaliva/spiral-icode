

## OCaml Frontent: ##

* Reduce/reduce conflicts in grammar using `parser.conflicts`
* Boolean literals in parser?

## Typecheck ##

* Remove unecessary checks and functions
* Check float and double constant validity (e.g. range check as we did for IConst)

## Pass1 ###

* in `vhex` find type based on all elements (now takes 1st)

## COQ Integration: ##

* Coq-level typechecks
  * check_cast in RCast
  * check for pointer type 1st paramter of deref

## Backend: ##

* Check vpara/vshuffle mask types to be immediate values during code generation (not checked at typecheck step)



