

## OCaml Frontent: ##

* Reduce/reduce conflicts in grammar using `parser.conflicts`
* Boolean literals in parser?

## Typecheck ##

* Remove unecessary checks and functions
* Check float and double constant validity (e.g. range check as we did for IConst)

## Pass1 ###

* ?

## COQ Integration: ##

* check_cast in RCast
* check for pointer type 1st paramter of deref
* in NthRvalue check fist param to be int type and 2nd to be Arr or Ptr
* check for Return to occur only inside IFunction
* Check Loop index range 
* In Assign `check_coercion rt lt`
* Check that 'Program' contains only function definitions
  
## Backend: ##

* Check vpara/vshuffle mask types to be immediate values during code generation (not checked at typecheck step)



