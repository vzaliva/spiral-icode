
* Check vpara/vshuffle mask types to be immediate values during code generation (not checked at typecheck step)
* More generally maybe introduce notion of immediate values, also maybe needed for DATA
* Introduce actual VPARAM type
* Sort out VPARAM subtypes.
* See where Arrays are actually vectors and where they are just arrays
* Clarify vec/array casts in `check_coercion`



Define in Coq and Extract

Check length in const array/vec type matches number of elemens.
Check iconst type and U/I64 value match




vstore_2l_4x32f(TPtr(TVect(TReal, 2)), TVect(TReal, 4))
vstore_2h_4x32f(TPtr(TVect(TReal, 2)), TVect(TReal, 4))
vstoreu_4x32f(TPtr(TReal), TVect(TFloat, 4))
                                
