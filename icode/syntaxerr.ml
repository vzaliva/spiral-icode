(* Workaround to raise this exception from parser action without intoducing cyclic depenency to any modules using parser *)

exception Error of string
