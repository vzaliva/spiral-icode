
let debug = ref false
let stop_on_err = ref false

let is64bit = ref true (* by default 64 bit pointers *)
let isDouble = ref false (* by default Reals is IEEE single precision float *)
let vecLen = ref 128 (* default length in bits of vector registers *)
