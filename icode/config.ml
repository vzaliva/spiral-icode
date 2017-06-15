open Ast
open IType
open IIntType

(* Mapping of generic numeric types to actual machine types. It is hardcoded now, but will be managed via config file or command line options later *)
let realType () = DoubleType

let intIntType () = Int64Type
let uIntIntType () = UInt64Type


(* --- helper functoin, in IType *)
let intType () = I (intIntType ())
let uIntType () = I (uIntIntType ())
