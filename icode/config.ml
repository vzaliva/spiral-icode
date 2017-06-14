open Ast
open IType

(* Mapping of generic numeric types to actual machine types. It is hardcoded now, but will be managed via config file or command line options later *)
let realType () = DoubleType
let intType () = Int64Type
let uIntType () = UInt64Type
