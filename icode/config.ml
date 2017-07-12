open Ast
open IType
open IIntType
open IArithType

(* Mapping of generic numeric types to actual machine types. It is hardcoded now, but will be managed via config file or command line options later *)

let realAType () = DoubleType
let prtSizeOf () = 8


(* --- helper functoin, in IArithType *)
let realType () = A (realAType ())
