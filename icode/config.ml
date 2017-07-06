open Ast
open IType
open IIntType
open IArithType

(* Mapping of generic numeric types to actual machine types. It is hardcoded now, but will be managed via config file or command line options later *)

let realAType () = DoubleType
let intIntType () = Int32Type
let uIntIntType () = UInt32Type


(* --- helper functoin, in IArithType *)
let intAType () = I (intIntType ())
let uIntAType () = I (uIntIntType ())

(* --- helper functoin, in IType *)
let intType () = A (intAType ())
let uIntType () = A (uIntAType ())
let realType () = A (realAType ())
