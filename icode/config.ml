open Ast
open IType
open IIntType
open IFloatType
open IArithType

let debug = ref false
let is64bit = ref true (* by default 64 bit (opposed to 32) *)

(* Mapping of generic numeric types to actual machine types. It is hardcoded now, but will be managed via config file or command line options later *)

let realFType () = if !is64bit then DoubleType else FloatType
let realAType () = F (realFType ())
let prtSizeOf () = if !is64bit then 8 else 4

(* --- helper functoin, in IArithType *)
let realType () = A (realAType ())


(* Some loggnig utilities *)

let msg x =
  let open Format in
  (if !debug then fprintf err_formatter x else ifprintf err_formatter x)

