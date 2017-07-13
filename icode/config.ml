open Ast
open IType
open IIntType
open IArithType

let debug = ref false
let is64bit = ref true (* by default 64 bit (opposed to 32) *)

(* Mapping of generic numeric types to actual machine types. It is hardcoded now, but will be managed via config file or command line options later *)

let realAType () = if !is64bit then DoubleType else FloatType
let prtSizeOf () = if !is64bit then 8 else 4

(* --- helper functoin, in IArithType *)
let realType () = A (realAType ())


(* Some loggnig utilities *)

(*
let null_formatter = Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())

let msg =
  let open Format in
  fprintf (if !debug then err_formatter else null_formatter)

let msg =
  let open Format in
  (if !debug then fprintf err_formatter else ifprintf err_formatter)

 *)

