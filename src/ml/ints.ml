open Core
open Sexplib
open Stdint

module IntEx (T:Int) = struct
  include T
  let tname = (if (T.compare T.min_int T.zero) = 0 then "U" else "I") ^
                string_of_int T.bits

  let sexp_of_t (v:T.t) = Sexp.List [Sexp.Atom tname; Sexp.Atom (T.to_string v)]

  let t_of_sexp = function
    | Sexp.List ((Sexp.Atom tname)::(Sexp.Atom x)::nil) -> T.of_string x
    | x  -> of_sexp_error "Must be List" x
end

module Int8Ex = IntEx(Int8)
open Int8Ex
module Int16Ex = IntEx(Int16)
open Int16Ex
module Int32Ex = IntEx(Int32)
open Int32Ex
module Int64Ex = IntEx(Int64)
open Int64Ex

module Uint8Ex = IntEx(Uint8)
open Uint8Ex
module Uint16Ex = IntEx(Uint16)
open Uint16Ex
module Uint32Ex = IntEx(Uint32)
open Uint32Ex
module Uint64Ex = IntEx(Uint64)
open Uint64Ex

let in_range f t x = Uint64Ex.compare x f >= 0 && Uint64Ex.compare x t <= 0
let in_int32_range x = in_range Uint64Ex.zero (Int32Ex.to_uint64 Int32Ex.max_int) x
let in_uint32_range x = in_range (Uint32Ex.to_uint64 Uint32Ex.min_int) (Uint32Ex.to_uint64 Uint32Ex.max_int) x
let in_int64_range x = in_range Uint64Ex.zero (Int64Ex.to_uint64 Int64Ex.max_int) x
