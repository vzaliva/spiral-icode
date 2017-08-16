open Core
open Sexplib
open Stdint
open ExtrOcamlIntConv
open BinNums
open BinInt
open Datatypes

let z_compare (x:coq_Z) (y:coq_Z) =
  match Z.compare x y with Lt -> -1 | Eq -> 0 | Gt -> 1

(* Convert binary string with optiona '0b' or '0B' prefix and sign to Z *)
let z_of_binstr (s:string) =
  let rec pos_of_binstr v s =
    if String.is_empty s then v
    else pos_of_binstr
           (match String.prefix s 1 with
            | "0" -> Coq_xO v
            | "1" -> Coq_xI v
            | c -> raise (Invalid_argument ("binary number contains unexpected character '" ^ c ^"'")))
           (String.drop_prefix s 1)
  in
  match String.lsplit2 ~on:'1' s with
  | None -> Z0
  | Some (_,b) ->
     let x = pos_of_binstr Coq_xH b in
     if String.prefix s 1 = "-"
     then (Zneg x)
     else (Zpos x)

(* Convert hex string with optiona '0x' or '0X' prefix and sign to Z *)
let z_of_hexstr (s:string) =
  (String.concat_map s (fun x -> match Char.lowercase x with
                                 | '0' -> "0000"
                                 | '1' -> "0001"
                                 | '2' -> "0010"
                                 | '3' -> "0011"
                                 | '4' -> "0100"
                                 | '5' -> "0101"
                                 | '6' -> "0110"
                                 | '7' -> "0111"
                                 | '8' -> "1000"
                                 | '9' -> "1001"
                                 | 'a' -> "1010"
                                 | 'b' -> "1011"
                                 | 'c' -> "1100"
                                 | 'd' -> "1101"
                                 | 'e' -> "1110"
                                 | 'f' -> "1111"
                                 | c -> raise (Invalid_argument ("hex number contains unexpected character '" ^ (Char.to_string c) ^"'"))
  )) |> z_of_binstr

module IntEx (T:Int) = struct
  include T
  let tname = (if (T.compare T.min_int T.zero) = 0 then "U" else "I") ^
                string_of_int T.bits

  let sexp_of_t (v:T.t) = Sexp.List [Sexp.Atom tname; Sexp.Atom (T.to_string v)]

  let t_of_sexp = function
    | Sexp.List ((Sexp.Atom tname)::(Sexp.Atom x)::nil) -> T.of_string x
    | x  -> of_sexp_error "Must be List" x

  let to_z (v:T.t) = z_of_binstr (to_string_bin v)

  let in_range z =
    z_compare z (to_z T.min_int) >= 0 &&
      z_compare z (to_z T.max_int) <= 0

  let in_range_u x =
    Uint64.compare x (T.to_uint64 T.max_int) <= 0

  let in_range_s x =
    Int64.compare x (T.to_int64 T.min_int) >= 0 &&
      Int64.compare x (T.to_int64 T.max_int) <= 0

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

let z_of_bool (v:bool) =
  if v then Zpos (Coq_xH) else Z0
