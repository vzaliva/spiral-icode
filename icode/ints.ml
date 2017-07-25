open Core
open Sexplib
open Uint64

(** Machine-safe int to hold any int value, signed or unsiged, 8-64 bit *)
module Int_or_uint_64 = struct
  type t =
    | I64 of int64
    | U64 of uint64

  let to_string = function
    | I64 x -> Int64.to_string x
    | U64 x -> Uint64.to_string x ^ "u"

  let sexp_of_t = function
    | I64 x as v -> Sexp.List [Sexp.Atom "I64"; Sexp.Atom (to_string v)]
    | U64 x as v -> Sexp.List [Sexp.Atom "U64"; Sexp.Atom (to_string v)]

  let t_of_sexp = function
    | Sexp.List l as s ->
       let open List in
       if 2 <> length l then
         of_sexp_error "Int_or_uint_64: List too short" s
       else
         let l0 = nth_exn l 0 in
         let l1 = nth_exn l 1 in
         (match l0 , l1 with
          | Sexp.Atom "U64", Sexp.Atom x -> U64 (Uint64.of_string x)
          | Sexp.Atom "I64", Sexp.Atom x -> I64 (Int64.of_string x)
          | _, _ -> of_sexp_error "Int_or_uint_64: must be 2 atoms" s)
    | x  -> of_sexp_error "Int_or_uint_64: must be List" x

  let compare a b =
    match a, b with
    | I64 x, I64 y -> Int64.compare x y
    | U64 x, U64 y -> Uint64.compare x y
    | _, _ -> invalid_arg "Int_or_uint_64: not comparable"

  (* inclusive *)
  let in_range64 f t x =
    Int64.compare x (Int64.of_string f) >= 0 &&
      Int64.compare x (Int64.of_string t) <= 0

  (* inclusive *)
  let in_rangeU64 f t x =
    try
      Uint64.compare x (Uint64.of_string f) >= 0 &&
        Uint64.compare x (Uint64.of_string t) <= 0
    with
    | _ -> false

  let in_int8_range = function
    | I64 s -> in_range64 "-128" "127" s
    | U64 u -> in_rangeU64 "0" "127" u

  let in_int16_range = function
    | I64 s -> in_range64 "-32768" "32767" s
    | U64 u -> in_rangeU64 "0" "32767" u

  let in_int32_range = function
    | I64 s -> in_range64 "-2147483648" "2147483647" s
    | U64 u -> in_rangeU64 "0" "2147483647" u

  let in_int64_range = function
    | I64 _ -> true
    | U64 u -> in_rangeU64 "0"  (Int64.to_string (Int64.max_value)) u

  let in_uint8_range = function
    | U64 u -> in_rangeU64 "0" "255" u
    | I64 s -> in_range64 "0" "255" s

  let in_uint16_range = function
    | U64 u -> in_rangeU64 "0" "65535" u
    | I64 s -> in_range64  "0" "65535" s

  let in_uint32_range = function
    | U64 u -> in_rangeU64 "0" "4294967295" u
    | I64 s -> in_range64  "0" "65535" s

  let in_uint64_range = function
    | U64 _ -> true
    | I64 s -> Int64.compare s (Int64.of_string "0") >= 0

  let to_int = function
    | U64 x -> if in_rangeU64 "0" (string_of_int (Pervasives.max_int)) x
               then Some (to_int x)
               else None
    | I64 x -> Int64.to_int x

  let in_bool_range = function
    | I64 s -> in_range64 "0" "1" s
    | U64 u -> in_rangeU64 "0" "1" u

  let uint16_cast x =
    if in_uint16_range x then to_int x else None

end
