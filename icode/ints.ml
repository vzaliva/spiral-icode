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

  let in_int8_range = in_range64 "-128" "127"
  let in_int16_range = in_range64 "-32768" "32767"
  let in_int32_range = in_range64 "-2147483648" "2147483647"

  (* inclusive *)
  let in_rangeU64 f t x =
    Uint64.compare x (Uint64.of_string f) >= 0 &&
      Uint64.compare x (Uint64.of_string t) <= 0

  let in_uint8_range = in_rangeU64 "0" "255"
  let in_uint16_range = in_rangeU64 "0" "65535"
  let in_uint32_range = in_rangeU64 "0" "4294967295"

  let uint16_cast : t -> int option = function
    | U64 x -> if in_uint16_range x then Some (to_int x) else None
    | I64 x -> if in_range64 "0" "65535" x then Int64.to_int x else None


(*  let int_const_in_range t c =
   match t with
   | Int8Type   -> in_int8_range c
   | Int16Type  -> in_int16_range c
   | Int32Type  -> in_int32_range c
   | Int64Type  -> true (\* TODO *\)
   | UInt8Type  -> in_uint8_range c
   | UInt16Type -> in_uint16_range c
   | UInt32Type -> in_uint32_range c
   | UInt64Type -> true
   | BoolType   -> true (\* TODO *\) *)


end
