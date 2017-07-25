(* I-code AST *)

open Core
open Sexplib
open Uint64
open Lexing

let sexp_of_position p =
  let sexp_kv k v = Sexp.List [ Sexp.Atom k; Sexp.Atom v] in
  Sexp.List [
      sexp_kv "pos_fname" p.pos_fname ;
      sexp_kv "pos_lnum" (string_of_int p.pos_lnum);
      sexp_kv "pos_bol"  (string_of_int p.pos_bol );
      sexp_kv "pos_cnum" (string_of_int p.pos_cnum)
    ]

let position_of_sexp: Sexp.t -> position = function
  | Sexp.List l as s ->
     let open List in
     if 4 <> length l then
       of_sexp_error "position: List too short" s
     else
       (* Hardcode order. shoud be alist *)
       {
         pos_fname  = string_of_sexp (nth_exn l 0) ;
         pos_lnum   = int_of_sexp (nth_exn l 1) ;
         pos_bol    = int_of_sexp (nth_exn l 2) ;
         pos_cnum   = int_of_sexp (nth_exn l 3)
       }
  | x  -> of_sexp_error "position_of_sexp: must be List" x

let compare_position x y =
  let c = compare_string x.pos_fname y.pos_fname in
  if c <> 0 then c
  else let c = compare_int x.pos_lnum y.pos_lnum in
       if c <> 0 then c
       else let c = compare_int x.pos_bol y.pos_bol in
            if c <> 0 then c
            else compare_int x.pos_cnum y.pos_cnum

(* Location code inspired by https://github.com/lucasaiu/ocaml/blob/master/parsing/location.ml *)
module Loc = struct
  type t = { loc_start: position; loc_end: position; loc_ghost: bool } [@@deriving compare, sexp]
end

(* Machine-safe int to hold any int value, signed or unsiged *)
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

end


module IIntType = struct
  type t =
    | Int8Type
    | Int16Type
    | Int32Type
    | Int64Type
    | UInt8Type
    | UInt16Type
    | UInt32Type
    | UInt64Type
    | BoolType [@@deriving compare, sexp, enumerate]
end
open IIntType
module IIntTypeSet = Set.Make(IIntType)

module IFloatType = struct
  type t =
    | FloatType (* IEEE 32-bit float *)
    | DoubleType (* IEEE 64-bit float *)  [@@deriving compare, sexp, enumerate]
end
open IFloatType
module IFloatTypeSet = Set.Make(IFloatType)

module IArithType = struct
  type t =
    | I of IIntType.t
    | F of IFloatType.t [@@deriving compare, sexp, enumerate]
end
open IArithType
module IArithTypeSet = Set.Make(IArithType)

module IType = struct
  type t =
    | A of IArithType.t
    | VoidType
    | ArrType of t*int
    | VecType of IArithType.t*int
    | PtrType of t*(int option) (* type, optional alignment *) [@@deriving compare, sexp]
end
open IType
module ITypeSet = Set.Make(IType)

type ivar = string [@@deriving compare, sexp]

type fconst = {
    node: fconst_node;
    loc: Loc.t
} [@@deriving compare, sexp]
and fconst_node =
  | FPLiteral of (IFloatType.t * float)
  | FloatEPS
  | DoubleEPS [@@deriving compare, sexp]

type iconst = {
    node: iconst_node;
    loc: Loc.t
} [@@deriving compare, sexp]
and iconst_node =
  | ILiteral of (IIntType.t * Int_or_uint_64.t) [@@deriving compare, sexp]

type rvalue = {
    rnode: rvalue_node;
    rloc: Loc.t
} [@@deriving compare, sexp]
and rvalue_node =
  | RFunCall of string*(rlvalue list)
  | VarRValue of ivar
  | VHex of (string list)
  | FConst of fconst
  | IConst of iconst
  | FConstArr of (IFloatType.t * (fconst list))
  | IConstArr of (IIntType.t   * (iconst list))
  | FConstVec of (IFloatType.t * (fconst list))
  | IConstVec of (IIntType.t   * (iconst list))
  | VdupRvalue of rvalue*Int_or_uint_64.t
  | NthRvalue of rvalue*rvalue (* 'int' type for index will be checked later *)
  | RCast of IType.t*rvalue
  | RDeref of rvalue [@@deriving compare, sexp]
and lvalue = {
    lnode: lvalue_node;
    lloc: Loc.t
} [@@deriving compare, sexp]
and lvalue_node =
  | LFunCall of string*(rlvalue list)
  | VarLValue of ivar
  | NthLvalue of lvalue*rvalue
  | LDeref of rvalue
  | LCast of IType.t*lvalue [@@deriving compare, sexp]
and rlvalue =
  | RValue of rvalue
  | LValue of lvalue [@@deriving compare, sexp]

type istmt = {
    node: istmt_node;
    loc: Loc.t
} [@@deriving compare, sexp]
and istmt_node =
  | Function of string*IType.t*(ivar list)*istmt
  | Skip
  | Decl of (ivar list)*istmt
  | Chain of (istmt list)
  | Data of ivar*(rvalue list)*istmt (* homogenity of rvalues to be checked later *)
  | Assign of lvalue*rvalue
  | Loop of ivar*Int_or_uint_64.t*Int_or_uint_64.t*istmt (* 'int' type for bounds, and a<=b will be checked later *)
  | If of rvalue*istmt*istmt
  | Vstore_2l_4x32f of lvalue*rvalue
  | Vstore_2h_4x32f of lvalue*rvalue
  | Vstoreu_4x32f of lvalue*rvalue
  | Return of rvalue [@@deriving compare, sexp]

type iprogram = Program of ((ivar*IType.t) list)*istmt [@@deriving compare, sexp]

let eq_itype (a:IType.t) (b:IType.t) = IType.compare a b = 0
let eq_int_type (a:IIntType.t) (b:IIntType.t) = IIntType.compare a b = 0
let eq_float_type (a:IFloatType.t) (b:IFloatType.t) = IFloatType.compare a b = 0
let iType_of_IntType t = A (I t)

(* --- Convinience functions for creating nodes --- *)
let symbol_rloc s e = {
    Loc.loc_start = s;
    Loc.loc_end = e;
    Loc.loc_ghost = false;
  }

let mkstmt   s e (d:istmt_node ): istmt   = { node = d; loc = symbol_rloc s e}
let mkrvalue s e (d:rvalue_node): rvalue  = { rnode = d; rloc = symbol_rloc s e}
let mklvalue s e (d:lvalue_node): lvalue  = { lnode = d; lloc = symbol_rloc s e}
let mkfconst s e (d:fconst_node): fconst  = { node = d; loc = symbol_rloc s e}
let mkiconst s e (d:iconst_node): iconst  = { node = d; loc = symbol_rloc s e}

(* Mapping of generic numeric types to actual machine types. It is hardcoded now, but will be managed via config file or command line options later *)

let realFType () = if !Config.isDouble then DoubleType else FloatType
let realAType () = F (realFType ())
let prtSizeOf () = if !Config.is64bit then 8 else 4

(* --- helper functoin, in IArithType *)
let realType () = A (realAType ())


(* -- Formatting --- *)
open Format

let rec pr_itype ppf = function
  | A I Int8Type   -> fprintf ppf "@[TInt8@]"
  | A I Int16Type  -> fprintf ppf "@[TInt16@]"
  | A I Int32Type  -> fprintf ppf "@[TInt32@]"
  | A I Int64Type  -> fprintf ppf "@[TInt64@]"
  | A I UInt8Type  -> fprintf ppf "@[TUInt8@]"
  | A I UInt16Type -> fprintf ppf "@[TUInt16@]"
  | A I UInt32Type -> fprintf ppf "@[TUInt32@]"
  | A I UInt64Type -> fprintf ppf "@[TUInt64@]"
  | A I BoolType   -> fprintf ppf "@[TBool@]"
  | A F FloatType  -> fprintf ppf "@[Float@]"
  | A F DoubleType -> fprintf ppf "@[Double@]"
  | VoidType       -> fprintf ppf "@[TVoid@]"
  | ArrType (t,s)  -> fprintf ppf "@[<h>Arr(%a,%d)@]" pr_itype t s
  | VecType (t,s)  -> fprintf ppf "@[<h>Vec(%a,%d)@]" pr_itype (A t) s
  | PtrType (t,_)  -> fprintf ppf "@[<h>Ptr(%a)@]" pr_itype t

let itype_as_string = Format.asprintf "%a" pr_itype

let type_list_fmt sep = Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt sep) pr_itype

let pr_err_loc ppf (l:Loc.t) =
  fprintf ppf "%a: error:" Utils.pr_pos l.Loc.loc_start
