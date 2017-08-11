(* I-code AST *)

open Core
open Sexplib
open Lexing
open Stdint
open Ints

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
    | PtrType of t*int (* type, alignment *) [@@deriving compare, sexp]
end
open IType
module ITypeSet = Set.Make(IType)

type ivar = string [@@deriving compare, sexp]

type fconst = {
    node: fconst_node;
    loc: Loc.t
} [@@deriving compare, sexp]
and fconst_node =
  | FPLiteral of (IFloatType.t * string) (* Since OCaml does not have single precision floats to avoid rounding and casting problems we represent them as strings *)
  | FloatEPS
  | DoubleEPS [@@deriving compare, sexp]

type iconst = {
    node: iconst_node;
    loc: Loc.t
} [@@deriving compare, sexp]
and iconst_node =
    | Int8Const   of Int8Ex.t
    | Int16Const  of Int16Ex.t
    | Int32Const  of Int32Ex.t
    | Int64Const  of Int64Ex.t
    | UInt8Const  of Uint8Ex.t
    | UInt16Const of Uint16Ex.t
    | UInt32Const of Uint32Ex.t
    | UInt64Const of Uint64Ex.t
    | BoolConst   of bool       [@@deriving compare, sexp]

type rvalue = {
    rnode: rvalue_node;
    rloc: Loc.t
} [@@deriving compare, sexp]
and rvalue_node =
  | FunCallValue of string*(rvalue list)
  | VarRValue of ivar
  | VHex of (string list)
  | FConst of fconst
  | IConst of iconst
  | FConstArr of (IFloatType.t * (fconst list))
  | IConstArr of (IIntType.t   * (iconst list))
  | FConstVec of (IFloatType.t * (fconst list))
  | IConstVec of (IIntType.t   * (iconst list))
  | VdupRvalue of rvalue*iconst
  | NthRvalue of rvalue*rvalue (* 'int' type for index will be checked later *)
  | RCast of IType.t*rvalue
  | RDeref of rvalue [@@deriving compare, sexp]

type lvalue = {
    lnode: lvalue_node;
    lloc: Loc.t
} [@@deriving compare, sexp]
and lvalue_node =
  | VarLValue of ivar
  | NthLvalue of lvalue*rvalue
  | LDeref of rvalue
  | LCast of IType.t*lvalue [@@deriving compare, sexp]

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
  | Loop of ivar*Int64Ex.t*Int64Ex.t*istmt (* 'int' type for bounds, and a<=b will be checked later *)
  | If of rvalue*istmt*istmt
  | FunCallStmt of string*(rvalue list)
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

