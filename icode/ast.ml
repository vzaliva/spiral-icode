(* I-code AST *)

open Core
open Sexplib

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
    | BoolType [@@deriving compare, sexp]
end
open IIntType
module IIntTypeSet = Set.Make(IIntType)

module IType = struct
  type t =
    | VoidType
    | FloatType (* IEEE 32-bit float *)
    | DoubleType (* IEEE 64-bit float *)
    | I of IIntType.t
    | VecType of t*int
    | PtrType of t*(int list) (* type, alignment *) [@@deriving compare, sexp]
end

open IType
module ITypeSet = Set.Make(IType)

type ivar = string

type fconst =
  | FPLiteral of float
  | FloatEPS
  | DoubleEPS

type vparam =
  | VParamList of int list
  | VParamValue of int

type rvalue =
  | FunCall of string*(rvalue list)
  | VarRValue of string
  | FConst of fconst
  | IConst of int
  | FConstVec of (fconst list)
  | IConstVec of (int list)
  | NthRvalue of rvalue*rvalue (* 'int' type for index will be checked later *)
  | RCast of IType.t*rvalue
  | VParam of vparam
  | RDeref of rvalue

type lvalue =
  | VarLValue of string
  | NthLvalue of lvalue*rvalue
  | LDeref of lvalue
  | LCast of IType.t*lvalue

type istmt =
  | Function of string*IType.t*(ivar list)*istmt
  | Skip
  | Decl of (ivar list)*istmt
  | Chain of (istmt list)
  | Data of ivar*(rvalue list)*istmt (* homogenity of rvalues to be checked later *)
  | Assign of lvalue*rvalue
  | Loop of ivar*int*int*istmt (* 'int' type for bounds, and a<=b will be checked later *)
  | If of rvalue*istmt*istmt
  | Return of rvalue

type iprogram = Program of ((string*IType.t) list)*istmt

let eq_itype a b = compare a b = 0

let iType_of_IntType t = I t


(* -- Formatting --- *)
open Format

let rec pr_itype ppf = function
  | I Int8Type -> fprintf ppf "@[TInt8]"
  | I Int16Type -> fprintf ppf "@[TInt16]"
  | I Int32Type -> fprintf ppf "@[TInt32]"
  | I Int64Type -> fprintf ppf "@[TInt64]"
  | I UInt8Type -> fprintf ppf "@[TUInt8]"
  | I UInt16Type -> fprintf ppf "@[TUInt16]"
  | I UInt32Type -> fprintf ppf "@[TUInt32]"
  | I UInt64Type -> fprintf ppf "@[TUInt64]"
  | I BoolType -> fprintf ppf "@[TBool@]"
  | VoidType -> fprintf ppf "@[TVoid@]"
  | FloatType -> fprintf ppf "@[Float@]"
  | DoubleType -> fprintf ppf "@[Double@]"
  | VecType (t,s) -> fprintf ppf "@[%a[%d]@]" pr_itype t s
  | PtrType (t,_) -> fprintf ppf "@[%a@]" pr_itype t

let itype_as_string = Format.asprintf "%a" pr_itype

let type_list_fmt = Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";") pr_itype
