(* I-code AST *)

open Core
open Sexplib

type itype =
  | VoidType
  | RealType (* generic Real type *)
  | FloatType (* IEEE 32-bit float *)
  | DoubleType (* IEEE 64-bit float *)
  | IntType (* generic int *)
  | Int8Type
  | Int16Type
  | Int32Type
  | Int64Type
  | UIntType (* generic unsigned int *)
  | UInt8Type
  | UInt16Type
  | UInt32Type
  | UInt64Type
  | BoolType
  | OtherType of string
  | UnknownType
  | VecType of itype*int
  | PtrType of itype*(int list) (* type, alignment *) [@@deriving compare, sexp]

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
  | RCast of itype*rvalue
  | VParam of vparam
  | RDeref of rvalue

type lvalue =
  | VarLValue of string
  | NthLvalue of lvalue*rvalue
  | LDeref of lvalue
  | LCast of itype*lvalue

type istmt =
  | Function of string*itype*(ivar list)*istmt
  | Skip
  | Decl of (ivar list)*istmt
  | Chain of (istmt list)
  | Data of ivar*(rvalue list)*istmt (* homogenity of rvalues to be checked later *)
  | Assign of lvalue*rvalue
  | Loop of ivar*int*int*istmt (* 'int' type for bounds, and a<=b will be checked later *)
  | If of rvalue*istmt*istmt
  | Return of rvalue

type iprogram = Program of ((string*itype) list)*istmt

(* -- For use in Set/Maps -- *)

module IType = struct type t = itype [@@deriving compare, sexp] end
module ITypeSet = Set.Make(IType)

let eq_itype a b = compare_itype a b = 0

(* -- Formatting --- *)
open Format

let rec pr_itype ppf = function
  | Int8Type -> fprintf ppf "@[TInt8]"
  | Int16Type -> fprintf ppf "@[TInt16]"
  | Int32Type -> fprintf ppf "@[TInt32]"
  | Int64Type -> fprintf ppf "@[TInt64]"
  | UIntType -> fprintf ppf "@[TUInt]"
  | UInt8Type -> fprintf ppf "@[TUInt8]"
  | UInt16Type -> fprintf ppf "@[TUInt16]"
  | UInt32Type -> fprintf ppf "@[TUInt32]"
  | UInt64Type -> fprintf ppf "@[TUInt64]"
  | VoidType -> fprintf ppf "@[TVoid@]"
  | RealType -> fprintf ppf "@[TReal@]"
  | FloatType -> fprintf ppf "@[Float@]"
  | DoubleType -> fprintf ppf "@[Double@]"
  | IntType -> fprintf ppf "@[TInt@]"
  | BoolType -> fprintf ppf "@[TBool@]"
  | OtherType n -> fprintf ppf "@[%s@]" n
  | UnknownType -> fprintf ppf "@[?@]"
  | VecType (t,s) -> fprintf ppf "@[%a[%d]@]" pr_itype t s
  | PtrType (t,_) -> fprintf ppf "@[%a@]" pr_itype t

let itype_as_string = Format.asprintf "%a" pr_itype
