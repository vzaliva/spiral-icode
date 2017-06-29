(* I-code AST *)

open Core
open Sexplib
open Uint64

(* Machine-safe int to hold any int value, signed or unsiged *)
module Int_or_uint_64 = struct
  type t =
    | I64 of int64
    | U64 of uint64

  let to_string = function
    | I64 x -> Int64.to_string x
    | U64 x -> Uint64.to_string x ^ "u"
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

module IArithType = struct
  type t =
    | I of IIntType.t
    | FloatType (* IEEE 32-bit float *)
    | DoubleType (* IEEE 64-bit float *)  [@@deriving compare, sexp, enumerate]
end
open IArithType
module IArithTypeSet = Set.Make(IArithType)

module IType = struct
  type t =
    | A of IArithType.t
    | VoidType
    | ArrType of t*int
    | VecType of t*int
    | PtrType of t*(int option) (* type, optional alignment *) [@@deriving compare, sexp]
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
  | VarRValue of ivar
  | FConst of fconst
  | IConst of Int_or_uint_64.t
  | FConstArr of (fconst list)
  | IConstArr of (Int_or_uint_64.t list)
  | NthRvalue of rvalue*rvalue (* 'int' type for index will be checked later *)
  | RCast of IType.t*rvalue
  | VParam of vparam
  | RDeref of rvalue

type lvalue =
  | VarLValue of ivar
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
  | Loop of ivar*Int_or_uint_64.t*Int_or_uint_64.t*istmt (* 'int' type for bounds, and a<=b will be checked later *)
  | If of rvalue*istmt*istmt
  | Return of rvalue

type iprogram = Program of ((ivar*IType.t) list)*istmt

let eq_itype (a:IType.t) (b:IType.t) = IType.compare a b = 0
let eq_int_type (a:IIntType.t) (b:IIntType.t) = IIntType.compare a b = 0
let iType_of_IntType t = A (I t)


(* -- Formatting --- *)
open Format

let rec pr_itype ppf = function
  | A I Int8Type -> fprintf ppf "@[TInt8]"
  | A I Int16Type -> fprintf ppf "@[TInt16]"
  | A I Int32Type -> fprintf ppf "@[TInt32]"
  | A I Int64Type -> fprintf ppf "@[TInt64]"
  | A I UInt8Type -> fprintf ppf "@[TUInt8]"
  | A I UInt16Type -> fprintf ppf "@[TUInt16]"
  | A I UInt32Type -> fprintf ppf "@[TUInt32]"
  | A I UInt64Type -> fprintf ppf "@[TUInt64]"
  | A I BoolType -> fprintf ppf "@[TBool@]"
  | A FloatType -> fprintf ppf "@[Float@]"
  | A DoubleType -> fprintf ppf "@[Double@]"
  | VoidType -> fprintf ppf "@[TVoid@]"
  | ArrType (t,s) -> fprintf ppf "@[%a[%d]@]" pr_itype t s
  | VecType (t,s) -> fprintf ppf "@[%a<%d>@]" pr_itype t s
  | PtrType (t,_) -> fprintf ppf "@[%a@]" pr_itype t

let itype_as_string = Format.asprintf "%a" pr_itype

let type_list_fmt = Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ";") pr_itype

