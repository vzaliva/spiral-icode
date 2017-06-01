(* I-code AST *)


type itype =
  | VoidType
  | RealType
  | FloatType (* IEEE 32-bit float *)
  | DoubleType (* IEEE 64-bit float *)
  | IntType
  | BoolType
  | OtherType of string
  | UnknownType
  | ArrayType of itype*(int option)
  | PtrType of itype

type ivar = string

type rvalue =
  | FunCall of string*(rvalue list)
  | VarRValue of string
  | FConst of float
  | IConst of int
  | NthRvalue of rvalue*rvalue (* 'int' type for index will be checked later *)

type lvalue =
  | VarLValue of string
  | NthLvalue of lvalue*rvalue

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

open Format

let rec pr_itype ppf = function
  | VoidType -> fprintf ppf "@[TVoid@]"
  | RealType -> fprintf ppf "@[TReal@]"
  | FloatType -> fprintf ppf "@[Float@]"
  | DoubleType -> fprintf ppf "@[Double@]"
  | IntType -> fprintf ppf "@[TInt@]"
  | BoolType -> fprintf ppf "@[TBool@]"
  | OtherType n -> fprintf ppf "@[%s@]" n
  | UnknownType -> fprintf ppf "@[?@]"
  | ArrayType (t,s) -> fprintf ppf "@[%a[%s]@]" pr_itype t
                               (match s with
                               | None -> "?"
                               | Some d -> string_of_int d)
  | PtrType t -> fprintf ppf "@[%a@]" pr_itype t
