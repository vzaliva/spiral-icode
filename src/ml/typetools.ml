open Ast
open IType
open IIntType
open IFloatType
open IArithType
open Ints
open Utils

let realFType () = if !Config.isDouble then DoubleType else FloatType
let realAType () = F (realFType ())
let ptrSizeOf () = if !Config.is64bit then 8 else 4

(* --- helper functoin, in IArithType *)
let realType () = A (realAType ())


let signed_integer_types =
  IIntTypeSet.of_list [
      Int8Type ;
      Int16Type ;
      Int32Type ;
      Int64Type]

let is_signed_integer =
  IIntTypeSet.mem signed_integer_types

let unsigned_integer_types =
  IIntTypeSet.of_list [
      BoolType ;
      UInt8Type ;
      UInt16Type ;
      UInt32Type ;
      UInt64Type ]

let is_unsigned_integer =
  IIntTypeSet.mem unsigned_integer_types

let integer_types = IIntTypeSet.of_list IIntType.all

let signed_arith_types = ITypeSet.union
                           (ITypeSet.of_list [
                                A (F FloatType) ;
                                A (F DoubleType) ; ])
                           (ITypeSet.map ~f:iType_of_IntType signed_integer_types)

let arith_types =
  IArithTypeSet.of_list IArithType.all

let is_integer = function
  | A I _ -> true
  | _ -> false

let is_arith = function
  | A _ -> true
  | _ -> false

let is_signed_arith = function
  | I t -> IIntTypeSet.mem signed_integer_types t
  | F _ -> true

let is_unsigned_arith = function
  | I t -> IIntTypeSet.mem unsigned_integer_types t
  | F _ -> false

let integer_type_rank = function
  | BoolType                -> 0
  | Int8Type  | UInt8Type   -> 1
  | Int16Type | UInt16Type  -> 2
  | Int32Type | UInt32Type  -> 3
  | Int64Type | UInt64Type  -> 4

let int_sizeof = function
  | BoolType | Int8Type  | UInt8Type   -> 1
  | Int16Type | UInt16Type  -> 2
  | Int32Type | UInt32Type  -> 4
  | Int64Type | UInt64Type  -> 8

let arith_sizeof = function
  | I i -> int_sizeof i
  | F FloatType -> 4
  | F DoubleType -> 8

let rec sizeof = function
  | VoidType -> 0
  | A t -> arith_sizeof t
  | ArrType (t,l) -> sizeof t * l
  | VecType (at,l) -> arith_sizeof at * l
  | PtrType _ -> ptrSizeOf ()

let unsigned_type = function
  | Int8Type -> UInt8Type
  | Int16Type -> UInt16Type
  | Int32Type -> UInt32Type
  | Int64Type -> UInt64Type
  | _ as t -> t

let natural_alignment = function
  | ArrType _ -> ptrSizeOf ()
  | t -> sizeof t

let type_of_const = function
    | Int8Const   _ -> Int8Type
    | Int16Const  _ -> Int16Type
    | Int32Const  _ -> Int32Type
    | Int64Const  _ -> Int64Type
    | UInt8Const  _ -> UInt8Type
    | UInt16Const _ -> UInt16Type
    | UInt32Const _ -> UInt32Type
    | UInt64Const _ -> UInt64Type
    | BoolConst   _ -> BoolType

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
  | PtrType (t, a) -> fprintf ppf "@[<h>Ptr(%a, %d)@]" pr_itype t a

let itype_as_string = Format.asprintf "%a" pr_itype

let type_list_fmt sep = Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt sep) pr_itype

let pr_err_loc ppf (l:Loc.t) =
  fprintf ppf "%a: error:" Utils.pr_pos l.Loc.loc_start

let is_power_of_2 n =  n <> 0 && (n land (n - 1) = 0)

