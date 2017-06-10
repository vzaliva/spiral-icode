(* Signatures of built-in functions *)

open Core
open Ast

(* function signature: return type and list of argument types *)
module FunSig = struct
  type t = (IType.t*(IType.t list)) [@@deriving compare, sexp]
end

let sig_with_same_typed_args_and_ret nargs typelist
  = List.map ~f:(fun t ->
               (t, List.map ~f:(fun _ -> t) (List.range 0 nargs))) typelist

let sig_with_same_typed_args rettype nargs typelist
  = List.map ~f:(fun t ->
               (rettype, List.map ~f:(fun _ -> t) (List.range 0 nargs))) typelist

open IType

let signed_numeric_types = [
    RealType ;
    FloatType ;
    DoubleType ;
    IntType ;
    Int8Type ;
    Int16Type ;
    Int32Type ;
    Int64Type ;
    UIntType]

let unsigned_numeric_types = [
    UInt8Type ;
    UInt16Type ;
    UInt32Type ;
    UInt64Type ]

let numeric_types = signed_numeric_types @ unsigned_numeric_types

let builtins_map =
  String.Map.Tree.of_alist_exn
    [
      ("max", sig_with_same_typed_args_and_ret 2 numeric_types) ;
      ("add", sig_with_same_typed_args_and_ret 2 numeric_types) ;
      ("sub", sig_with_same_typed_args_and_ret 2 numeric_types) ;
      ("mul", sig_with_same_typed_args_and_ret 2 numeric_types) ;
      ("div", sig_with_same_typed_args_and_ret 2 numeric_types) ;
      ("neg", sig_with_same_typed_args_and_ret 1 signed_numeric_types) ;
      ("abs", sig_with_same_typed_args_and_ret 1 numeric_types) ;
      ("geq", sig_with_same_typed_args BoolType 2 numeric_types) (* TODO: extend to non-numeric *) ;
    ]
