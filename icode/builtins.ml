(* Signatures of built-in functions *)

open Core
open Ast

(* function signature: return type and list of argument types *)
module FunSig = struct
  type t = (IType.t*(IType.t list)) [@@deriving compare, sexp]
end

let make_sig_with_same_typed_args_and_ret nargs typelist
  = List.map ~f:(fun t ->
               (t, List.map ~f:(fun _ -> t) (List.range 0 nargs))) typelist

open IType

let numeric_types = [
    RealType ;
    FloatType ;
    DoubleType ;
    IntType ;
    Int8Type ;
    Int16Type ;
    Int32Type ;
    Int64Type ;
    UIntType ;
    UInt8Type ;
    UInt16Type ;
    UInt32Type ;
    UInt64Type ]

let builtins_map =
  String.Map.Tree.of_alist_exn
    [
      ("max", make_sig_with_same_typed_args_and_ret 2 numeric_types) ;
      ("add", make_sig_with_same_typed_args_and_ret 2 numeric_types) ;
      ("sub", make_sig_with_same_typed_args_and_ret 2 numeric_types) ;
      ("mul", make_sig_with_same_typed_args_and_ret 2 numeric_types) ;
      ("div", make_sig_with_same_typed_args_and_ret 2 numeric_types) ;
      ("abs", make_sig_with_same_typed_args_and_ret 1 numeric_types)
    ]
