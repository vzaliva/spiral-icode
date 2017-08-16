Require Import Nat.

Require Import Icode.IAst.

Definition is_signed_integer x : bool :=
  match x with
  | Int8Type | Int16Type | Int32Type | Int64Type => true
  | UInt8Type | UInt16Type | UInt32Type | UInt64Type => false
  | BoolType => false
  end.

Definition integer_type_rank x :=
  match x with
  | BoolType => 0
  | Int8Type | UInt8Type => 1
  | Int16Type | UInt16Type => 2
  | Int32Type | UInt32Type => 3
  | Int64Type | UInt64Type => 4
  end.

Local Open Scope nat.
Definition integer_promotion t :=
  let i := if is_signed_integer t then Int32Type else UInt32Type in
  if integer_type_rank t <? integer_type_rank i then i else t.
