(* iCode AST *)

Require Import List.
Require Import ZArith.
Require Import String.

Import ListNotations.

Open Scope list_scope.

Inductive IIntType :=
| Int8Type
| Int16Type
| Int32Type
| Int64Type
| UInt8Type
| UInt16Type
| UInt32Type
| UInt64Type
| BoolType.

Inductive IFloatType :=
| FloatType (* IEEE 32-bit float *)
| DoubleType (* IEEE 64-bit float *).

Inductive IArithType :=
| I (value:IIntType)
| F (value: IFloatType).

Inductive IType :=
| A (value:IArithType)
| VoidType
| ArrType (t:IType) (len:Z)
| VecType (t:IArithType) (len:Z)
| PtrType (t:IType) (alignment:Z).

(* Types for function and vairable names. For simplicity we will index them by integers *)
Definition varname := Z.
Definition funcname := Z.

Definition float := string.
Definition hexvalue := string.

Inductive fconst :=
| FLiteral (value:float)
| FEPS.

Inductive dconst :=
| DLiteral (value:float)
| DEPS.

Inductive rvalue :=
| FunCallValue (name:funcname) (params:list rvalue)
| VarRValue (var:varname)
| VHex (values: list hexvalue)
| FConst (value:fconst)
| DConst (value:dconst)
| IConst (type:IIntType) (value:Z)
| IConstArr (type:IIntType) (values: list Z)
| FConstArr (values: list fconst)
| DConstArr (values: list dconst)
| FConstVec (values: list fconst)
| DConstVec (values: list dconst)
| IConstVec (type:IIntType) (values: list Z)
| VdupRvalue (r:rvalue) (n:Z)
| NthRvalue (r:rvalue) (index:Z)
| RCast (type:IType) (r:rvalue)
| RDeref (r:rvalue).

Inductive lvalue :=
| VarLValue (var:varname)
| NthLvalue (l:lvalue) (index:rvalue)
| LDeref (r:rvalue)
| LCast (type:IType) (l:lvalue).

Inductive istmt :=
| IFunction (name: funcname) (type:IType) (param: list varname) (body:istmt)
| Skip
| Decl (vars: list varname) (body:istmt)
| Chain (body: list istmt)
| Data (var: varname) (values: list rvalue) (body:istmt)
| Assign (l:lvalue) (r:rvalue)
| Loop (var: varname) (from:Z) (to:Z) (body:istmt)
| If (cond:rvalue) (thenbranch:istmt) (elsebranch:istmt)
| FunCallStmt (name: funcname) (params: list rvalue)
| Return (r:rvalue).

Inductive iprogram :=
| Program (bindings: varname -> option IType) (body:istmt). (* TODO: map? *)
