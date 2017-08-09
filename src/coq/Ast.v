(* iCode AST *)

Require Import List.
Require Import ZArith.
Require Import String Ascii.

Import ListNotations.

Open Scope string_scope.
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

Definition ivar := string. (* TODO: Consider Z *)
Definition float := string.

Inductive fconst (type:IFloatType) :=
| FPLiteral (value:float)
| EPS.

Inductive iconst (type:IIntType) :=
| ILiteral (value:Z).

Inductive rvalue :=
| FunCallValue (name:string) (params:list rvalue)
| VarRValue (var:ivar)
| VHex (values: list string)
| FConst (type:IFloatType) (value:fconst type)
| IConst (type:IIntType) (value:iconst type)
| FConstArr (type:IFloatType) (values: list (fconst type))
| IConstArr (type:IIntType  ) (values: list (iconst type))
| FConstVec (type:IFloatType) (values: list (fconst type))
| IConstVec (type:IIntType  ) (values: list (iconst type))
| VdupRvalue (r:rvalue) (n:Z)
| NthRvalue (r:rvalue) (index:Z)
| RCast (type:IType) (r:rvalue)
| RDeref (r:rvalue).

Inductive lvalue :=
| VarLValue (var:ivar)
| NthLvalue (l:lvalue) (index:rvalue)
| LDeref (r:rvalue)
| LCast (type:IType) (l:lvalue).

Inductive istmt :=
| IFunction (name:string) (type:IType) (param: list ivar) (body:istmt)
| Skip
| Decl (vars: list ivar) (body:istmt)
| Chain (body: list istmt)
| Data (var:ivar) (values: list rvalue) (body:istmt)
| Assign (l:lvalue) (r:rvalue)
| Loop (var:ivar) (from:Z) (to:Z) (body:istmt)
| If (cond:rvalue) (thenbranch:istmt) (elsebranch:istmt)
| FunCallStmt (name:string) (params: list rvalue)
| Return (r:rvalue).

Inductive iprogram :=
| Program (bindings: ivar -> option IType) (body:istmt). (* TODO: map? *)
