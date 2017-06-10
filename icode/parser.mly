%{
    open Ast
    open IType
%}

%token <float> FLOAT
%token <int> UINT
%token <string> STRING

%token MINUS COMMA
%token DOT TWODOT
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token DEF

%token LET VAR ALIGNED
%token V DECL CHAIN IVENV DATA ASSIGN LOOP FUNC NTH SKIP IF CRETURN EOF
%token TVOID TREAL TDOUBLE TFLOAT TBOOL TPTR TVECT
%token TINT TINT8 TINT16 TINT32 TINT64
%token TUINT TUINT8 TUINT16 TUINT32 TUINT64

%token REALEPS TCAST DEREF VPARAM VHEX

%token <string> IDENTIFIER

%start <Ast.iprogram> i_program

%%

i_type:
  | TINT     { IntType    }
  | TINT8    { Int8Type   }
  | TINT16   { Int16Type  }
  | TINT32   { Int32Type  }
  | TINT64   { Int64Type  }
  | TUINT    { UIntType   }
  | TUINT8   { UInt8Type  }
  | TUINT16  { UInt16Type }
  | TUINT32  { UInt32Type }
  | TUINT64  { UInt64Type }
  | TREAL    { RealType   }
  | TFLOAT   { FloatType  }
  | TDOUBLE  { DoubleType }
  | TBOOL    { BoolType   }
  | TVOID    { VoidType   }
  | TPTR LPAREN t=i_type RPAREN DOT ALIGNED LPAREN LBRACKET a=separated_list(COMMA, UINT) RBRACKET RPAREN {PtrType (t,a)}
  | TVECT LPAREN t=i_type COMMA s=UINT RPAREN { VecType (t,s) }
  | n=IDENTIFIER { OtherType n }
  ;

i_var:
   | i=IDENTIFIER DEF VAR LPAREN STRING COMMA t=i_type RPAREN COMMA { (i,t) }
   ;

i_program:
    | LET LPAREN v=list(i_var) b=i_stmt RPAREN EOF
      { Program (v,b) }
    | f=i_stmt EOF { Program ([], f) }
    | EOF { Program ([], Skip) }
    ;

i_fconst:
  | V LPAREN f=FLOAT RPAREN { FPLiteral f }
  | REALEPS LPAREN TFLOAT RPAREN { FloatEPS }
  | REALEPS LPAREN TDOUBLE RPAREN { DoubleEPS }
  ;

i_int:
  | x=UINT { x }
  | MINUS x=UINT { -x }

i_iconst:
  | V LPAREN i=i_int RPAREN { i }
  ;

i_rvalue:
  | V LPAREN LBRACKET l=separated_nonempty_list(COMMA, i_fconst) RBRACKET RPAREN
              { FConstVec l }
  | V LPAREN LBRACKET l=separated_nonempty_list(COMMA, i_iconst) RBRACKET RPAREN
              { IConstVec l }
  | VPARAM LPAREN LBRACKET l=separated_nonempty_list(COMMA, UINT) RBRACKET RPAREN
              { VParam (VParamList l) }
  | VHEX LPAREN LBRACKET l=separated_nonempty_list(COMMA, STRING) RBRACKET RPAREN
              { IConstVec [] } (* TODO *)             
  | f=i_fconst { FConst f }
  | i=i_iconst { IConst i }
  | NTH LPAREN a=i_rvalue COMMA i=i_rvalue RPAREN { NthRvalue (a,i) }
  | TCAST LPAREN t=i_type COMMA v=i_rvalue RPAREN { RCast (t,v) }
  | n=IDENTIFIER LPAREN a=separated_list(COMMA, i_rvalue) RPAREN {FunCall (n,a)}
  | v=IDENTIFIER {VarRValue v}
  | DEREF LPAREN v=i_rvalue RPAREN { RDeref v }
  ;

i_lvalue:
  | v=IDENTIFIER {VarLValue v}
  | NTH LPAREN a=i_lvalue COMMA i=i_rvalue RPAREN { NthLvalue (a,i) }
  | DEREF LPAREN v=i_lvalue RPAREN { LDeref v }
  | TCAST LPAREN t=i_type COMMA v=i_lvalue RPAREN { LCast (t,v) }
  ;

i_rvalue_comma: v=i_rvalue COMMA { v }

i_chain_kw:
           | CHAIN  {}
           | IVENV  {}

i_stmt:
  | SKIP {Skip}
  | FUNC LPAREN t=i_type COMMA n=STRING COMMA LBRACKET a=separated_list(COMMA, IDENTIFIER) RBRACKET COMMA b=i_stmt RPAREN {Function (n,t,a,b)}
  | DECL LPAREN LBRACKET a=separated_list(COMMA, IDENTIFIER) RBRACKET COMMA b=i_stmt RPAREN {Decl (a,b)}
  | i_chain_kw LPAREN c=separated_list(COMMA, i_stmt) RPAREN {Chain c}
  | DATA LPAREN n=IDENTIFIER COMMA v=list(i_rvalue_comma) b=i_stmt RPAREN {Data (n,v,b)}
  | ASSIGN LPAREN n=i_lvalue COMMA e=i_rvalue RPAREN {Assign (n,e)}
  | CRETURN LPAREN i=i_rvalue RPAREN { Return i }
  | IF LPAREN v=i_rvalue COMMA t=i_stmt COMMA e=i_stmt RPAREN { If (v,t,e) }
  | LOOP LPAREN v=IDENTIFIER COMMA LBRACKET f=i_int TWODOT t=i_int RBRACKET COMMA b=i_stmt RPAREN  { Loop (v,f,t,b) }
  ;
