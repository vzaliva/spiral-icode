%{
    open Ast
%}

%token <float> FLOAT
%token <int> INT
%token <string> STRING

%token COMMA
%token DOT TWODOT
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token DEF

%token LET VAR ALIGNED
%token V DECL CHAIN DATA ASSIGN LOOP FUNC NTH SKIP IF CRETURN EOF
%token TVOID TINT TREAL TDOUBLE TFLOAT TBOOL TPTR TVECT

%token <string> IDENTIFIER

%start <Ast.iprogram> i_program

%%

i_type:
  | TINT     { IntType    }
  | TREAL    { RealType   }
  | TFLOAT   { FloatType  }
  | TDOUBLE  { DoubleType }
  | TBOOL    { BoolType   }
  | TVOID    { VoidType   }
  | TPTR LPAREN t=i_type RPAREN DOT ALIGNED LPAREN LBRACKET a=separated_list(COMMA, INT) RBRACKET RPAREN {PtrType (t,a)}
  | TVECT LPAREN t=i_type COMMA s=INT RPAREN { VecType (t,s) }
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
  | V LPAREN f=FLOAT RPAREN { FConst f }
  ;

i_iconst:
  | V LPAREN i=INT RPAREN {IConst i}
  ;
  
i_rvalue:
  | n=IDENTIFIER LPAREN a=separated_list(COMMA, i_rvalue) RPAREN {FunCall (n,a)}
  | v=IDENTIFIER {VarRValue v}
  | f=i_fconst { f }
  | i=i_iconst { i }
  | NTH LPAREN a=i_rvalue COMMA i=i_rvalue RPAREN { NthRvalue (a,i) }
  ;

i_lvalue:
  | v=IDENTIFIER {VarLValue v}
  | NTH LPAREN a=i_lvalue COMMA i=i_rvalue RPAREN { NthLvalue (a,i) }
  ;

i_rvalue_comma: v=i_rvalue COMMA { v }
    
i_stmt:
  | SKIP {Skip}
  | FUNC LPAREN t=i_type COMMA n=STRING COMMA LBRACKET a=separated_list(COMMA, IDENTIFIER) RBRACKET COMMA b=i_stmt RPAREN {Function (n,t,a,b)}
  | DECL LPAREN LBRACKET a=separated_list(COMMA, IDENTIFIER) RBRACKET COMMA b=i_stmt RPAREN {Decl (a,b)}
  | CHAIN LPAREN c=separated_list(COMMA, i_stmt) RPAREN {Chain c}
  | DATA LPAREN n=IDENTIFIER COMMA v=list(i_rvalue_comma) b=i_stmt RPAREN {Data (n,v,b)}
  | ASSIGN LPAREN n=i_lvalue COMMA e=i_rvalue RPAREN {Assign (n,e)}
  | CRETURN LPAREN i=i_rvalue RPAREN { Return i }
  | IF LPAREN v=i_rvalue COMMA t=i_stmt COMMA e=i_stmt RPAREN { If (v,t,e) }
  | LOOP LPAREN v=IDENTIFIER COMMA LBRACKET f=INT TWODOT t=INT RBRACKET COMMA b=i_stmt RPAREN  { Loop (v,f,t,b) }
  ;
