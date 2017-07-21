%{
    open Ast
    open IIntType
    open IArithType
    open IType
    open Config

    open Uint
%}

%token <float> FLOAT
%token <string> UINT
%token <string> STRING

%token MINUS COMMA
%token DOT TWODOT
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token DEF

%token LET VAR ALIGNED
%token V DECL CHAIN IVENV PROGRAM DATA ASSIGN LOOP FUNC NTH SKIP IF CRETURN EOF
%token TVOID TREAL TDOUBLE TFLOAT TBOOL TPTR TVECT TARR
%token TINT TINT8 TINT16 TINT32 TINT64
%token TUINT TUINT8 TUINT16 TUINT32 TUINT64

%token REALEPS TCAST DEREF VPARAM VHEX VDUP

%token <string> IDENTIFIER

%start <Ast.iprogram> i_program

%%

i_arith_type:
  | TINT     { I Int32Type  }
  | TINT8    { I Int8Type   }
  | TINT16   { I Int16Type  }
  | TINT32   { I Int32Type  }
  | TINT64   { I Int64Type  }
  | TUINT    { I UInt32Type }
  | TUINT8   { I UInt8Type  }
  | TUINT16  { I UInt16Type }
  | TUINT32  { I UInt32Type }
  | TUINT64  { I UInt64Type }
  | TREAL    { realAType () }
  | TFLOAT   { F FloatType  }
  | TDOUBLE  { F DoubleType }
  | TBOOL    { I BoolType   }
  ;

i_type:
  | a=i_arith_type { A a }
  | TVOID    { VoidType   }
  | TPTR LPAREN t=i_type RPAREN DOT ALIGNED LPAREN LBRACKET a=separated_list(COMMA, UINT) RBRACKET RPAREN
                {
                    let l = List.map int_of_string a in
                    (* Per our discussion with Franz on 2017-06-19 we will ignore 2nd
                     element of alignment in Spiral and will use only first one 
                     *)
                    let open Core in
                    let a = if List.is_empty l then None else Some (List.hd_exn l) in
                    PtrType (t, a)
                }
  | TARR  LPAREN t=i_type COMMA s=UINT RPAREN { ArrType (t, int_of_string s) }
  | TVECT LPAREN t=i_arith_type COMMA s=UINT RPAREN { VecType (t, int_of_string s) }
  ;

i_var:
   | i=IDENTIFIER DEF VAR LPAREN STRING COMMA t=i_type RPAREN COMMA { (i,t) }
   ;

i_fconst:
  | V LPAREN f=FLOAT RPAREN { mkfconst $symbolstartpos $endpos (FPLiteral f) }
  | REALEPS LPAREN TFLOAT RPAREN { mkfconst $symbolstartpos $endpos FloatEPS }
  | REALEPS LPAREN TDOUBLE RPAREN { mkfconst $symbolstartpos $endpos DoubleEPS }
  ;

i_uint:
  | x=UINT { Int_or_uint_64.U64 (Uint64.of_string x) }

i_int:
  | x=UINT { Int_or_uint_64.U64 (Uint64.of_string x) }
  | MINUS x = UINT { Int_or_uint_64.I64 (Int64.of_string ("-" ^ x)) }

i_iconst:
  | V LPAREN i=i_int RPAREN { i }
  ;

i_rvalue:
  | V LPAREN LBRACKET l=separated_nonempty_list(COMMA, i_fconst) RBRACKET RPAREN
              { mkrvalue $symbolstartpos $endpos (FConstArr l) }
  | V LPAREN LBRACKET l=separated_nonempty_list(COMMA, i_iconst) RBRACKET RPAREN
              { mkrvalue $symbolstartpos $endpos (IConstArr l) }
  | VPARAM LPAREN LBRACKET l=separated_nonempty_list(COMMA, i_uint) RBRACKET RPAREN
              { mkrvalue $symbolstartpos $endpos (IConstArr l) }
  | VHEX LPAREN LBRACKET l=separated_nonempty_list(COMMA, STRING) RBRACKET RPAREN
              { mkrvalue $symbolstartpos $endpos (VHex l) }
  | f=i_fconst { mkrvalue $symbolstartpos $endpos (FConst f) }
  | i=i_iconst { mkrvalue $symbolstartpos $endpos (IConst i) }
  | NTH  LPAREN a=i_rvalue COMMA i=i_rvalue RPAREN { mkrvalue $symbolstartpos $endpos (NthRvalue (a,i)) }
  | VDUP LPAREN a=i_rvalue COMMA i=i_iconst RPAREN { mkrvalue $symbolstartpos $endpos (VdupRvalue (a,i)) }
  | TCAST LPAREN t=i_type COMMA v=i_rvalue RPAREN { mkrvalue $symbolstartpos $endpos (RCast (t,v)) }
  | n=IDENTIFIER LPAREN a=separated_list(COMMA, i_rvalue) RPAREN
                {
                    mkrvalue $symbolstartpos $endpos (FunCall (n,a))
                }
  | v=IDENTIFIER { mkrvalue $symbolstartpos $endpos (VarRValue v) }
  | DEREF LPAREN v=i_rvalue RPAREN { mkrvalue $symbolstartpos $endpos (RDeref v) }
  ;

i_lvalue:
    | v=IDENTIFIER {mklvalue $symbolstartpos $endpos (VarLValue v) }
    | NTH LPAREN a=i_lvalue COMMA i=i_rvalue RPAREN { mklvalue $symbolstartpos $endpos (NthLvalue (a,i)) }
    | DEREF LPAREN v=i_rvalue RPAREN { mklvalue $symbolstartpos $endpos (LDeref v) }
    | TCAST LPAREN t=i_type COMMA v=i_lvalue RPAREN { mklvalue $symbolstartpos $endpos (LCast (t,v)) }
  ;

i_rvalue_comma: v=i_rvalue COMMA { v }

i_chain_kw:
        | CHAIN  {}
        | IVENV  {}

i_func:
  | FUNC LPAREN t=i_type COMMA n=STRING COMMA LBRACKET a=separated_list(COMMA, IDENTIFIER) RBRACKET COMMA b=i_stmt RPAREN { mkstmt $symbolstartpos $endpos (Function (n,t,a,b)) }

i_stmt:
  | SKIP { mkstmt $symbolstartpos $endpos Skip}
  | f=i_func {f}
  | DECL LPAREN LBRACKET a=separated_list(COMMA, IDENTIFIER) RBRACKET COMMA b=i_stmt RPAREN { mkstmt $symbolstartpos $endpos (Decl (a,b)) }
  | i_chain_kw LPAREN c=separated_list(COMMA, i_stmt) RPAREN { mkstmt $symbolstartpos $endpos (Chain c)}
  | DATA LPAREN n=IDENTIFIER COMMA v=list(i_rvalue_comma) b=i_stmt RPAREN { mkstmt $symbolstartpos $endpos (Data (n,v,b)) }
  | ASSIGN LPAREN n=i_lvalue COMMA e=i_rvalue RPAREN { mkstmt $symbolstartpos $endpos (Assign (n,e)) }
  | CRETURN LPAREN i=i_rvalue RPAREN { mkstmt $symbolstartpos $endpos (Return i) }
  | IF LPAREN v=i_rvalue COMMA t=i_stmt COMMA e=i_stmt RPAREN { mkstmt $symbolstartpos $endpos (If (v,t,e)) }
  | LOOP LPAREN v=IDENTIFIER COMMA LBRACKET f=i_int TWODOT t=i_int RBRACKET COMMA b=i_stmt RPAREN
                {
                    mkstmt $symbolstartpos $endpos (Loop (v,f,t,b))
                }
  ;

/* Top level: definitions single 'func' or 'program' of func */
i_top_defs:
  | f=i_func { mkstmt $symbolstartpos $endpos (Chain [f])}
  | PROGRAM LPAREN c=separated_list(COMMA, i_stmt) RPAREN { mkstmt $symbolstartpos $endpos (Chain c)}

i_program:
  | LET LPAREN v=list(i_var) b=i_top_defs RPAREN EOF
                { Program (v,b) }
  | f=i_top_defs EOF { Program ([], f) }
  | EOF { Program ([], mkstmt $symbolstartpos $endpos Skip) }
  ;
