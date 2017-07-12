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
  | TINT     { intAType ()  }
  | TINT8    { I Int8Type   }
  | TINT16   { I Int16Type  }
  | TINT32   { I Int32Type  }
  | TINT64   { I Int64Type  }
  | TUINT    { uIntAType () }
  | TUINT8   { I UInt8Type  }
  | TUINT16  { I UInt16Type }
  | TUINT32  { I UInt32Type }
  | TUINT64  { I UInt64Type }
  | TREAL    { realAType () }
  | TFLOAT   { FloatType   }
  | TDOUBLE  { DoubleType  }
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
  | V LPAREN f=FLOAT RPAREN { FPLiteral f }
  | REALEPS LPAREN TFLOAT RPAREN { FloatEPS }
  | REALEPS LPAREN TDOUBLE RPAREN { DoubleEPS }
  ;

i_int:
  | x=UINT { Int_or_uint_64.U64 (Uint64.of_string x) }
  | MINUS x = UINT { Int_or_uint_64.I64 (Int64.of_string ("-" ^ x)) }

i_iconst:
  | V LPAREN i=i_int RPAREN { i }
  ;

i_rvalue:
  | V LPAREN LBRACKET l=separated_nonempty_list(COMMA, i_fconst) RBRACKET RPAREN
              { FConstArr l }
  | V LPAREN LBRACKET l=separated_nonempty_list(COMMA, i_iconst) RBRACKET RPAREN
              { IConstArr l }
  | VPARAM LPAREN LBRACKET l=separated_nonempty_list(COMMA, UINT) RBRACKET RPAREN
              {
                  VParam (VParamList (List.map int_of_string l))
              }
  | VHEX LPAREN LBRACKET l=separated_nonempty_list(COMMA, STRING) RBRACKET RPAREN
              {
                  VHex l
              }
  | f=i_fconst { FConst f }
  | i=i_iconst { IConst i }
  | NTH  LPAREN a=i_rvalue COMMA i=i_rvalue RPAREN { NthRvalue (a,i) }
  | VDUP LPAREN a=i_rvalue COMMA i=i_iconst RPAREN { VdupRvalue (a,i) }
  | TCAST LPAREN t=i_type COMMA v=i_rvalue RPAREN { RCast (t,v) }
  | n=IDENTIFIER LPAREN a=separated_list(COMMA, i_rvalue) RPAREN {FunCall (n,a)}
  | v=IDENTIFIER {VarRValue v}
  | DEREF LPAREN v=i_rvalue RPAREN { RDeref v }
  ;

i_lvalue:
  | v=IDENTIFIER {VarLValue v}
  | NTH LPAREN a=i_lvalue COMMA i=i_rvalue RPAREN { NthLvalue (a,i) }
  | DEREF LPAREN v=i_rvalue RPAREN { LDeref v }
  | TCAST LPAREN t=i_type COMMA v=i_lvalue RPAREN { LCast (t,v) }
  ;

i_rvalue_comma: v=i_rvalue COMMA { v }

i_chain_kw:
        | CHAIN  {}
        | IVENV  {}

i_func:
  | FUNC LPAREN t=i_type COMMA n=STRING COMMA LBRACKET a=separated_list(COMMA, IDENTIFIER) RBRACKET COMMA b=i_stmt RPAREN {Function (n,t,a,b)}

i_stmt:
  | SKIP {Skip}
  | f=i_func {f}
  | DECL LPAREN LBRACKET a=separated_list(COMMA, IDENTIFIER) RBRACKET COMMA b=i_stmt RPAREN {Decl (a,b)}
  | i_chain_kw LPAREN c=separated_list(COMMA, i_stmt) RPAREN {Chain c}
  | DATA LPAREN n=IDENTIFIER COMMA v=list(i_rvalue_comma) b=i_stmt RPAREN {Data (n,v,b)}
  | ASSIGN LPAREN n=i_lvalue COMMA e=i_rvalue RPAREN {Assign (n,e)}
  | CRETURN LPAREN i=i_rvalue RPAREN { Return i }
  | IF LPAREN v=i_rvalue COMMA t=i_stmt COMMA e=i_stmt RPAREN { If (v,t,e) }
  | LOOP LPAREN v=IDENTIFIER COMMA LBRACKET f=i_int TWODOT t=i_int RBRACKET COMMA b=i_stmt RPAREN
                {
                    Loop (v,f,t,b)
                }
  ;

/* Top level: definitions single 'func' or 'program' of func */
i_top_defs:
  | f=i_func {Chain [f]}
  | PROGRAM LPAREN c=separated_list(COMMA, i_stmt) RPAREN {Chain c}

i_program:
    | LET LPAREN v=list(i_var) b=i_top_defs RPAREN EOF
      { Program (v,b) }
    | f=i_top_defs EOF { Program ([], f) }
    | EOF { Program ([], Skip) }
    ;

