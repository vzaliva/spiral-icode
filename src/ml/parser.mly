%{
    open Ast
    open Typetools
    open IIntType
    open IArithType
    open IFloatType
    open IType
    open Config
    open Ints
%}

%token <string> UINT
%token <string> STRING
%token <string> FP

%token MINUS COMMA
%token DOT TWODOT
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token DEF

%token LET VAR ALIGNED
%token VALUE DECL CHAIN IVENV PROGRAM DATA ASSIGN LOOP FUNC NTH SKIP IF CRETURN EOF
%token TVOID TREAL TDOUBLE TFLOAT TBOOL TPTR TVECT TARR
%token TINT TINT8 TINT16 TINT32 TINT64
%token TUINT TUINT8 TUINT16 TUINT32 TUINT64

%token REALEPS TCAST DEREF VPARAM VHEX VDUP

%token <string> IDENTIFIER

%start <Ast.iprogram> i_program

%%

i_ftype:        
  | TFLOAT   { FloatType  }
  | TDOUBLE  { DoubleType }
  | TREAL    { realFType () }
  ;

i_signed_itype:
  | TINT     { Int32Type  }
  | TINT8    { Int8Type   }
  | TINT16   { Int16Type  }
  | TINT32   { Int32Type  }
  | TINT64   { Int64Type  }
  ;

i_unsigned_itype:
  | TUINT    { UInt32Type }
  | TUINT8   { UInt8Type  }
  | TUINT16  { UInt16Type }
  | TUINT32  { UInt32Type }
  | TUINT64  { UInt64Type }
  ;

i_itype:
  | s = i_signed_itype { s }
  | u = i_unsigned_itype { u }
  ;

i_arith_type:
  | t=i_itype { I t }
  | t=i_ftype { F t }
  | TBOOL     { I BoolType } /* maybe needs to be in i_itype */
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
                    let a = if List.is_empty l then natural_alignment t else List.hd_exn l in
                    PtrType (t, a)
                }
  | TARR  LPAREN t=i_type COMMA s=UINT RPAREN { ArrType (t, int_of_string s) }
  | TVECT LPAREN t=i_arith_type COMMA s=UINT RPAREN
                {
                    let n = int_of_string s in
                    if is_power_of_2 n then VecType (t, n) 
                        else raise (Syntaxerr.Error ("Size of vector must be power of 2. Got: " ^ s))
                }
  ;

i_var:
   | i=IDENTIFIER DEF VAR LPAREN STRING COMMA t=i_type RPAREN COMMA { (i,t) }
   ;

i_fconst:
  | VALUE LPAREN t=i_ftype COMMA f=FP RPAREN { mkfconst $symbolstartpos $endpos
                                                 (FPLiteral (t,f)) }
  | REALEPS LPAREN TFLOAT RPAREN { mkfconst $symbolstartpos $endpos FloatEPS }
  | REALEPS LPAREN TDOUBLE RPAREN { mkfconst $symbolstartpos $endpos DoubleEPS }
  ;

i_uint:
  | x=UINT { x }

i_int:
  | x=UINT { x }
  | MINUS x = UINT { ("-" ^ x) }

i_iconst:
  | VALUE LPAREN t=i_unsigned_itype COMMA i=i_uint RPAREN
                {
                    match t with
                    | UInt8Type  -> mkiconst $symbolstartpos $endpos (UInt8Const  (Uint8Ex.of_string i))
                    | UInt16Type -> mkiconst $symbolstartpos $endpos (UInt16Const (Uint16Ex.of_string i))
                    | UInt32Type -> mkiconst $symbolstartpos $endpos (UInt32Const (Uint32Ex.of_string i))
                    | UInt64Type -> mkiconst $symbolstartpos $endpos (UInt64Const (Uint64Ex.of_string i))
                    | _ -> raise (Syntaxerr.Error "Internal error")
                }
  | VALUE LPAREN t=i_signed_itype COMMA i=i_int RPAREN
                {
                    match t with
                    | Int8Type  -> mkiconst $symbolstartpos $endpos (Int8Const  (Int8Ex.of_string i))
                    | Int16Type -> mkiconst $symbolstartpos $endpos (Int16Const (Int16Ex.of_string i))
                    | Int32Type -> mkiconst $symbolstartpos $endpos (Int32Const (Int32Ex.of_string i))
                    | Int64Type -> mkiconst $symbolstartpos $endpos (Int64Const (Int64Ex.of_string i))
                    | _ -> raise (Syntaxerr.Error "Internal error")
                }
  ;

/*i_uiconst:
  | VALUE LPAREN t=i_itype COMMA i=i_uint RPAREN { mkiconst $symbolstartpos $endpos
                                                  (ILiteral (t,i)) }
  ;
*/

i_vparamelem:
  | i=i_uint { mkiconst $symbolstartpos $endpos (UInt16Const (Uint16Ex.of_string i)) }
  ;

i_rvalue:
  | VALUE LPAREN
                TARR LPAREN t=i_ftype COMMA s=UINT RPAREN COMMA
                LBRACKET l=separated_nonempty_list(COMMA, i_fconst) RBRACKET RPAREN
                {
                    if (int_of_string s) <> (List.length l) then
                        raise (Syntaxerr.Error "list size mismatch")
                    else
                        mkrvalue $symbolstartpos $endpos (FConstArr (t,l))
                }
  | VALUE LPAREN
                TARR LPAREN t=i_itype COMMA s=UINT RPAREN COMMA
                LBRACKET l=separated_nonempty_list(COMMA, i_iconst) RBRACKET RPAREN
                {
                    if (int_of_string s) <> (List.length l) then
                        raise (Syntaxerr.Error "list size mismatch")
                    else
                        mkrvalue $symbolstartpos $endpos (IConstArr (t,l))
                }
  | VALUE LPAREN
                TVECT LPAREN t=i_ftype COMMA s=UINT RPAREN COMMA
                LBRACKET l=separated_nonempty_list(COMMA, i_fconst) RBRACKET RPAREN
                {
                    if (int_of_string s) <> (List.length l) then
                        raise (Syntaxerr.Error "list size mismatch")
                    else
                        mkrvalue $symbolstartpos $endpos (FConstVec (t,l))
                }
  | VALUE LPAREN
                TVECT LPAREN t=i_itype COMMA s=UINT RPAREN COMMA
                LBRACKET l=separated_nonempty_list(COMMA, i_iconst) RBRACKET RPAREN
                {
                    if (int_of_string s) <> (List.length l) then
                        raise (Syntaxerr.Error "list size mismatch")
                    else
                        mkrvalue $symbolstartpos $endpos (IConstVec (t,l))
                }
  | VPARAM LPAREN LBRACKET l=separated_nonempty_list(COMMA, i_vparamelem) RBRACKET RPAREN
                { mkrvalue $symbolstartpos $endpos (IConstArr (UInt16Type, l)) }
  | VHEX LPAREN LBRACKET l=separated_nonempty_list(COMMA, STRING) RBRACKET RPAREN
              { mkrvalue $symbolstartpos $endpos (VHex l) }
  | f=i_fconst { mkrvalue $symbolstartpos $endpos (FConst f) }
  | i=i_iconst { mkrvalue $symbolstartpos $endpos (IConst i) }
  | NTH  LPAREN a=i_rvalue COMMA i=i_rvalue RPAREN { mkrvalue $symbolstartpos $endpos (NthRvalue (a,i)) }
  | VDUP LPAREN a=i_rvalue COMMA i=i_iconst RPAREN { mkrvalue $symbolstartpos $endpos (VdupRvalue (a,i)) }
  | TCAST LPAREN t=i_type COMMA v=i_rvalue RPAREN { mkrvalue $symbolstartpos $endpos (RCast (t,v)) }
  | n=IDENTIFIER LPAREN a=separated_list(COMMA, i_rvalue) RPAREN
                {
                    mkrvalue $symbolstartpos $endpos (FunCallValue (n,a))
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
                    mkstmt $symbolstartpos $endpos (Loop (v,Int64Ex.of_string f,Int64Ex.of_string t,b))
                }
  | n=IDENTIFIER LPAREN a=separated_list(COMMA, i_rvalue) RPAREN
                {
                    mkstmt $symbolstartpos $endpos (FunCallStmt (n,a))
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
