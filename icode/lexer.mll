{
    open Parser
    exception Error of string

    let next_line lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
    { pos with
      Lexing.pos_bol  = pos.Lexing.pos_cnum;
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1
    }

    let keyword_table = Hashtbl.create 53
    let _ =  List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [
        "decl"             , DECL ;
        "chain"            , CHAIN ;
        "program"          , PROGRAM ;
        "ivenv"            , IVENV ;
        "data"             , DATA ;
        "assign"           , ASSIGN ;
        "loop"             , LOOP ;
        "func"             , FUNC ;
        "nth"              , NTH ;
        "Value"            , VALUE ;
        "skip"             , SKIP ;
        "if"               , IF ;
        "creturn"          , CRETURN ;
        "let"              , LET ;
        "var"              , VAR ;
        "aligned"          , ALIGNED ;
        "vparam"           , VPARAM ;
        "vhex"             , VHEX ;
        "RealEPS"          , REALEPS ;
        "tcast"            , TCAST ;
        "deref"            , DEREF ;
        "vdup"             , VDUP ;
        "vstore_2l_4x32f"  , VSTORE_2L_4X32F ;
        "vstore_2h_4x32f"  , VSTORE_2H_4X32F ;
        "vstoreu_4x32f"    , VSTOREU_4X32F
    ]
    }

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar =
['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule main = parse

| white { main lexbuf } (* ignore whitespace *)
| newline {  next_line lexbuf; main lexbuf } (* count lines *)

(* numeric literals *)
| '-'?['0'-'9']*'.'['0'-'9']+ as f
{ FLOAT (float_of_string f) }
| ['0'-'9']+ as i
{ UINT (i) }

(* special characters *)
| ','  { COMMA    }
| ".." { TWODOT   }
| "."  { DOT      }
| '('  { LPAREN   }
| ')'  { RPAREN   }
| '['  { LBRACKET }
| ']'  { RBRACKET }
| ":=" { DEF      }


(* type names *)
| "TVoid"      { TVOID   }
| "TInt"       { TINT    }
| "T_Int(8)"   { TINT8   }
| "T_Int(16)"  { TINT16  }
| "T_Int(32)"  { TINT32  }
| "T_Int(64)"  { TINT64  }
| "TUnt"       { TUINT   }
| "T_UInt(8)"  { TUINT8  }
| "T_UInt(16)" { TUINT16 }
| "T_UInt(32)" { TUINT32 }
| "T_UInt(64)" { TUINT64 }
| "TReal"      { TREAL   }
| "T_Real(32)" { TFLOAT  }
| "T_Real(64)" { TDOUBLE }
| "TBool"      { TBOOL   }
| "TPtr"       { TPTR    }
| "TVect"      { TVECT   }
| "TArray"     { TARR    }

(* string literals *)
| '"'
{ let buffer = Buffer.create 10 in
    STRING (stringl buffer lexbuf)
    }

| "/*"
{ comment lexbuf }

| ((lowercase | uppercase) (identchar*)) as i
{
    try Hashtbl.find keyword_table i
    with Not_found -> IDENTIFIER i
    }

| eof
{ EOF }
| _
{ raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

and stringl buffer = parse
| '"' { Buffer.contents buffer }
    | "\\t" { Buffer.add_char buffer '\t'; stringl buffer lexbuf }
    | "\\n" { Buffer.add_char buffer '\n'; stringl buffer lexbuf }
    | '\\' '"' { Buffer.add_char buffer '"'; stringl buffer lexbuf }
    | '\\' '\\' { Buffer.add_char buffer '\\'; stringl buffer lexbuf }
    | newline {  next_line lexbuf; stringl buffer lexbuf }
    | eof { raise End_of_file }
    | _ as char { Buffer.add_char buffer char; stringl buffer lexbuf }

    and comment = parse
| "*/"
    { main lexbuf }
        | eof
        { raise (Error "Unterminated comment") }
        | newline {  next_line lexbuf; comment lexbuf }
        | _
        { comment lexbuf }
