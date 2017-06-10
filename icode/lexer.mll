{
  open Parser

  exception Error of string
}

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_'] 
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222'] 
let identchar =
    ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

rule main = parse

(* ignore whitespace *)
| [' ' '\t' '\r' '\n'] { main lexbuf }

(* numeric literals *)
| '-'?['0'-'9']*'.'['0'-'9']+ as f
    { FLOAT (float_of_string f) }
| ['0'-'9']+ as i
    { UINT (int_of_string i) }

(* special characters *)
| ','  { COMMA    }
| ".." { TWODOT   }
| "."  { DOT      }
| '('  { LPAREN   }
| ')'  { RPAREN   }
| '['  { LBRACKET }
| ']'  { RBRACKET }
| ":=" { DEF      }

(* some reserved words below *)    
| "decl"    { DECL    }
| "chain"   { CHAIN   }
| "ivenv"   { IVENV   }
| "data"    { DATA    }
| "assign"  { ASSIGN  }
| "loop"    { LOOP    }
| "func"    { FUNC    }
| "nth"     { NTH     }
| "V"       { V       }
| "skip"    { SKIP    }
| "if"      { IF      }
| "creturn" { CRETURN }
| "let"     { LET     }
| "var"     { VAR     }
| "aligned" { ALIGNED }
| "vparam"  { VPARAM  }
| "vhex"    { VHEX    }

(* type names *)
| "TInt"       { TINT    }
| "T_Int(8)"   { TINT8   }
| "T_Int(16)"  { TINT16  }
| "T_Int(32)"  { TINT32  }
| "T_Int(64)"  { TINT64  }
| "TUnt"       { TUINT    }
| "T_UInt(8)"  { TUINT8   }
| "T_UInt(16)" { TUINT16  }
| "T_UInt(32)" { TUINT32  }
| "T_UInt(64)" { TUINT64  }
| "TReal"      { TREAL   }
| "T_Real(32)" { TFLOAT  }
| "T_Real(64)" { TDOUBLE }
| "TBool"      { TBOOL   }
| "TPtr"       { TPTR    }
| "TVect"      { TVECT   }


(* Special functions *)
| "RealEPS" { REALEPS }
| "tcast"   { TCAST   }
| "deref"   { DEREF   }

(* string literals *)
| '"'
     { let buffer = Buffer.create 10 in
         STRING (stringl buffer lexbuf)
     }

| "/*"
    { comment lexbuf }
     
| ((lowercase | uppercase) (identchar*)) as i
    { IDENTIFIER i }
    
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
 | eof { raise End_of_file }
 | _ as char { Buffer.add_char buffer char; stringl buffer lexbuf }

and comment = parse
| "*/"
    { main lexbuf }
| eof
    { raise (Error "Unterminated comment") }
| _
    { comment lexbuf }
