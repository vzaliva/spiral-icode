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
| [' ' '\t' '\n'] { main lexbuf }

(* numeric literals *)
| ['0'-'9']*'.'['0'-'9']+ as f
    { FLOAT (float_of_string f) }
| ['0'-'9']+ as i
    { INT (int_of_string i) }

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

(* type names *)
| "TInt"       { TINT    }
| "TReal"      { TREAL   }
| "T_Real(32)" { TFLOAT  }
| "T_Real(64)" { TDOUBLE }
| "TBool"      { TBOOL   }
| "TPtr"       { TPTR    }
| "TVect"      { TVECT   }

(* string literals *)
| '"'
     { let buffer = Buffer.create 10 in
         STRING (stringl buffer lexbuf)
     }   
 
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
