open Typechecker
open Format
open Core

(* TODO: argument parsing *)
let filename = Sys.argv.(1)

let () =
    let inBuffer = In_channel.create filename in
    let lineBuffer = Lexing.from_channel inBuffer in
    try
      let Ast.Program (valist, body) = Parser.i_program Lexer.main lineBuffer in
      let vmap = Typechecker.build_var_map valist in
      Typechecker.typecheck vmap body ;
      Printf.fprintf stderr "OK\n"
    with
        | Typechecker.TypeError msg -> Printf.fprintf stderr "Type check failed: %s%!\n" msg
        | Lexer.Error msg -> Printf.fprintf stderr "Lexer error %s%!\n" msg
        | Parser.Error -> Printf.fprintf stderr "Parsing error at offset %d: syntax error.\n%!" (Lexing.lexeme_start lineBuffer)
