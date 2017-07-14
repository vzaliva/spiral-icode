open Typechecker
open Config

open Format
open Core

open Getopt
open Getoptext

let prograname = "icodec" (* must match executable module name *)
and version = "0.1"

let input_files = ref []

let specs =
  [
    ( 'v', "version", Some (fun _ -> Printf.printf "%s %s\n" prograname version ; exit 0), None,
      "Show program version");
    ( 'h', "help", Some usage_action, None,
      "Show this help");
    ( 'd', "debug", (set Config.debug true), None,
      "Debug");
    ( '4', "32", (set Config.is64bit false), None,
      "32-bit (4-byte) mode");
    ( '8', "64", (set Config.is64bit true), None,
      "64-bit (8-byte) mode");
  ]

let parse_cmdline () =
  let add_input x = input_files := !input_files@[x] in
  try ext_parse_cmdline specs add_input print_usage_and_exit_action with
  | Getopt.Error s -> Printf.printf "Error:\n    %s\n" s; print_usage specs; exit 1

let _ =
  let process_file filename =
    let inBuffer = In_channel.create filename in
    let lineBuffer = Lexing.from_channel inBuffer in
    lineBuffer.Lexing.lex_curr_p <- { lineBuffer.Lexing.lex_curr_p with Lexing.pos_fname = filename };
    try
      let Ast.Program (valist, body) = Parser.i_program Lexer.main lineBuffer in
      let vmap = Typechecker.build_var_map valist in
      Typechecker.typecheck vmap body ;
      msg "*** OK\n"
    with
    | Typechecker.TypeError msg -> eprintf "Type check failed: %s%!\n" msg
    | Lexer.Error msg -> eprintf "Lexer error %s%!\n" msg
    | Parser.Error -> eprintf "Parsing error at offset %d: syntax error.\n%!" (Lexing.lexeme_start lineBuffer)
  in
  parse_cmdline ();
  List.map ~f:process_file !input_files
