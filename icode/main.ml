open Typechecker
open Config
open Utils

open Format
open Core

open Getopt
open Getoptext

let prograname = "icodec" (* must match executable module name *)
and version = "0.1"

let input_files = ref []

(** Program return codes *)
let exitOK     = 0
let exitBadArg = 1
let exitErr    = 2

let specs =
  [
    ( 'v', "version", Some (fun _ -> Printf.printf "%s %s\n" prograname version ; exit 0), None,
      "Show program version");
    ( 'h', "help", Some usage_action, None,         "Show this help");
    ( 'd', "debug", (set Config.debug true), None,  "Debug");
    ( 'e', "stop-on-err", (set Config.stop_on_err true), None,  "Stop on first error");
    ( 'f', "float", (set Config.isDouble false), None,  "32-bit (4-byte) floating point mode");
    ( 'd', "double", (set Config.isDouble true), None, "64-bit (8-byte) floating point mode mode");
    ( '4', "32", (set Config.is64bit false), None,  "32-bit (4-byte) addressing");
    ( '8', "64", (set Config.is64bit true), None,   "64-bit (8-byte) addressing");
  ]

let parse_cmdline () =
  let add_input x = input_files := !input_files@[x] in
  try ext_parse_cmdline specs add_input print_usage_and_exit_action with
  | Getopt.Error s -> Printf.printf "Error:\n    %s\n" s; print_usage specs; exit exitBadArg

let _ =
  let process_file filename =
    msg "*** Compiling %s\n" filename ;
    let inBuffer = In_channel.create filename in
    let lineBuffer = Lexing.from_channel inBuffer in
    lineBuffer.Lexing.lex_curr_p <- { lineBuffer.Lexing.lex_curr_p with Lexing.pos_fname = filename };
    try
      let Ast.Program (valist, body) = Parser.i_program Lexer.main lineBuffer in
      let vmap = Typechecker.build_var_map valist in
      Typechecker.typecheck vmap body ;
      msg "*** %s compiled OK\n" filename;
      exitOK
    with
    | Typechecker.TypeError msg ->
       eprintf "Type check failed: %s%!\n" msg ;
       exitErr
    | Lexer.Error msg ->
       Format.eprintf "%a Syntax error (lexing error)\n" pr_pos (Lexing.lexeme_start_p lineBuffer);
       exitErr
    | Parser.Error ->
       Format.eprintf "%a Syntax error (parsing error)\n" pr_pos (Lexing.lexeme_start_p lineBuffer);
       exitErr
    | Syntaxerr.Error msg ->
       Format.eprintf "%a %s\n" pr_pos (Lexing.lexeme_start_p lineBuffer) msg ;
       exitErr
  in
  parse_cmdline ();
  let rec process_files l lastErr =
    match l with
    | [] -> lastErr
    | f::l' -> let rc = process_file f in
               if rc <> exitOK && !Config.stop_on_err then
                 let rls = List.length l' in
                 (if rls <> 0 then
                   msg "*** Compilation of file %s exited with code %d. The remaining %d will not be processed\n" f rc rls) ;
                 rc
               else
                 process_files l' rc
  in
  exit (process_files !input_files exitOK)
