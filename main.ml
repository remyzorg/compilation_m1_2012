(* Programme principal *)

open Format
open Lexing
open Lexer
open Parser
open Ast
open Typing

let usage = "usage: minic [options] file.c"

let parse_only = ref false
let type_only = ref false

let spec =
  ["-parse-only", Arg.Set parse_only, "  stops after parsing";
   "-type-only", Arg.Set type_only, "  stops after typing"
]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".c") then
      raise (Arg.Bad "no .c extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let report_loc (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let p = Parser.file Lexer.token lb in
    close_in c;
    if !parse_only then exit 0;
    let p = Typing.type_file p in
    if !type_only then exit 0;
    let p = Rename.rename_file p in 
    let c = Compile.compile_file p in
    let output_file = (Filename.chop_suffix file ".c") ^ ".s" in
    let out = open_out output_file in
    let outf = formatter_of_out_channel out in
        Mips.print_program outf c;
        pp_print_flush outf ();
        close_out out
  with
    | Lexical_error s ->
	report_loc (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "lexical error: %s\n@." s;
	exit 1
    | Parsing.Parse_error ->
	report_loc (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "Syntax error\n@.";
	exit 1
    | Typing.Error (err, loc)-> 
	      report_loc loc;
        Typing.print_error err;
	      exit 1
    | Typing.No_main_error ->
        eprintf "File \"%s\" : \n" file; 
        eprintf "Error : 'main' function doesn't exist or is not well-formed@.";
        exit 1
    | e ->
   eprintf "Anomaly: %s\n@." (Printexc.to_string e);
   exit 2
