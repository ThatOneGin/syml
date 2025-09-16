(*
 * Syml:
 *  Compiler driver
 *)

open Common
open Lexer
open Parser
open Code
open Il
open Ast
open X86
open Type

let dostring (name: string) (s: string): unit =
  let ls: lex_State = lex_new name s in
  let ps: parser_State = ps_new ls in
  let ts: type_State = ts_new () in
  let t: toplevel = parse_func ps in
  check_func ts t;
  let smod: smod = Il.smod_create name Linux_X86_64 in
  let cs: code_State = cs_new smod in
  Il.smod_open_out smod (name ^ ".s");
  cs_toplevel cs t;
  cs_finish cs;
  emit_insts smod cs.code;
  emit_constants smod;
  Il.smod_close_out smod;
  ()
;;

let dofile (filename: string): unit =
  let chunk_content: string =
    let in_channel = open_in filename in
    try
      let content = really_input_string in_channel (in_channel_length in_channel) in
      close_in in_channel;
      content
    with _ ->
      close_in_noerr in_channel;
      ""
  in
  dostring "main" chunk_content;
  ()
;;

let () =
  dofile Sys.argv.(1)
;;
