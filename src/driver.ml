(*
 * Syml:
 *  Compiler driver
 *)

open Lexer
open Parser
open Code
open Il
open Ast
open X86
open Type

type comp_opt = {
    mutable log_il: bool;
    mutable log_ra: bool;
    mutable in_files: string list;
    target: Common.target_arch;
  }

let dostring (opts: comp_opt) (name: string) (s: string): unit =
  let ls: lex_State = lex_new name s in
  let ps: parser_State = ps_new ls in
  let ts: type_State = ts_new None in
  let smod: smod = smod_create name opts.target in
  let cs: code_State = cs_new smod in
  smod_open_out smod (name ^ ".s");
  while ps.peek != TK_EOF do
    let t: toplevel = parse_func ps in
    check_toplevel ts t;
    cs_toplevel cs t;
    emit_insts smod (cs_finish cs);
  done;
  emit_constants smod;
  smod_close_out smod
;;

let dofile (opts: comp_opt) (filename: string): unit =
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
  dostring opts (Filename.remove_extension filename) chunk_content
;;

let dofiles (opts: comp_opt) (files: string list): bool =
  let succ: bool ref = ref true in
  List.iter
    (fun f ->
      try dofile opts f
      with e ->
        Printf.eprintf "[Error]: at '%s': %s\n" f @@ Printexc.to_string e;
        succ := false;
    ) files;
  !succ
;;
