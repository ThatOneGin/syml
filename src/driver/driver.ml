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
open Type
open Typeutils
open Resolve

let dostring (opts: Comp_state.t) (name: string) (s: string): unit =
  let _mod: ast_mod = parse name s in
  let ts: type_State = ts_new None in
  let rs: resolve_State = rs_new () in
  let smod: smod = smod_create name opts.target in
  let cs: code_State = cs_new opts smod in

  rs_resolve_module rs _mod;
  ts_check_module ts _mod;
  cs_emit_module cs _mod
;;

let dofile (opts: Comp_state.t) (filename: string): unit =
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

let errprint (succ: bool ref) fmt: 'a =
  succ := false;
  Printf.ksprintf (fun s ->
    print_string "error: ";
    print_endline s) fmt
;;

let dofiles (opts: Comp_state.t) (files: string list): bool =
  let succ: bool ref = ref true in
  List.iter
    (fun f ->
      try dofile opts f
      with
        | Lexical_error (loc, msg) -> errprint succ "%s: %s" (location2str loc) msg
        | Parsing_error (loc, msg) -> errprint succ "%s: %s" (location2str loc) msg
        | Common_error msg -> errprint succ "%s" msg
        | Unreachable_error (what, where) -> errprint succ "unreachable state at %s: %s" what where
        | Todo_error what -> errprint succ "%s is not implemented" what
        | _ as e -> raise e
    ) files;
  !succ
;;
