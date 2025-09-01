(*
 * Syml:
 *  Compiler driver
 *)

open Lexer
open Parser

let () =
  let ls: lex_State = lex_new "main" "def main(): nil {let x: i32 = 3;;}" in
  let ps: parser_State = ps_new ls in
  let _: Ast.toplevel = parse_func ps in
  ()
