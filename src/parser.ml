(*
 * Syml:
 *  Syml parser
 *)

open Lexer

(*
 * Parser functions and state
 *)

type parser_State =
  {mutable peek: token;
   mutable depth: int;
   ls: lex_State;}

exception Unexpected of string

let ps_new (ls: lex_State): parser_State = {
    peek = lex_next ls;
    depth = 0;
    ls = ls;
  }

let ps_next (ps: parser_State): unit =
  ps.peek <- lex_next ps.ls; ()

let ps_tk2str (ps: parser_State): string =
  match ps.peek with
  | TK_identifier s -> "'" ^ s ^ "'"
  | TK_number n -> "'" ^ n ^ "'"
  | TK_string s -> "'" ^ s ^ "'"
  | TK_let -> "<let>"
  | TK_def -> "<def>"
  | TK_return -> "<return>"
  | TK_asm -> "<asm>"
  | TK_nil -> "<nil>"
  | TK_int -> "<int>"
  | TK_str -> "<str>"
  | TK_i8 -> "<i8>"
  | TK_i16 -> "<i16>"
  | TK_i32 -> "<i32>"
  | TK_i64 -> "<i64>"
  | TK_plus -> "<+>"
  | TK_minus -> "<->"
  | TK_mul -> "<*>"
  | TK_div -> "</>"
  | TK_lparen -> "<(>"
  | TK_rparen -> "<)>"
  | TK_lbrace -> "<{>"
  | TK_rbrace -> "<}>"
  | TK_equals -> "<=>"
  | TK_colon -> "<:>"
  | TK_semicolon -> "<;>"
  | TK_EOF -> "<EOF>"

let ps_describe_token (ps: parser_State): string =
  Printf.sprintf "token %s" (ps_tk2str ps)

let ps_error (message: string) =
  (Unexpected message)

let ps_unexpected (ps: parser_State) (expected: string): 'a =
  raise (ps_error
    (Printf.sprintf "Unexpected %s, expected %s"
      (ps_describe_token ps) expected))

let ps_enter (ps: parser_State) =
  ps.depth <- ps.depth + 1; ()

let ps_leave (ps: parser_State) =
  ps.depth <- ps.depth - 1; ()

let peek (ps: parser_State): token =
  let t: token = ps.peek in
  ps_next ps; t

(*
 * This function should only be used
 * when expecting tokens whose constructors
 * don't expect any agument (e.g. reserved keywords and binary/unary operators)
 *)
let ps_expect_sym
      (ps: parser_State)
      (expect: token)
      (message: string): 'a =
  if ps.peek = expect then begin
    ps_next ps; ()
  end else
    ps_unexpected ps message

(* Expression parsing *)

let parse_name (ps: parser_State): Ast.expr =
  match ps.peek with
  | TK_identifier s -> Ast.Ident s
  | _ -> ps_unexpected ps "name"

let parse_name_as_string (ps: parser_State): string =
  let name = parse_name ps in
  ps_next ps;
  match name with
  | Ast.Ident s -> s
  | _ -> "" (* unreachable *)

let parse_number (ps: parser_State): Ast.expr =
  match ps.peek with
  | TK_number n -> Ast.Number (int_of_string n)
  | _ -> ps_unexpected ps "number"

let parse_string (ps: parser_State): Ast.expr =
  match ps.peek with
  | TK_string s -> Ast.String s
  | _ -> ps_unexpected ps "string"

let parse_primaryexpr (ps: parser_State): Ast.expr =
  let expr: Ast.expr option =
    match ps.peek with
    | TK_number _ -> Some (parse_number ps)
    | TK_identifier _ -> Some (parse_name ps)
    | TK_string _ -> Some (parse_string ps)
    | _ -> None
  in
  match expr with
  | Some e -> ps_next ps; e
  | None -> ps_unexpected ps "string, number or valid identifier"

let parse_expr (ps: parser_State) = parse_primaryexpr ps

(* Statement parsing *)

(* type = nil | str | i8 | i16 | i32 | i64 *)
let parse_type (ps: parser_State): Dtypes.datatype =
  match ps.peek with
  | TK_nil -> ps_next ps; Dtypes.Nil
  | TK_int -> ps_next ps; Dtypes.Int
  | TK_str -> ps_next ps; Dtypes.Str
  | TK_i8 ->  ps_next ps; Dtypes.I8
  | TK_i16 -> ps_next ps; Dtypes.I16
  | TK_i32 -> ps_next ps; Dtypes.I32
  | TK_i64 -> ps_next ps; Dtypes.I64
  | _ -> ps_unexpected ps "valid type specifier."

(* var = 'let' ident '=' expr *)
let parse_var (ps: parser_State): Ast.stat =
  match ps.peek with
  | TK_let ->
    ps_next ps;
    let name: string =
      match ps.peek with
      | TK_identifier s -> ps_next ps; s
      | _ -> ps_unexpected ps "<identifier> token"
    in
    let ty: Dtypes.datatype =
      match ps.peek with
      | TK_colon -> ps_next ps; parse_type ps
      | _ -> Dtypes.Int
    in
    let value: Ast.expr = 
      match ps.peek with
      | TK_equals -> ps_next ps; parse_expr ps
      | _ -> ps_unexpected ps "<=> token"
    in
    let vard: Ast.vard = {
       name = name;
       value = value;
       ty = ty
      } in
    Var vard
  | _ -> ps_unexpected ps "'let' token"

(* return = 'return' expr *)
let parse_return (ps: parser_State): Ast.stat =
  match ps.peek with
  | TK_return ->
    ps_next ps;
    let e: Ast.expr = parse_expr ps in
    Return e
  | _ -> ps_unexpected ps "'return' token"

(* asm = 'asm' string *)
let parse_asm (ps: parser_State): Ast.stat =
  ps_expect_sym ps TK_asm "";
  match ps.peek with
  | TK_string s -> ps_next ps; Ast.Asm s;
  | _ -> ps_unexpected ps "string"

let parse_voidcall (ps: parser_State): Ast.stat =
  match ps.peek with
  | TK_identifier name ->
    ps_next ps;
    ps_expect_sym ps TK_lparen "Expected '(' in function call.";
    ps_expect_sym ps TK_rparen "Expected ')' in argument list.";
    Ast.Voidcall {
      name = name;
      args = [||];
    }
  | _ -> ps_unexpected ps "statement"

(* stat = var | return | asm *)
let parse_stat (ps: parser_State): Ast.stat =
  let stat: Ast.stat = 
    match ps.peek with
    | TK_let -> parse_var ps
    | TK_return -> parse_return ps
    | TK_asm -> parse_asm ps
    | _ -> parse_voidcall ps
  in
  while ps.peek = TK_semicolon do
    ps_next ps;
  done;
  stat

(* block = '{' stat list '}' *)
let parse_block (ps: parser_State): Ast.block =
  let blk: Ast.block = {body=[||]} in
  ps_expect_sym ps TK_lbrace "expected '{'";
  ps_enter ps; (* enter level *)
  while ps.peek != TK_rbrace  && ps.peek != TK_EOF do
    Ast.block_append blk (parse_stat ps);
  done;
  ps_leave ps; (* leave level *)
  ps_expect_sym ps TK_rbrace "expected '}'";
  blk

(* func = 'def' '(' ')' '{' stat list '}' *)
let parse_func (ps: parser_State): Ast.toplevel =
  match ps.peek with
  | TK_def ->
    ps_next ps;
    let name: string = parse_name_as_string ps in
    ps_expect_sym ps TK_lparen "expected '('";
    (* TODO: function params *)
    ps_expect_sym ps TK_rparen "expected ')'";
    let ty: Dtypes.datatype =
      match ps.peek with
      | TK_colon -> ps_next ps; parse_type ps
      | _ -> Dtypes.Int
    in
    let fdesc: Ast.funct = {
        name = name;
        ty = ty;
        blk = parse_block ps;
      } in
    Ast.Func fdesc
  | _ -> ps_unexpected ps "'def' token"
