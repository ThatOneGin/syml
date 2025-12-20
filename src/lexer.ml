(*
 * Syml:
 *  Lexical analyzer
 *)

open Common

type token =
(* terminal symbols *)
  | TK_identifier of string
  | TK_number of string
  | TK_string of string
(* reserved keywords *)
  | TK_let
  | TK_def
  | TK_return
  | TK_asm
  | TK_if
  | TK_while
(* types *)
  | TK_nil (* nil is also a keyword *)
  | TK_int
  | TK_str
  | TK_i8
  | TK_i16
  | TK_i32
  | TK_i64
(* operators *)
  | TK_plus
  | TK_minus
  | TK_mul
  | TK_div
  | TK_equ
  | TK_neq
  | TK_lparen | TK_rparen
  | TK_lbrace | TK_rbrace
  | TK_equals | TK_colon
  | TK_semicolon
  | TK_comma
  | TK_EOF

type lex_State = {
    name: string;
    src: string;
    len: int;
    mutable current: char;
    mutable pos: int;
    mutable line: int;
  }

let reserved_table = [
    ("let", TK_let);
    ("def", TK_def);
    ("return", TK_return);
    ("asm", TK_asm);
    ("if", TK_if);
    ("while", TK_while);
    ("nil", TK_nil);
    ("int", TK_int);
    ("str", TK_str);
    ("i8", TK_i8);
    ("i16", TK_i16);
    ("i32", TK_i32);
    ("i64", TK_i64);
  ]

let lex_new (name: string) (source: string) = {
  name = name;
  src = source;
  len = String.length source;
  pos = 0;
  line = 1;
  current = if String.length source > 0 then source.[0] else '\000';
}

let lex_terminated(ls: lex_State): bool = ls.pos >= ls.len

let lex_advance(ls: lex_State): unit =
  if ls.current = '\n' then ls.line <- ls.line + 1;
  ls.pos <- ls.pos + 1;
  if lex_terminated ls then
    ls.current <- '\000'
  else
    ls.current <- ls.src.[ls.pos]

let ctype_is_whitespace (ls: lex_State): bool =
  if lex_terminated ls then false else
    match ls.current with
    | ' ' | '\n' | '\r' | '\x0C' -> true
    | _ -> false

let lex_skip_whitespace (ls: lex_State): unit =
  while (not (lex_terminated ls)) && ctype_is_whitespace ls do
    lex_advance ls;
  done; ()

let ctype_is_alpha (ls: lex_State): bool =
  if lex_terminated ls then false else
  match ls.current with
  | 'a' .. 'z' | 'A' .. 'Z'
  | '_' -> true
  | _ -> false

let ctype_is_digit (ls: lex_State): bool =
  if lex_terminated ls then false else
    match ls.current with
    | '0' .. '9' -> true
    | _ -> false

let ctype_is_alnum (ls: lex_State): bool =
  match ls.current with
    | 'a' .. 'z' | 'A' .. 'Z'
    | '_' -> true
    | '0' .. '9' -> true
    | _ -> false

let lex_read_identifier (ls: lex_State): string =
  let start = ls.pos in
  while (not (lex_terminated ls)) && ctype_is_alnum ls do
    lex_advance ls;
  done;
  String.sub ls.src start (ls.pos - start)

let lex_read_digit (ls: lex_State): string =
  let start = ls.pos in
  while not (lex_terminated ls) && ctype_is_digit ls do
    lex_advance ls;
  done;
  String.sub ls.src start (ls.pos - start)

let lex_read_string (ls: lex_State): string =
  lex_advance ls;
  let start: int = ls.pos in
  while ls.current != '"' do
    lex_advance ls;
  done;
  let l_end: int = ls.pos in
  lex_advance ls;
  String.sub ls.src start (l_end - start)

let lex_report_location (ls: lex_State): location =
  let loc: location = location_new ls.name ls.line in
  loc

let read_double_op (ls: lex_State): token =
  match ls.current with
  | '=' ->
      lex_advance ls;
      if ls.current = '=' then begin lex_advance ls; TK_equ end
      else TK_equals
  | '!' ->
      lex_advance ls;
      if ls.current = '=' then begin lex_advance ls; TK_neq end
      else syml_errorf "Expected '=' after '!'"
  | _ -> syml_errorf "Unsupported char <%d>" (int_of_char ls.current)

let lex_read_char (ls: lex_State): token =
  match ls.current with
  | '(' -> lex_advance ls; TK_lparen
  | ')' -> lex_advance ls; TK_rparen
  | '{' -> lex_advance ls; TK_lbrace
  | '}' -> lex_advance ls; TK_rbrace
  | '=' | '!' -> read_double_op ls
  | ':' -> lex_advance ls; TK_colon
  | '+' -> lex_advance ls; TK_plus
  | '-' -> lex_advance ls; TK_minus
  | '*' -> lex_advance ls; TK_mul
  | '/' -> lex_advance ls; TK_div
  | ';' -> lex_advance ls; TK_semicolon
  | ',' -> lex_advance ls; TK_comma
  | _ -> syml_errorf "Unsupported char <%d>" (int_of_char ls.current)

let lex_search (s: string): token =
  let rec aux (s: string) (l: (string * token) list): token =
    match l with
    | [] -> TK_identifier s
    | (k, v) :: _ when k = s -> v
    | _ :: tail -> aux s tail
  in
  aux s reserved_table

(* get next token in lexer stream *)
let lex_next(ls: lex_State): token =
  lex_skip_whitespace ls;
  if ctype_is_alpha ls then
    let ident = lex_read_identifier ls in
    lex_search ident
  else if ctype_is_digit ls then
    let digit = lex_read_digit ls in
    TK_number digit
  else if lex_terminated ls then TK_EOF
  else if ls.current == '"' then
    let str: string = lex_read_string ls in
    TK_string str
  else lex_read_char ls
