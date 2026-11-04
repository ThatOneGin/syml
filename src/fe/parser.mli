
exception Parsing_error of Common.location * string

val parse : string -> string -> Ast.ast_mod