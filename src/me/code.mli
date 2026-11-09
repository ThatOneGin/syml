open Il
open Ast

type code_State

val cs_new: Comp_state.t -> Typeutils.type_State -> smod -> code_State
val cs_emit_module: code_State -> ast_mod -> unit