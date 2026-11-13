(*
 * Syml:
 *  Resolve type names
 *)

open Ast
open Dtypes

type resolve_State

val rs_new: unit -> resolve_State
val rs_function_get_opt: resolve_State -> string -> datatype list -> (string * datatype) option
val rs_function_get: resolve_State -> string -> datatype list -> (string * datatype)
val rs_typename_get_opt: resolve_State -> string -> datatype option
val rs_typename_get: resolve_State -> string -> datatype
val rs_typename_set: resolve_State -> string -> datatype -> unit
val rs_template_get_opt: resolve_State -> string -> template option
val rs_template_get: resolve_State -> string -> template
val rs_template_set: resolve_State -> string -> template -> unit
val rs_resolve_module: resolve_State -> ast_mod -> unit
val rs_get_instance: resolve_State -> string -> datatype -> toplevel
val rs_get_instances: resolve_State -> (string * toplevel) array