(*
 * Syml:
 *  Resolve type names
 *)

open Ast
open Dtypes

type resolve_State

val rs_new: unit -> resolve_State
val rs_typename_get_opt: resolve_State -> string -> datatype option
val rs_typename_get: resolve_State -> string -> datatype
val rs_typename_set: resolve_State -> string -> datatype -> unit
val rs_main: resolve_State -> toplevel -> unit