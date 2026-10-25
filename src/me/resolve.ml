(*
 * Syml:
 *  Resolve type names
 *)

open Ast
open Dtypes

type resolve_State = (string, datatype) Hashtbl.t

let rs_new (): resolve_State = Hashtbl.create 0;;

let rs_typename_get_opt
  (rs: resolve_State)
  (s: string): datatype option = Hashtbl.find_opt rs s;;

let rs_typename_get
  (rs: resolve_State)
  (s: string): datatype =
  match rs_typename_get_opt rs s with
  | Some t -> t
  | None -> type_error (Printf.sprintf "unknown type %s" s)
;;

let rs_typename_set
  (rs:resolve_State)
  (s: string)
  (t: datatype): unit =
  match rs_typename_get_opt rs s with
  | Some _ -> type_error (Printf.sprintf "trying to redefine type %s" s)
  | None -> Hashtbl.replace rs s t
;;

let rec visit_type (rs: resolve_State) (d: datatype): datatype =
  match d with
  | Name s -> rs_typename_get rs s
  | Struct s -> Struct (List.map (fun (n, t) -> (n, visit_type rs t)) s)
  | _ -> d
;;

let rs_main (rs: resolve_State) (t: toplevel): unit =
  let visit_stat a s =
    let ve = a.visit_expr a in
    match s with
    | Var v ->
      ve v.value;
      v.ty <- visit_type rs v.ty
    | _ -> ()
  in
  let visit_toplevel a t =
    let ve = a.visit_expr a in
    let vb = a.visit_blk a in
    match t with
    | Func f ->
      vb f.blk;
      f.ty <- visit_type rs f.ty;
      Array.iter (fun (p: param) -> p.ty <- visit_type rs p.ty) f.params
    | Globvar v ->
      ve v.value;
      v.ty <- visit_type rs v.ty
    | Extern e -> e.ty <- visit_type rs e.ty
    | Typedef (name, ty) -> rs_typename_set rs name ty
  in
  let av = {ast_default_visitor with
            visit_stat = visit_stat;
            visit_toplevel = visit_toplevel;} in
  av.visit_toplevel av t
;;