(*
 * Syml:
 *  Resolve type names
 *)

open Ast
open Dtypes

type resolve_State = {
    generics: (string, datatype) Hashtbl.t;
    typenames: (string, datatype) Hashtbl.t;
  }

let rs_new (): resolve_State = {
    generics = Hashtbl.create 0;
    typenames = Hashtbl.create 0;
  }
;;

let rs_generic_get_opt
  (rs: resolve_State)
  (s: string): datatype option = Hashtbl.find_opt rs.generics s;;

let rs_generic_get
  (rs: resolve_State)
  (s: string): datatype =
  match rs_generic_get_opt rs s with
  | Some t -> t
  | None -> type_error (Printf.sprintf "unknown type variable %s" s)
;;

let rs_generic_set
  (rs: resolve_State)
  (s: string)
  (t: datatype): unit =
  match rs_generic_get_opt rs s with
  | Some _ -> type_error (Printf.sprintf "trying to redefine type variable %s" s)
  | None -> Hashtbl.replace rs.generics s t
;;

let rs_generic_clear (rs: resolve_State): unit = Hashtbl.clear rs.generics;;

let rs_typename_get_opt
  (rs: resolve_State)
  (s: string): datatype option = Hashtbl.find_opt rs.typenames s;;

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
  | None -> Hashtbl.replace rs.typenames s t
;;

let rs_get_symbol (rs: resolve_State) (s: string): datatype =
  let tn = rs_typename_get_opt rs s in
  match tn with
  | Some t -> t
  | None -> rs_generic_get rs s
;;

let rec visit_type (rs: resolve_State) (d: datatype): datatype =
  match d with
  | Name s -> rs_get_symbol rs s
  | Struct s -> Struct (List.map (fun (n, t) -> (n, visit_type rs t)) s)
  | Generic (n, t) -> instantiate_type rs n t
  | Ptr t -> Ptr (visit_type rs t)
  | Fptr f ->
    let ret  = visit_type rs f.ret in
    let args = List.map (fun t -> visit_type rs t) f.args in
    Fptr {
      ret = ret;
      args = args;
    }
  | _ -> d
and instantiate_type (rs: resolve_State) (n: datatype) (t: datatype): datatype =
  let (name, ndt) =
    match visit_type rs t with
    | Generic (Name s, ndt) -> (s, ndt)
    | _ -> assert false
  in
  rs_generic_set rs name (visit_type rs n);
  let nt = visit_type rs ndt in
  rs_generic_clear rs;
  nt
;;

let rs_toplevel (rs: resolve_State) (t: toplevel): unit =
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

let rs_resolve_module (rs: resolve_State) (m: ast_mod): unit =
  List.iter (rs_toplevel rs) m
;;