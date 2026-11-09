(*
 * Syml:
 *  Resolve type names
 *)

open Ast
open Dtypes

type resolve_State = {
    templates: (string, template) Hashtbl.t;
    generics: (string, datatype) Hashtbl.t;
    typenames: (string, datatype) Hashtbl.t;
    mutable instances: (string * toplevel) array;
  }

let rs_new (): resolve_State = {
    templates = Hashtbl.create 0;
    generics = Hashtbl.create 0;
    typenames = Hashtbl.create 0;
    instances = [||];
  }
;;

let rs_instance_push (rs: resolve_State) (s: string) (t: toplevel): unit =
  let len = Array.length rs.instances in
  let rec aux (i:int): bool =
    match i >= len with
    | true -> false (* not found *)
    | _ ->
      let (n, _) = rs.instances.(i) in
      if n = s then true
      else aux (i + 1)
  in
  if aux 0 then ()
  else rs.instances <- Array.append rs.instances [|(s, t)|];
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

let rs_typename_add
  (rs:resolve_State)
  (s: string)
  (t: datatype): unit =
  Hashtbl.add rs.typenames s t
;;

let rs_typename_remove
  (rs:resolve_State)
  (s: string): unit =
  Hashtbl.remove rs.typenames s
;;

let rs_template_get_opt
  (rs: resolve_State)
  (s: string): template option = Hashtbl.find_opt rs.templates s;;

let rs_template_get
  (rs: resolve_State)
  (s: string): template =
  match rs_template_get_opt rs s with
  | Some t -> t
  | None -> type_error (Printf.sprintf "unknown template %s" s)
;;

let rs_template_set
  (rs: resolve_State)
  (s: string)
  (t: template): unit =
  match rs_template_get_opt rs s with
  | Some _ -> type_error (Printf.sprintf "trying to redefine template %s" s)
  | None -> Hashtbl.replace rs.templates s t
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

let rec rs_toplevel (rs: resolve_State) (t: toplevel): unit =
  let visit_expr a e =
    let ve = a.visit_expr a in
    match e with
    | Binop (l, _, r) -> ve l; ve r
    | Call (caller, args) -> ve caller; Array.iter ve args
    | Fieldaccess (lv, _) -> ve lv
    | Templatearg tmpl ->
      ve tmpl.e;
      let name =
        match compile_to_name tmpl.e with
        | Some s -> s
        | _ ->
          type_error
            "trying to pass a type argument to an expression that does not accept it"
      in
      let ty = visit_type rs tmpl.ty in
      let templ = rs_template_get rs name in
      let mname = Mangling.mangle_name_with_type name [ty] in
      let inst = rs_instanciate_template rs mname templ ty in
      tmpl.ty <- ty;
      rs_instance_push rs mname inst;
    | _ -> ()
  in
  let visit_stat a s =
    let ve = a.visit_expr a in
    match s with
    | Var v ->
      ve v.value;
      v.ty <- visit_type rs v.ty
    | Return e -> ve e
    | _ -> ()
  in
  let visit_toplevel a t =
    let ve = a.visit_expr a in
    let vb = a.visit_blk a in
    match t with
    | Func f ->
      vb f.blk;
      f.ty <- visit_type rs f.ty;
      f.params <- Array.map (fun (p: param) -> {p with ty = visit_type rs p.ty}) f.params
    | Globvar v ->
      ve v.value;
      v.ty <- visit_type rs v.ty
    | Extern e -> e.ty <- visit_type rs e.ty
    | Typedef (name, ty) -> rs_typename_set rs name ty
    | Template t -> rs_toplevel_template_aux rs t
  in
  let av = {ast_default_visitor with
            visit_expr = visit_expr;
            visit_stat = visit_stat;
            visit_toplevel = visit_toplevel;} in
  av.visit_toplevel av t
and rs_toplevel_template_aux (rs: resolve_State) (t: template): unit =
  match t.decl with
  | Typedef (name, ty) -> rs_typename_set rs name (Generic (Name t.param, ty))
  | Extern e when e.altname != None ->
    Common.syml_warnf "custom name of extern declaration may not work inside a template";
    rs_template_set rs (name_of_toplevel t.decl) t
  | _ -> rs_template_set rs (name_of_toplevel t.decl) t
and rs_instanciate_template
  (rs: resolve_State)
  (mname: string)
  (t: template)
  (d: datatype): toplevel =
  let nd = visit_type rs d in 
  let tm: toplevel =
    match t.decl with
    | Func f -> Func {
        f with
        name = mname;
      }
    | Globvar g -> Globvar {g with name = mname}
    | Extern e -> Extern {e with name = mname}
    | _ -> assert false
  in
  rs_typename_add rs t.param nd;
  rs_toplevel rs tm;
  rs_typename_remove rs t.param;
  tm
;;

let rs_resolve_module (rs: resolve_State) (md: ast_mod): unit =
  List.iter (rs_toplevel rs) md.mod_body
;;

let rs_get_instance (rs: resolve_State) (name: string) (t: datatype): toplevel =
  let mname = Mangling.mangle_name_with_type name [t] in
  let rec aux (i: int): toplevel =
    if i >= Array.length rs.instances
    then
      type_error
        (Printf.sprintf "no instance for %s<%s>"
          name (type2str t))
    else
      let (imname, top) = rs.instances.(i) in
      match imname = mname with
      | true -> top
      | _ -> aux (i+1)
  in
  aux 0
;;

let rs_get_instances (rs:resolve_State): (string * toplevel) array =
  rs.instances
;;