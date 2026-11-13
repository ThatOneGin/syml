(*
 * Syml:
 *  Syml's abstract syntax tree
 *)

type field = {
  mutable idx: int;
  name: string;
}

type operator =
  | OADD | OSUB | ODIV | OMUL | OEQU | ONEQ | OINVALID
type expr =
  | Number of int
  | String of string
  | Binop of (expr * operator * expr)
  | Ident of string
  | Call of callexpr
  | Cast of castexpr
  | Fieldaccess of (expr * field)
  | Templatearg of templatearg
and templatearg = {
    e: expr;
    mutable ty: Dtypes.datatype;
  }
and castexpr = {
    e: expr;
    mutable ty: Dtypes.datatype;
  }
and callexpr = {
    mutable caller: expr;
    args: expr array;
  }

type stat =
  | Var of vard
  | Return of expr
  | Asm of asm
  | Voidcall of vcall
  | Block of block
  | Ifstat of ifstat
  | While of whilestat
and toplevel =
  | Func of funct
  | Globvar of vard
  | Extern of extern
  | Typedef of (string * Dtypes.datatype)
  | Template of template
and block = {
    mutable body: stat array;
  }
and param = {
    name: string;
    mutable ty: Dtypes.datatype;
  }
and funct = {
    name: string;
    blk: block;
    mutable ty: Dtypes.datatype;
    mutable params: param array;
  }
and vard = {
    name: string;
    mutable ty: Dtypes.datatype;
    value: expr;
  }
and vcall = {
    func: expr;
    args: expr array;
  }
and ifstat = {
    cond: expr;
    blk: block;
  }
and whilestat = {
    cond: expr;
    blk: block;
  }
and asm = {
    code: string;
    inputs: expr array;
  }
and extern = {
    name: string; (* name used in the program *)
    altname: string option; (* name used in the backend *)
    mutable ty: Dtypes.datatype;
  }
and template = {
    param: string;
    decl: toplevel;
  }

let block_append (b: block) (s: stat): unit =
  b.body <- Array.append b.body [|s|]; ()
;;

type ast_visitor = {
    visit_expr: ast_visitor -> expr -> unit;
    visit_stat: ast_visitor -> stat -> unit;
    visit_blk: ast_visitor -> block -> unit;
    visit_toplevel: ast_visitor -> toplevel -> unit;
  }

let (ast_default_visitor: ast_visitor) =
  let visit_expr a e =
    let ve = a.visit_expr a in
    match e with
    | Binop (l, _, r) -> ve l; ve r
    | Call c -> ve c.caller; Array.iter ve c.args
    | Fieldaccess (lv, _) -> ve lv
    | Templatearg tmpl -> ve tmpl.e
    | _ -> ()
  in
  let visit_stat a s =
    let ve = a.visit_expr a in
    let vb = a.visit_blk a in
    match s with
    | Var v -> ve v.value
    | Return e -> ve e
    | Asm asm -> Array.iter ve asm.inputs
    | Voidcall vc -> ve vc.func; Array.iter ve vc.args
    | Block b -> vb b
    | Ifstat i -> ve i.cond; vb i.blk
    | While w -> ve w.cond; vb w.blk
  in
  let visit_blk a b =
    let vs = a.visit_stat a in
    Array.iter vs b.body
  in
  let visit_toplevel a t =
    let ve = a.visit_expr a in
    let vb = a.visit_blk a in
    match t with
    | Func f -> vb f.blk
    | Globvar v -> ve v.value
    | _ -> ()
  in
  {
    visit_expr = visit_expr;
    visit_stat = visit_stat;
    visit_blk = visit_blk;
    visit_toplevel = visit_toplevel;
  }
;;

type ast_mod = {
    mod_name: string;
    mutable mod_body: toplevel list;
  }

let new_mod name: ast_mod = {
    mod_name = name;
    mod_body = [];
  }
;;

let mod_append (md: ast_mod) (t: toplevel): unit =
  md.mod_body <- md.mod_body @ [t]
;;

(* returns the type signature of a toplevel structure (if any) *)
let rec type_sig_of_toplevel = function
  | Func f -> Dtypes.Fptr {
    ret = f.ty;
    args = List.init (Array.length f.params) (fun i -> f.params.(i).ty);
  }
  | Globvar g -> g.ty
  | Extern e -> e.ty
  | Typedef (_, t) -> t
  | Template t -> type_sig_of_toplevel t.decl
;;

(* returns the name of the declaration of a toplevel structure (if any) *)
let rec name_of_toplevel = function
  | Func f -> f.name
  | Globvar g -> g.name
  | Extern e -> e.name
  | Typedef (n, _) -> n
  | Template t -> name_of_toplevel t.decl
;;

(* check if e compiles to a name *)
let rec compile_to_name (e: expr): string option =
  match e with
  | Ident s -> Some s
  | Templatearg tmpl -> compile_to_name tmpl.e
  | _ -> None
;;
