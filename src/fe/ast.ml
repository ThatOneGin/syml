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
  | Call of (expr * expr array)
  | Fieldaccess of (expr * field)
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
    params: param array;
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

let block_append (b: block) (s: stat): unit =
  b.body <- Array.append b.body [|s|]; ()

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
    | Call (caller, args) -> ve caller; Array.iter ve args
    | Fieldaccess (lv, _) -> ve lv
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