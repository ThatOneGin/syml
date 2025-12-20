(*
 * Syml:
 *  Type checker
 *)

open Ast
open Dtypes

type type_State = {
    parent: type_State option; (* parent scope *)
    variables: (string, datatype) Hashtbl.t;
    mutable curr: datatype; (* current function return type *)
  }

let ts_new (parent: type_State option): type_State = {
    parent = parent;
    variables = Hashtbl.create 32;
    curr = Nil;
  }

let ts_enter (ts: type_State): type_State = ts_new (Some ts)

let ts_reg_variable (ts: type_State) (name: string) (ty: datatype): unit =
  Hashtbl.replace ts.variables name ty; ()

let rec ts_query_variable (ts: type_State) (name: string): datatype =
  match Hashtbl.find_opt ts.variables name with
  | Some t -> t
  | None when ts.parent = None -> type_error (Printf.sprintf "Unknown variable '%s'" name)
  | None ->
    ts_query_variable
      (match ts.parent with
        | Some t -> t
        | None -> Common.unreachable
          "type-checking"
          "no parent type state") name

let is_numeric_type (ty: datatype): bool =
  match ty with
  | I64 | I32 | I16 | I8 | Int -> true
  | _ -> false

let is_pointer_type (ty: datatype): bool =
  match ty with
  | Ptr _ -> true
  | _ -> false

let pointed_type (ty: datatype): datatype option =
  match ty with
  | Ptr t -> Some t
  | _ -> None

let equal (ty1: datatype) (ty2: datatype): bool =
  if (is_numeric_type ty1) && (is_numeric_type ty2) then true
  else
    match (ty1, ty2) with
    | (Ptr t1, Ptr t2) -> t1 = t2
    | _ -> ty1 = ty2

let rec check_binop (ts: type_State) (b: Ast.expr) : datatype =
  let (lhs, op, rhs) = match b with
    | Binop (lhs, op, rhs) -> (lhs, op, rhs)
    | _ -> Common.unreachable "" ""
  in
  let lt = typeof_expr ts lhs in
  let rt = typeof_expr ts rhs in
  (* helper to raise a nice error *)
  let invalid () =
    type_error (Printf.sprintf "invalid operands to binary operator (%s, %s)"
      (type2str lt) (type2str rt))
  in
  match op with
  | OADD | OSUB ->
    (* int + int -> int *)
    if is_numeric_type lt && is_numeric_type rt then I32
    (* pointer + int -> pointer *)
    else if is_pointer_type lt && is_numeric_type rt then lt
    (* int + pointer -> pointer *)
    else if op = OADD && is_numeric_type lt && is_pointer_type rt then rt
    (* pointer - pointer -> ptrdiff *)
    else if op = OSUB && is_pointer_type lt && is_pointer_type rt then
      (match (pointed_type lt, pointed_type rt) with
       | (Some ptl, Some ptr) when ptl = ptr -> Int
       | _ -> type_error "pointer subtraction requires pointers to same type")
    else if op = OEQU || op = ONEQ then
      (* we need exact types *)
      (if lt = rt then I8
       else type_error "Comparison requires both hands of the same type")
    else invalid ()
  | OMUL | ODIV ->
    if is_numeric_type lt && is_numeric_type rt then I32
    else invalid ()
  | _ -> I32
and typeof_expr (ts: type_State) (e: expr): datatype =
  match e with
  | Number _ -> Int
  | String _ -> Str
  | Ident s -> ts_query_variable ts s
  | Binop _ -> check_binop ts e

let check_vard (ts: type_State) (v: vard): unit =
  let texp = typeof_expr ts v.value in
  if not (equal v.ty texp) then
    type_error
    (Printf.sprintf
      "variable type mismatch (%s != %s)"
      (type2str v.ty) (type2str texp))
  else ts_reg_variable ts v.name v.ty; ()

let check_return (ts: type_State) (r: expr): unit =
  let texp = typeof_expr ts r in
  if not (equal ts.curr texp) then
    type_error
    (Printf.sprintf
      "function return type mismatch (%s != %s)"
      (type2str ts.curr) (type2str texp))
  else ()

let check_call (ts: type_State) (c: vcall): unit =
  let t = ts_query_variable ts c.name in
  if t = Dtypes.Nil then ()
  else
    type_error
    (Printf.sprintf
      "A call statement must return nil, but instead returns %s (function '%s')."
      (type2str t)
      c.name)

let rec check_stat (ts: type_State) (s: stat): unit =
  match s with
  | Var v -> check_vard ts v; ()
  | Return r -> check_return ts r; ()
  | Asm _ -> ()
  | Voidcall c -> check_call ts c; ()
  | Ifstat i -> check_if ts i; ()
  | While w -> check_while ts w; ()
  | Block b -> Array.iter (fun (s: stat) -> check_stat ts s) b.body; ()
and check_if (ts: type_State) (i: ifstat): unit =
  let _ = typeof_expr ts i.cond in
  Array.iter (fun (s: stat) -> check_stat ts s) i.blk.body
and check_while (ts: type_State) (w: whilestat): unit =
  let _ = typeof_expr ts w.cond in
  Array.iter (fun (s: stat) -> check_stat ts s) w.blk.body

let reg_params (ts: type_State) (t: Ast.param array): unit =
  let f = (fun (p: Ast.param) ->
    ts_reg_variable ts p.name p.ty;
  ) in
  Array.iter f t

let check_func (ts: type_State) (f: funct): unit =
  ts.curr <- f.ty;
  reg_params ts f.params;
  Array.iter (fun (s: stat) -> check_stat ts s) f.blk.body

let check_toplevel (ts: type_State) (f: toplevel): unit =
  match f with
  | Func ft ->
    let func_scope = ts_enter ts in
    check_func func_scope ft;
    ts_reg_variable ts ft.name ft.ty
  | _ -> ()
