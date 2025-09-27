(*
 * Syml:
 *  Type checker
 *)

open Ast
open Dtypes

type type_State = {
  variables: (string, datatype) Hashtbl.t;
  mutable curr: datatype; (* current function return type *)
}

let ts_new (): type_State = {
  variables = Hashtbl.create 32;
  curr = Nil;
}

let ts_reg_variable (ts: type_State) (name: string) (ty: datatype): unit =
  Hashtbl.replace ts.variables name ty; ()

let ts_query_variable (ts: type_State) (name: string): datatype =
  match Hashtbl.find_opt ts.variables name with
  | Some t -> t
  | None -> type_error (Printf.sprintf "Unknown variable '%s'" name)

let is_numeric_type (ty: datatype): bool =
  match ty with
  | I64 | I32 | I16 | I8 | Int -> true
  | _ -> false

let equal (ty1: datatype) (ty2: datatype): bool =
  if (is_numeric_type ty1) && (is_numeric_type ty2) then true
  else ty1 = ty2

let typeof_expr (ts: type_State) (e: expr): datatype =
  match e with
  | Number _ -> Int
  | String _ -> Str
  | Ident s -> ts_query_variable ts s
  | _ -> I32

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

let check_stat (ts: type_State) (s: stat): unit =
  match s with
  | Var v -> check_vard ts v; ()
  | Return r -> check_return ts r; ()
  | Asm _ -> ()

let check_func (ts: type_State) (f: toplevel): unit =
  match f with
  | Func ft ->
    ts.curr <- ft.ty;
    Array.iter (fun (s: stat) -> check_stat ts s) ft.blk.body;
    ts_reg_variable ts ft.name ft.ty; ()
  | _ -> ()
