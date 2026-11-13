(*
 * Syml:
 *  Type checker
 *)

open Typeutils
open Ast
open Dtypes
open Resolve

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
and check_call (ts: type_State) (c: callexpr): datatype =
  let _name = compile_to_name c.caller in
  let check_call_aux ty: datatype =
    match ty with
    | Fptr ft ->
      List.iteri (fun i a ->
          let arg_ty = typeof_expr ts c.args.(i) in
          if equal arg_ty a then ()
          else
            type_error
              (Printf.sprintf "argument #%d type mismatch (wanted %s, got %s)"
                i (type2str a) (type2str arg_ty))
      ) ft.args;
      ft.ret
    | _ -> type_error (Printf.sprintf "trying to call a value of type %s" (type2str ty))
  in
  let check_caller (caller: (string * datatype) option): datatype =
    match caller with
    | Some (f_name, f_type) ->
      c.caller <- Ident f_name;
      f_type
    | None ->
      let ty = typeof_expr ts c.caller in
      check_call_aux ty
  in
  match _name with
  | None ->
    let ty = typeof_expr ts c.caller in
    check_call_aux ty
  | Some name ->
    let args = List.init (Array.length c.args) (fun i -> typeof_expr ts c.args.(i)) in
    let f_opt = rs_function_get_opt ts.rs name args in
    check_caller f_opt
and check_field_access (ts: type_State) (lhs: expr) (field: field): datatype =
  let lty = typeof_expr ts lhs in
  match lty with
  | Ptr (Struct s) | Struct s -> find_field_by_name s field
  | _ ->
    type_error
      (Printf.sprintf
        "Field access can only be done with structs and not with %s"
        (type2str lty))
    (* this error is so bad *)
and check_cast (ts: type_State) (c: castexpr): unit =
  let rt = c.ty in
  let lt = typeof_expr ts c.e in
  if (is_numeric_type lt && is_numeric_type rt) || (equal lt rt) then ()
  else match (lt, rt) with
    | (_, Ptr _) when is_numeric_type lt -> ()
    | (Ptr I8, Str) | (_, Ptr _) -> ()
    | _ ->
      type_error
        (Printf.sprintf "cast from %s to %s is not allowed"
        (type2str lt) (type2str rt))
and typeof_expr (ts: type_State) (e: expr): datatype =
  match e with
  | Number _ -> Int
  | String _ -> Str
  | Ident s -> ts_query_symbol ts s
  | Binop _ -> check_binop ts e
  | Call c -> check_call ts c
  | Cast c -> check_cast ts c; c.ty
  | Fieldaccess (lhs, field) -> check_field_access ts lhs field
  | Templatearg t ->
    let name =
      match compile_to_name t.e with
      | Some s -> s
      | None -> type_error "invalid instance"
    in
    let inst = rs_get_instance ts.rs name t.ty in
    type_sig_of_toplevel inst
;;

let check_initializer (t1: datatype) (ini: datatype): bool =
  match (t1, ini) with
  | (Struct _, Int) -> true
  | _ -> equal t1 ini

let check_vard (ts: type_State) (v: vard): unit =
  let texp = typeof_expr ts v.value in
  if not (check_initializer v.ty texp) then
    type_error
    (Printf.sprintf
      "variable type mismatch (%s != %s)"
      (type2str v.ty) (type2str texp))
  else ts_reg_symbol ts v.name v.ty; ()
;;

let is_expr_const (e: expr): bool =
  match e with
  | String _ | Number _ -> true
  | _ -> false
;;

let ensure_expr_is_const (e: expr): unit =
  if not (is_expr_const e)
  then type_error "expression is not constant"
  else ()
;;

let check_const_vard (ts: type_State) (v: vard): unit =
  ensure_expr_is_const v.value;
  check_vard ts v
;;

let check_return (ts: type_State) (r: expr): unit =
  let texp = typeof_expr ts r in
  if not (equal ts.curr texp) then
    type_error
    (Printf.sprintf
      "function return type mismatch (%s != %s)"
      (type2str ts.curr) (type2str texp))
  else ()
;;

let check_vcall (ts: type_State) (c: vcall): unit =
  let _name = compile_to_name c.func in
  let check_call_aux ty: unit =
    match ty with
    | Fptr ft when ft.ret = Nil ->
      List.iteri (fun i a ->
          let arg_ty = typeof_expr ts c.args.(i) in
          if equal arg_ty a then ()
          else
            type_error
              (Printf.sprintf "argument #%d type mismatch (wanted %s, got %s)"
                i (type2str a) (type2str arg_ty))
      ) ft.args
    | Fptr _ ->
      type_error
        (Printf.sprintf
          "function must return nil when calling as statement (but it returns a value of type %s)"
          (type2str ty))
    | _ ->
      type_error
        (Printf.sprintf
          "caller must be a function not a %s"
          (type2str ty))
  in
  let check_caller (caller: (string * datatype) option): unit =
    match caller with
    | Some (f_name, _) -> c.func <- Ident f_name
    | None ->
      let ty = typeof_expr ts c.func in
      check_call_aux ty
  in
  match _name with
  | None ->
    let ty = typeof_expr ts c.func in
    check_call_aux ty
  | Some name ->
    let args = List.init (Array.length c.args) (fun i -> typeof_expr ts c.args.(i)) in
    let f_opt = rs_function_get_opt ts.rs name args in
    check_caller f_opt
;;

let check_asm (ts: type_State) (a: asm): unit =
  Array.iter (fun v -> Common.drop @@ typeof_expr ts v) a.inputs
;;

let rec check_stat (ts: type_State) (s: stat): unit =
  match s with
  | Var v -> check_vard ts v; ()
  | Return r -> check_return ts r; ()
  | Asm a -> check_asm ts a; ()
  | Voidcall c -> check_vcall ts c; ()
  | Ifstat i -> check_if ts i; ()
  | While w -> check_while ts w; ()
  | Block b -> Array.iter (fun (s: stat) -> check_stat ts s) b.body; ()
and check_if (ts: type_State) (i: ifstat): unit =
  let _ = typeof_expr ts i.cond in
  Array.iter (fun (s: stat) -> check_stat ts s) i.blk.body
and check_while (ts: type_State) (w: whilestat): unit =
  let _ = typeof_expr ts w.cond in
  Array.iter (fun (s: stat) -> check_stat ts s) w.blk.body
;;

let reg_params (ts: type_State) (t: Ast.param array): unit =
  let f = (fun (p: Ast.param) ->
    ts_reg_symbol ts p.name p.ty;
  ) in
  Array.iter f t
;;

let check_func (ts: type_State) (f: funct): unit =
  ts.curr <- f.ty;
  reg_params ts f.params;
  Array.iter (fun (s: stat) -> check_stat ts s) f.blk.body
;;

let check_toplevel (ts: type_State) (f: toplevel): unit =
  match f with
  | Func ft -> let fscope = ts_enter ts in check_func fscope ft
  | Globvar v -> check_const_vard ts v
  | Extern e ->
    ts_reg_symbol ts e.name e.ty
  | _ -> ()
;;

let ts_check_module (ts: type_State) (md: ast_mod): unit =
  rs_resolve_module ts.rs md;
  let instances = rs_get_instances ts.rs in
  List.iter (check_toplevel ts) md.mod_body;
  Array.iter (fun (_, t) ->
    let _ts = ts_enter ts in
    check_toplevel _ts t) instances;
;;