(*
 * Syml:
 *  type utilities
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
;;

let ts_enter (ts: type_State): type_State = ts_new (Some ts);;

let ts_reg_symbol (ts: type_State) (name: string) (ty: datatype): unit =
  Hashtbl.replace ts.variables name ty; ()
;;

let rec ts_query_symbol (ts: type_State) (name: string): datatype =
  match Hashtbl.find_opt ts.variables name with
  | Some t -> t
  | None when ts.parent = None -> type_error (Printf.sprintf "Unknown variable '%s'" name)
  | None ->
    ts_query_symbol
      (match ts.parent with
        | Some t -> t
        | None -> Common.unreachable
          "type-checking"
          "no parent type state") name
;;

let is_numeric_type (ty: datatype): bool =
  match ty with
  | I64 | I32 | I16 | I8 | Int -> true
  | _ -> false
;;

let is_pointer_type (ty: datatype): bool =
  match ty with
  | Ptr _ -> true
  | _ -> false
;;

let pointed_type (ty: datatype): datatype option =
  match ty with
  | Ptr t -> Some t
  | _ -> None
;;

let equal (ty1: datatype) (ty2: datatype): bool =
  if (is_numeric_type ty1) && (is_numeric_type ty2) then true
  else
    match (ty1, ty2) with
    | (Ptr t1, Ptr t2) -> t1 = t2
    | _ -> ty1 = ty2
;;

(* check if e compiles to a name *)
let compile_to_name (e: Ast.expr): string option =
  match e with
  | Ident s -> Some s
  | _ -> None
;;

let find_field_by_name (s: (string * datatype) list) (field: field): datatype =
  List.iteri (fun i (v, _) -> if v = field.name then field.idx <- i else ()) s;
  match field.idx with
  | -1 ->
    type_error
      (Printf.sprintf
        "no field %s in struct %s"
        field.name (type2str (Struct s)))
  | _ ->
    let (_, ty) = List.nth s field.idx in
    ty
;;
