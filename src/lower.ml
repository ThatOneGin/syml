(*
 * Syml:
 *  Lower AST into IL
 *)

open Il
open Common

type block = {
  mutable body: Il.insts;
}

let expr2val (e: Ast.expr): Il.value =
  match e with
  | Number d -> Il.Int d
  | String s -> Il.Str s
  | Binop _ -> todo "Binop translation to Il.value"
  | Ident _ -> todo "Ident translation to Il.value"

let typeofexp (e: Ast.expr) =
  match e with
  | Number _ -> Dtypes.I32
  | String _ -> Dtypes.Str
  | _ -> Dtypes.Nil

let val2op (v: Il.value): Il.operand = Val v

let stat2inst (s: Ast.stat): Il.inst =
  match s with
    | Var v ->
      let vv: Il.value = expr2val v.value in
      Move {name = v.name;
            ty   = v.ty;
            dest = None;
            src  = val2op vv;}
    | Return r ->
      Il.Ret {ty = typeofexp r;
              value = val2op (expr2val r);}

let lower_func_body (f: Ast.funct): Il.insts =
  let bl = {body = [|Enter|]} in
  let iter = fun (s: Ast.stat): unit -> 
    bl.body <- Array.append bl.body [|stat2inst s|];
  in
  Array.iter iter f.body;
  bl.body <- Array.append bl.body [|Leave|];
  bl.body

let lower_func (smod: smod) (f: Ast.toplevel): Il.label =
  let fbody: Il.insts =
    match f with
    | Func ft -> lower_func_body ft
    | _ -> [||]
  in
  let fname: string option =
    match f with
    | Func ft -> Some ft.name
    | _ -> None
  in
  let fl: Il.label = smod_newlabel smod fname in
  fl.body <- fbody;
  fl
