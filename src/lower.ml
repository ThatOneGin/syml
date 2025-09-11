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
  | Ident s -> Il.Var {
    name = s;
    reg = None;
  }

let typeofexp (e: Ast.expr) =
  match e with
  | Number _ -> Dtypes.I32
  | String _ -> Dtypes.Str
  | _ -> Dtypes.Nil

let val2op (v: Il.value): Il.operand = Val v

let stat2inst (s: Ast.stat) (t: Dtypes.datatype): Il.inst =
  match s with
    | Var v ->
      let vv: Il.value = expr2val v.value in
      Move {name = v.name;
            ty   = v.ty;
            dest = None;
            src  = val2op vv;}
    | Return r ->
      Il.Ret {ty = t;
              value = val2op (expr2val r);}

let lower_func_body (f: Ast.funct): Il.insts =
  let lab: Il.inst = Label {
    name = Some f.name;
    global = true;
  } in
  let bl = {body = [|lab; Enter|]} in
  let iter = fun (s: Ast.stat): unit -> 
    bl.body <- Array.append bl.body [|stat2inst s f.ty|];
  in
  Array.iter iter f.body;
  bl.body <- Array.append bl.body [|Leave|];
  bl.body

let lower_func (smod: smod) (f: Ast.toplevel): Il.insts =
  let fbody: Il.insts =
    match f with
    | Func ft -> lower_func_body ft
    | _ -> [||]
  in
  smod_newlabel smod true;
  fbody;
