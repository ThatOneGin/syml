(*
 * Syml:
 *  syml codegen
 *)

open Il
open Ast
open Ra

(* state needed to convert AST to IL *)
type code_State = {
  smod: smod;
  mutable code: insts;
  mutable ty: Dtypes.datatype;
}

let cs_new (smod: smod): code_State = {
    smod = smod;
    code = [||];
    ty = Dtypes.Nil;
  }

(* emit instruction to cs.code *)
let cs_code (cs: code_State) (i: inst): unit =
  cs.code <- Array.append cs.code [|i|]; ()

let code_enter (cs: code_State): unit = cs_code cs Enter; ()
let code_leave (cs: code_State): unit = cs_code cs Leave; ()

(* create labels *)

let code_namedlabel (cs: code_State) (name: string) (global: bool): unit =
  cs_code cs (Label (Named_label {
    name = name;
    global = global;
  })); ()

let code_unnamedlabel (cs: code_State): unit =
  cs_code cs (Label (Unnamed_label cs.smod.labelcount));
  smod_newlabel cs.smod false; ()

(* -- tranlations -- *)

let code_exp (e: expr): value =
  match e with
  | Number i -> Il.Int i
  | String s -> Il.Str s
  | Ident s  -> Il.Var {name = s; reg = None;}
  | _ -> Il.Int 0

let code_ret (cs: code_State) (r: expr): unit =
  let op = Val (code_exp r) in
  cs_code cs (Ret {ty = cs.ty;
                  value = op;
                  pc = 0;})

let code_var (cs: code_State) (v: vard): unit =
  let op = Val (code_exp v.value) in
  cs_code cs (Move {
    name = v.name;
    ty = v.ty;
    dest = None;
    src = op;
  })

let code_stat (cs: code_State) (s: stat): unit =
  match s with
  | Return r -> code_ret cs r
  | Var v -> code_var cs v
  | Asm s -> cs_code cs (Il.Asm s)

let code_func (cs: code_State) (f: funct): unit = 
  code_namedlabel cs f.name true; 
  code_unnamedlabel cs;
  code_enter cs;
  Array.iter (fun (s: stat): unit -> code_stat cs s) f.body;
  code_unnamedlabel cs; (* TODO: patch function return to here *)
  code_leave cs

let cs_toplevel (cs: code_State) (t: toplevel): unit =
  match t with
  | Func f ->
    cs.ty <- f.ty;
    code_func cs f
  | _ -> ()

(* set all returns of the function to the last label
 * which contains a leave instruction
 *)
let patch_ret (cs: code_State): unit =
  let iter = fun (i: inst): unit ->
    match i with
    | Ret r -> r.pc <- cs.smod.labelcount - 1; ()
    | _ -> ()
  in
  Array.iter iter cs.code; ()

let cs_finish (cs: code_State): insts =
  let ctxt = ctxt_new cs.smod in
  patch_ret cs;
  ctxt_allocregs ctxt cs.code;
  let tmp: insts = cs.code in
  cs.code <- [||];
  cs.ty <- Dtypes.Nil;
  tmp
