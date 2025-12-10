(*
 * Syml:
 *  syml codegen
 *)

open Il
open Ast
open Ra
open Common

(* state needed to convert AST to IL *)
type code_State = {
  mutable ctxt: ctxt;
  smod: smod;
  mutable code: insts;
  mutable ty: Dtypes.datatype;
}

let cs_new (smod: smod): code_State = {
    ctxt = ctxt_new smod;
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

let get_jmp_from_expr (e: Ast.expr) (i: int) (o: operand) (invert: bool): inst =
  match e with
  | Binop (_, op, _) -> begin
    match op with
    | OEQU -> if invert then Jmp (Jne i) else Jmp (Je i)
    | ONEQ -> if invert then Jmp (Je i) else Jmp (Jne i)
    | _ -> Jmp (Test {op = o; jit =i;})
  end
  | _ -> Jmp (Test {op = o; jit =i;})

(* -- tranlations -- *)

let code_primexp (e: expr): value =
  match e with
  | Number i -> Il.Int i
  | String s -> Il.Str s
  | Ident s  -> Il.Var {name = s; reg = None;}
  | _ -> Il.Int 0

(* compile a binary expressino *)
let rec code_binopexp (cs: code_State) (e: expr) (return_val: bool): operand =
  match e with
  | Binop (l, o, r) ->
    let lhs_reg = alloc_simple_reg cs.ctxt cs.ty in
    let rhs_reg = alloc_simple_reg cs.ctxt cs.ty in
    (* move the left hand *)
    cs_code cs (Move {
      name = None;
      ty = cs.ty;
      dest = Some lhs_reg;
      src = code_exp cs l true;
    });
    (* move the right hand *)
    cs_code cs (Move {
      name = None;
      ty = cs.ty;
      dest = Some rhs_reg;
      src = code_exp cs r true;
    });
    let b = Il.Binop {
      left = Reg (Some lhs_reg);
      right = Reg (Some rhs_reg);
      op = o;
      ty = cs.ty;
      return_val = return_val;
    } in
    cs_code cs b;
    free_reg cs.ctxt rhs_reg; (* free the right register *)
    free_reg cs.ctxt lhs_reg; (* and the left one *)
    Reg (Some lhs_reg) (* but return it *)
  | _ -> unreachable "Codegen" "expected binary expression";
and code_exp (cs: code_State) (e: expr) (return_val: bool): operand =
  match e with
  | Binop _ ->
    code_binopexp cs e return_val
  | _ ->
    Val (code_primexp e)

let code_ret (cs: code_State) (r: expr): unit =
  let op = (code_exp cs r true) in
  cs_code cs (Ret {ty = cs.ty;
                  value = op;
                  pc = 0;})

let code_var (cs: code_State) (v: vard): unit =
  let dest = Ra.alloc_var cs.ctxt v.name v.ty in
  let old_cs_ty = cs.ty in
  cs.ty <- v.ty; (* for code_binopexp *)
  let op = (code_exp cs v.value true) in
  cs.ty <- old_cs_ty; (* restore helper *)
  cs_code cs (Move {
    name = None;
    ty = v.ty;
    dest = Some dest;
    src = op;
  })

let code_call (cs: code_State) (c: vcall): unit =
  cs_code cs (Il.Call {
    f = c.name;
    args = [||];
    regs = [||];
  });
  ()
;;

let rec code_stat (cs: code_State) (s: stat): unit =
  match s with
  | Return r -> code_ret cs r
  | Var v -> code_var cs v
  | Asm s -> cs_code cs (Il.Asm s)
  | Voidcall c -> code_call cs c
  | Block b -> Array.iter (fun (s: stat): unit -> code_stat cs s) b.body
  | Ifstat i -> code_if cs i
  | While i -> code_while cs i
and code_if (cs: code_State) (i: ifstat): unit =
  let cond: operand = code_exp cs i.cond false in
  let _if = Array.length cs.code in
  cs_code cs (Jmp (Je 0));
  Array.iter (fun (s: stat): unit -> code_stat cs s) i.blk.body;
  code_unnamedlabel cs;
  let curr_label = cs.smod.labelcount - 1 in
  cs.code.(_if) <- get_jmp_from_expr i.cond curr_label cond true
and code_while (cs: code_State) (w: whilestat): unit =
  cs_code cs (Jmp (Jump (cs.smod.labelcount+1)));
  let curr_label = cs.smod.labelcount in
  code_unnamedlabel cs;
  Array.iter (fun (s: stat): unit -> code_stat cs s) w.blk.body;
  code_unnamedlabel cs;
  let cond: operand = code_exp cs w.cond false in
  cs_code cs (get_jmp_from_expr w.cond curr_label cond false)

let code_func (cs: code_State) (f: funct): unit = 
  code_namedlabel cs f.name true; 
  code_unnamedlabel cs;
  code_enter cs;
  Array.iter (fun (s: stat): unit -> code_stat cs s) f.blk.body;
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
  patch_ret cs;
  ctxt_allocregs cs.ctxt cs.code;
  let tmp: insts = cs.code in
  cs.code <- [||];
  cs.ty <- Dtypes.Nil;
  cs.ctxt <- ctxt_new cs.smod;
  tmp
