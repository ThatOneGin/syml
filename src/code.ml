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
    vars: (string, typed_mem) Hashtbl.t;
  }

let cs_new (smod: smod): code_State = {
    ctxt = ctxt_new smod;
    smod = smod;
    code = [||];
    ty = Dtypes.Nil;
    vars = Hashtbl.create 32;
  }

(* emit instruction to cs.code *)
let cs_code (cs: code_State) (i: inst): unit =
  cs.code <- Array.append cs.code [|i|]; ()

let cs_get_var (cs: code_State) (name: string): typed_mem =
  match Hashtbl.find_opt cs.vars name with
  | Some v -> v
  | _ -> syml_errorf "Unknown variable %s" name

let cs_reg_var (cs: code_State) (name: string) (loc: typed_mem): unit =
  Hashtbl.replace cs.vars name loc

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

(* Reserve label id but not emit a 'label' instruction on the buffer *)
let reserve_unnamed_label (cs: code_State): int =
  let id = cs.smod.labelcount in
  smod_newlabel cs.smod false; id

let get_jmp_from_expr (e: Ast.expr) (i: int) (o: operand) (invert: bool): inst =
  match e with
  | Binop (_, op, _) -> begin
    match op with
    | OEQU -> if invert then Jmp (Jne i) else Jmp (Je i)
    | ONEQ -> if invert then Jmp (Je i) else Jmp (Jne i)
    | _ -> Jmp (Test {op = o; jit =i;})
  end
  | _ -> Jmp (Test {op = o; jit =i;})

(* -- translations -- *)

let code_primexp (cs: code_State) (e: expr): operand =
  match e with
  | Number i -> Il.Imm (Int64.of_int i, Bits32)
  | String s -> Il.Mem (Il.Addr (smod_push_const cs.smod s), Bits64)
  | Ident s -> Il.Mem (cs_get_var cs s)
  | _ -> Il.Imm (0L, Bits8)

(* compile a binary expressino *)
let rec code_binopexp (cs: code_State) (e: expr) (return_val: bool): operand =
  match e with
  | Binop (l, o, r) ->
    let ty_bits = type2bits cs.ty in
    let lhs_reg = ctxt_alloc_vreg cs.ctxt in
    let rhs_reg = ctxt_alloc_vreg cs.ctxt in
    (* move the left hand *)
    cs_code cs (Move {
      dest = lhs_reg;
      src = code_exp cs l true;
    });
    (* move the right hand *)
    cs_code cs (Move {
      dest = rhs_reg;
      src = code_exp cs r true;
    });
    let b = Il.Binop {
      left = Il.Mem (lhs_reg, ty_bits);
      right = Il.Mem (rhs_reg, ty_bits);
      op = o;
      ty = cs.ty;
      return_val = return_val;
    } in
    cs_code cs b;
    Il.Mem (lhs_reg, ty_bits)
  | _ -> unreachable "Codegen" "expected binary expression";
and code_exp (cs: code_State) (e: expr) (return_val: bool): operand =
  match e with
  | Binop _ -> code_binopexp cs e return_val
  | _ -> code_primexp cs e

let code_ret (cs: code_State) (r: expr): unit =
  let op = (code_exp cs r true) in
  cs_code cs (Ret {ty = cs.ty;
                  value = op;
                  pc = 0;})

let code_var (cs: code_State) (v: vard): unit =
  let dest = Ra.ctxt_alloc_vreg cs.ctxt in
  let old_cs_ty = cs.ty in
  cs.ty <- v.ty; (* for code_binopexp *)
  let op = code_exp cs v.value true in
  cs.ty <- old_cs_ty; (* restore helper *)
  cs_reg_var cs v.name (dest, type2bits v.ty);
  cs_code cs (Move {
    dest = dest;
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
  let end_label = reserve_unnamed_label cs in
  cs_code cs (get_jmp_from_expr i.cond end_label cond true);
  Array.iter (fun (s: stat): unit -> code_stat cs s) i.blk.body;
  (* now emit the label we've reserved *)
  cs_code cs (Label (Unnamed_label end_label))
and code_while (cs: code_State) (w: whilestat): unit =
  let start_label = reserve_unnamed_label cs in
  let cond_label = reserve_unnamed_label cs in
  cs_code cs (Jmp (Jump cond_label));
  cs_code cs (Label (Unnamed_label start_label));
  Array.iter (fun (s: stat): unit -> code_stat cs s) w.blk.body;
  cs_code cs (Label (Unnamed_label cond_label));
  let cond: operand = code_exp cs w.cond false in
  cs_code cs (get_jmp_from_expr w.cond start_label cond false)

(*
 * put the function argument into a 
 * local variable, e.g. edi -> [rbp - 4]
 *)
let code_param
  (cs: code_State)
  (p: param)
  (r: reg): unit =
  let dest = Ra.ctxt_alloc_vreg cs.ctxt in
  let ty_bits = type2bits p.ty in
  cs_reg_var cs p.name @@ (dest, ty_bits);
  cs_code cs (Move {
    src = Mem (Reg r, ty_bits);
    dest = dest;
  })

(* move function arguments to stack variables *)
let code_func_params (cs: code_State) (f: funct): unit =
  let regs: reg list = get_arch_funcargs cs.smod.arch in
  (* TODO: use stack when this is true *)
  if List.length regs < Array.length f.params then
    (* FIXME: better error message *)
    Common.syml_errorf "Reached maximum function parameters capacity."
  else
    Array.iteri (fun (i: int) (p: param) ->
      code_param cs p (List.nth regs i)) f.params

let code_func (cs: code_State) (f: funct): unit = 
  code_namedlabel cs f.name true; 
  code_unnamedlabel cs;
  code_enter cs;
  code_func_params cs f;
  Array.iter (fun (s: stat): unit -> code_stat cs s) f.blk.body;
  code_unnamedlabel cs;
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
  let tmp: insts = regalloc cs.ctxt cs.code in
  cs.code <- [||];
  cs.ty <- Dtypes.Nil;
  cs.ctxt <- ctxt_new cs.smod;
  tmp
