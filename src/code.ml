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

let cs_move
  (cs: code_State)
  (dest: mem)
  (src: operand): unit =
  cs_code cs (Move {
    dest = dest;
    src = src;
  })
;;

let cs_binop
    (cs: code_State)
    (left: operand)
    (right: operand)
    (op: Ast.operator)
    (ty: Dtypes.datatype)
    (ret: bool)
  : unit =
  cs_code cs (Binop {
    left = left;
    right = right;
    op = op;
    ty = ty;
    return_val = ret;
  })
;;

let cs_get_var (cs: code_State) (name: string): typed_mem =
  match Hashtbl.find_opt cs.vars name with
  | Some v -> v
  | _ -> syml_errorf "Unknown variable %s" name

let cs_reg_var (cs: code_State) (name: string) (loc: typed_mem): unit =
  Hashtbl.replace cs.vars name loc

let code_enter (cs: code_State): unit = cs_code cs (Enter (-1L)); ()
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
    let dst = ctxt_alloc_vreg cs.ctxt in
    cs_move cs dst @@ code_exp cs l true; (* move the left hand *)
    cs_binop cs (Il.Mem (dst, ty_bits)) (code_exp cs r true) o cs.ty return_val;
    Il.Mem (dst, ty_bits)
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
  let bits_ty = type2bits v.ty in
  let dest = Ra.reserve_var cs.ctxt bits_ty in
  let old_cs_ty = cs.ty in
  cs.ty <- v.ty; (* for code_binopexp *)
  let op = code_exp cs v.value true in
  cs.ty <- old_cs_ty; (* restore helper *)
  cs_reg_var cs v.name (dest, bits_ty);
  cs_code cs (Move {
    dest = dest;
    src = op;
  })

let code_args
  (cs: code_State)
  (a: Ast.expr array): operand array =
  let args = ref [||] in
  Array.iter
    (fun e ->
      args := Array.append !args [|code_exp cs e true|]
    ) a;
  !args
;;

let code_call (cs: code_State) (c: vcall): unit =
  let args = code_args cs c.args in
  cs_code cs (Il.Call {
    f = c.name;
    args = args;
  })
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

(* Register a function parameter in 'vars' *)
let code_param
  (cs: code_State)
  (p: param)
  (i: int): unit =
  let ty_bits = type2bits p.ty in
  let dest = Stack (16 + (8 * i)) in
  cs_reg_var cs p.name @@ (dest, ty_bits)

(* move function arguments to stack variables *)
let code_func_params (cs: code_State) (f: funct): unit =
  Array.iteri (fun (i: int) (p: param) ->
    code_param cs p i) f.params

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

let align_vars (cs: code_State) (n: int): int64 =
  let x = get_arch_stack_align cs.smod.arch in
  if n = 0
  then 0L
  else Int64.of_int @@ (n + (x - 1)) land (lnot (x - 1))

let patch_enter (cs: code_State): unit =
  let iter = fun (k: int) (i: inst): unit ->
    match i with
    | Enter _ ->
      let sp = cs.ctxt.sp in
      let n = align_vars cs (-sp) in
      cs.code.(k) <- Enter n
    | _ -> ()
  in
  Array.iteri iter cs.code

let cs_finish (cs: code_State): insts =
  patch_ret cs;
  patch_enter cs;
  let tmp: insts = regalloc cs.ctxt cs.code in
  cs.code <- [||];
  cs.ty <- Dtypes.Nil;
  cs.ctxt <- ctxt_new cs.smod;
  Hashtbl.clear cs.vars;
  tmp
