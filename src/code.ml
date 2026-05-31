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
    opts: Comp_state.t;
    mutable ctxt: ctxt;
    smod: smod;
    mutable code: insts;
    mutable ty: Dtypes.datatype;
    vars: (string, typed_mem) Hashtbl.t;
    glob: (string, typed_mem) Hashtbl.t;
  }

let cs_new (opts: Comp_state.t) (smod: smod): code_State = {
    opts = opts;
    ctxt = ctxt_new opts smod;
    smod = smod;
    code = [||];
    ty = Dtypes.Nil;
    vars = Hashtbl.create 32;
    glob = Hashtbl.create 32;
  }
;;

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

let cs_alloca
  (cs: code_State)
  (ty: Dtypes.datatype)
  (dest: vreg): unit =
  cs_code cs (Alloca {
    ty = ty;
    dest = dest;
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

let cs_lea (cs: code_State) (src: operand) (dest: mem): unit =
  cs_code cs (Lea {
    src = src;
    dest = dest;
  })
;;

let cs_call (cs: code_State) (caller: operand) (args: operand array): unit =
  cs_code cs (Call {
    f = caller;
    args = args;
  })
;;

let cs_asm (cs: code_State) (code: string) (inputs: operand array): unit =
  cs_code cs (Asm {
    code = code;
    inputs = inputs;
  })
;;

let cs_get_glob (cs: code_State) (name: string): typed_mem =
  match Hashtbl.find_opt cs.glob name with
  | Some v -> v
  | None -> syml_errorf "Unknown variable '%s'" name
;;

let cs_get_var (cs: code_State) (name: string): typed_mem =
  match Hashtbl.find_opt cs.vars name with
  | Some v -> v
  | None -> cs_get_glob cs name
;;

let cs_reg_var
  (cs: code_State)
  (name: string)
  (loc: typed_mem): unit =
  Hashtbl.replace cs.vars name loc
;;

let cs_reg_glob
  (cs: code_State)
  (name: string)
  (loc: typed_mem): unit =
  Hashtbl.replace cs.glob name loc
;;

let code_enter (cs: code_State): unit = cs_code cs (Enter (-1L)); ();;
let code_leave (cs: code_State): unit = cs_code cs Leave; ();;

(* create labels *)

let code_namedlabel (cs: code_State) (name: string) (global: bool) (ty: ltype): unit =
  cs_code cs (Label (Named_label {
    name = name;
    global = global;
    ltype = ty;
  })); ()
;;

let code_unnamedlabel (cs: code_State): unit =
  cs_code cs (Label (Unnamed_label cs.smod.labelcount));
  smod_newlabel cs.smod false; ()
;;

(* Reserve label id but not emit a 'label' instruction on the buffer *)
let reserve_unnamed_label (cs: code_State): int =
  let id = cs.smod.labelcount in
  smod_newlabel cs.smod false; id
;;

let get_jmp_from_expr (e: Ast.expr) (i: int) (o: operand) (invert: bool): inst =
  match e with
  | Binop (_, op, _) -> begin
    match op with
    | OEQU -> if invert then Jmp (Jne i) else Jmp (Je i)
    | ONEQ -> if invert then Jmp (Je i) else Jmp (Jne i)
    | _ -> Jmp (Test {op = o; jit =i;})
  end
  | _ -> Jmp (Test {op = o; jit =i;})
;;

(* -- translations -- *)

let code_const (cs: code_State) (k: const) (t: ref_ty): operand * int =
  let idx = smod_push_const cs.smod k in
  (Il.Mem(Il.Addr idx, t), idx)
;;

let code_glob (cs: code_State) (name: string) (k: const) (t: ref_ty): operand =
  drop @@ smod_push_global cs.smod name k;
  Il.Mem (Il.Name name, t)
;;

let code_string  (cs: code_State) (s: string): operand =
  let (lea_src, _) = code_const cs (K_string s) str_t in
  let lea_dest = Ra.ctxt_alloc_vreg cs.ctxt in
  cs_lea cs lea_src lea_dest;
  Il.Mem(lea_dest, str_t)
;;

let code_primexp (cs: code_State) (e: expr): operand =
  match e with
  | Number i -> Il.Imm (Int64.of_int i, i32_t)
  | String s -> code_string cs s
  | Ident s -> Il.Mem (cs_get_var cs s)
  | _ -> Il.Imm (0L, i8_t)
;;

(* compile a binary expressino *)
let rec code_binopexp (cs: code_State) (e: expr) (return_val: bool): operand =
  match e with
  | Binop (l, o, r) ->
    let ty = ref_of_type cs.ty in
    let dst = ctxt_alloc_vreg cs.ctxt in
    cs_move cs dst @@ code_exp cs l true; (* move the left hand *)
    cs_binop cs (Il.Mem (dst, ty)) (code_exp cs r true) o cs.ty return_val;
    Il.Mem (dst, ty)
  | _ -> unreachable "Codegen" "expected binary expression";
and code_exp (cs: code_State) (e: expr) (return_val: bool): operand =
  match e with
  | Binop _ -> code_binopexp cs e return_val
  | Call _ -> code_call cs e
  | _ -> code_primexp cs e

and code_call (cs: code_State) (e: expr): operand =
  match e with
  | Call (caller, args) ->
    let il_call_caller = code_exp cs caller true in
    let il_call_args = code_args cs args in
    cs_call cs il_call_caller il_call_args;
    Mem (Reg (Mreg 0), ref_of_type cs.ty)
  | _ -> unreachable "Codegen" "expected call expression";

and code_args (* helper *)
  (cs: code_State)
  (a: Ast.expr array): operand array =
  let args = ref [||] in
  Array.iter
    (fun e ->
      args := Array.append !args [|code_exp cs e true|]
    ) a;
  !args
;;

let code_const_exp (e: expr): const =
  match e with
  | String s -> K_string s
  | Number i -> K_integer (Int64.of_int i, Bits32)
  | _ -> unreachable "" ""
;;

let code_glob_exp (cs: code_State) (name: string) (e: expr): operand =
  match e with
  | String s -> code_glob cs name (K_string s) str_t
  | Number i -> code_glob cs name (K_integer (Int64.of_int i, Bits32)) i32_t
  | _ -> unreachable "" ""
;;

let code_ret (cs: code_State) (r: expr): unit =
  let op = (code_exp cs r true) in
  cs_code cs (Ret {ty = cs.ty;
                  value = op;
                  pc = 0;})
;;

let code_var (cs: code_State) (v: vard): unit =
  let ty = ref_of_type v.ty in
  let dest = Ra.ctxt_alloc_vreg cs.ctxt in
  let old_cs_ty = cs.ty in
  cs.ty <- v.ty; (* for code_binopexp *)
  let op = code_exp cs v.value true in
  cs.ty <- old_cs_ty; (* restore helper *)
  cs_reg_var cs v.name (dest, ty);
  cs_alloca cs v.ty (vreg_of_mem dest);
  cs_move cs dest op
;;

let code_vcall (cs: code_State) (c: vcall): unit =
  let args = code_args cs c.args in
  cs_code cs (Il.Call {
    f = code_exp cs c.func true;
    args = args;
  })
;;

let code_asm (cs: code_State) (a: asm): unit =
  let inputs = code_args cs a.inputs in
  cs_asm cs a.code inputs
;;

let rec code_stat (cs: code_State) (s: stat): unit =
  match s with
  | Return r -> code_ret cs r
  | Var v -> code_var cs v
  | Asm a -> code_asm cs a
  | Voidcall c -> code_vcall cs c
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
;;

(* Register a function parameter in 'vars' *)
let code_param
  (cs: code_State)
  (p: param)
  (i: int): unit =
  let ty = ref_of_type p.ty in
  let dest = Stack (16 + (8 * i)) in
  cs_reg_var cs p.name @@ (dest, ty)
;;

(* move function arguments to stack variables *)
let code_func_params (cs: code_State) (f: funct): unit =
  Array.iteri (fun (i: int) (p: param) ->
    code_param cs p i) f.params
;;

let code_func (cs: code_State) (f: funct): unit =
  cs_reg_glob cs f.name (Name f.name, fptr_t);
  code_namedlabel cs f.name true Lfunction; 
  code_unnamedlabel cs;
  code_enter cs;
  code_func_params cs f;
  Array.iter (fun (s: stat): unit -> code_stat cs s) f.blk.body;
  code_unnamedlabel cs;
  code_leave cs
;;

let code_globvar (cs: code_State) (v: vard): unit =
  let ty = ref_of_type v.ty in
  let _ = code_glob cs v.name (code_const_exp v.value) ty in
  cs_reg_var cs v.name (Il.Name v.name, ty)
;;

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
;;

let align_vars (cs: code_State) (n: int): int64 =
  let x = get_arch_stack_align cs.smod.arch in
  if n = 0
  then 0L
  else Int64.of_int @@ (n + (x - 1)) land (lnot (x - 1))
;;

let patch_enter (cs: code_State): unit =
  let iter = fun (k: int) (i: inst): unit ->
    match i with
    | Enter _ ->
      let _n: int ref = ref 0 in
      Hashtbl.iter (fun _ (v: typed_mem) -> 
        let (_, ty) = v in
        _n := size_of_ref_ty ty) cs.vars;
      let n = align_vars cs !_n in
      cs.code.(k) <- Enter n
    | _ -> ()
  in
  Array.iteri iter cs.code
;;

let cs_finish (cs: code_State): insts =
  patch_ret cs;
  patch_enter cs;
  let tmp: insts = regalloc cs.ctxt cs.code in
  cs.code <- [||];
  cs.ty <- Dtypes.Nil;
  cs.ctxt <- ctxt_new cs.opts cs.smod;
  Hashtbl.clear cs.vars;
  Comp_state.ifdo cs.opts.log_il (fun _ -> print_insts tmp);
  tmp
;;

let cs_toplevel (cs: code_State) (t: toplevel): unit =
  match t with
  | Func f ->
    cs.ty <- f.ty;
    code_func cs f;
  | Globvar v -> code_globvar cs v
;;