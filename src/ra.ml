(*
 * Syml:
 *  Prototype register allocator (unfinished)
 *)
open Il
open Common

type ctxt = {
    smod: smod;
    nregs: int; (* number of registers *)
    mutable sp: int; (* stack pointer *)
    mutable reg: int; (* first available register *)
    args: reg list; (* function arguments *)
  }

let ctxt_new (smod: smod): ctxt = {
    smod = smod;
    nregs = (Il.get_arch_nregister smod.arch);
    sp = 0;
    reg = 0;
    args = (Il.get_arch_funcargs smod.arch);
  }

let ctxt_stack_alloc (ctxt: ctxt) (n: int): unit =
  ctxt.sp <- ctxt.sp - n; ()

let ctxt_stack_var (ctxt: ctxt) (ty: Dtypes.datatype): mem =
  let ty_bits = type2bits ty in
  ctxt_stack_alloc ctxt (bits2size ty_bits);
  let sp = ctxt.sp in
  Stack sp

let alloc_simple_reg (ctxt: ctxt) (ty: Dtypes.datatype): mem =
  let ty_bits = type2bits ty in
  if ctxt.reg = ctxt.nregs then begin
    ctxt_stack_alloc ctxt (bits2size ty_bits);
    Stack ctxt.sp
  end else begin
    let r: int = ctxt.reg in
    ctxt.reg <- ctxt.reg + 1;
    Reg r
  end

(* used when a leave instruction is reached *)
let free_all (ctxt: ctxt): unit =
  ctxt.reg <- 0;
  ctxt.sp <- 0;
  ()

let alloc_call (ctxt: ctxt) (c: call): unit =
  let f = (fun (i: int) _: unit ->
    c.regs <- Array.append c.regs [|(List.nth ctxt.args i)|];
    ())
  in
  Array.iteri f c.args
;;

let free_reg (ctxt: ctxt) (m: mem): unit =
  match m with
  | Reg r ->
    if ctxt.reg - 1 != r then syml_errorf "bad register deallocation"
    else ctxt.reg <- ctxt.reg - 1
  | _ -> syml_errorf "bad register deallocation"

(* we might have to delete those two functions below *)

let alloc_reg_for_inst (ctxt: ctxt) (i: inst): unit =
  match i with
  | Leave -> free_all ctxt
  | Call c -> alloc_call ctxt c
  | _ -> ()

let ctxt_allocregs (ctxt: ctxt) (i: insts): unit =
  Array.iter
    (fun (inst: inst) ->
      alloc_reg_for_inst ctxt inst;
      ) i;
  ()
