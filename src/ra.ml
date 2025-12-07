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
  var_map: (string, reg) Hashtbl.t ;
  args: reg list; (* function arguments *)
}

let ctxt_new (smod: smod): ctxt = {
    smod = smod;
    nregs = (Il.get_arch_nregister smod.arch);
    sp = 0;
    reg = 0;
    var_map = Hashtbl.create 32;
    args = (Il.get_arch_funcargs smod.arch);
  }

let ctxt_getvar (ctxt: ctxt) (name: string): reg =
  match Hashtbl.find_opt ctxt.var_map name with
  | Some r -> r
  | None -> syml_errorf "Unknown variable '%s'." name

let ctxt_stack_alloc (ctxt: ctxt) (n: int): unit =
  ctxt.sp <- ctxt.sp + n; ()

let alloc_var (ctxt: ctxt) (name: string) (ty: Dtypes.datatype): reg =
  ctxt_stack_alloc ctxt (bits2size (type2bits ty));
  let loc = Spill ctxt.sp in
  Hashtbl.replace ctxt.var_map name loc;
  loc

let alloc_simple_reg (ctxt: ctxt) (ty: Dtypes.datatype): reg =
  if ctxt.reg = ctxt.nregs then begin
    ctxt_stack_alloc ctxt (bits2size (type2bits ty));
    Spill ctxt.sp
  end else begin
    let r: int = ctxt.reg in
    ctxt.reg <- ctxt.reg + 1;
    Rreg (r, (type2bits ty))
  end

let scratch_reg (ty: Dtypes.datatype): reg = 
  Rreg (0, (type2bits ty))

(* used when a leave instruction is reached *)
let free_all (ctxt: ctxt): unit =
  ctxt.reg <- 0;
  ctxt.sp <- 0;
  ()

let check_value (ctxt: ctxt) (i: value): unit =
  match i with
  | Var v -> v.reg <- Some (ctxt_getvar ctxt v.name); ()
  | _ -> ()
;;

let check_operand (ctxt: ctxt) (o: operand): unit =
  match o with
  | Val v -> check_value ctxt v
  | _ -> ()
;;

let alloc_call (ctxt: ctxt) (c: call): unit =
  let f = (fun (i: int) (o: operand): unit ->
    check_operand ctxt o; (* check the operand *)
    c.regs <- Array.append c.regs [|(List.nth ctxt.args i)|];
    ())
  in
  Array.iteri f c.args
;;

let free_reg (ctxt: ctxt) (r: reg): unit =
  match r with
  | Spill _ -> ()
  | Rreg (r, _) ->
    if ctxt.reg - 1 != r then
      syml_errorf "bad register deallocation"
    else
      ctxt.reg <- ctxt.reg - 1

let alloc_reg_for_inst (ctxt: ctxt) (i: inst): unit =
  match i with
  | Move m ->
    begin
      match m.dest with
      | Some _ -> ()
      | None ->
        m.dest <-
        Some (alloc_simple_reg ctxt m.ty);
    end;
    let loc = (* unwrap m.dest *)
      match m.dest with
      | Some r -> r
      | None -> unreachable "Ra" "while unwrapping instruction"
    in
    check_operand ctxt m.src;
    begin
      match m.name with
        | Some name -> Hashtbl.replace ctxt.var_map name loc
        | None -> ()
    end
  | Ret r -> check_operand ctxt r.value
  | Enter -> ()
  | Leave -> free_all ctxt
  | Label _ -> ()
  | Asm _ -> ()
  | Call c -> alloc_call ctxt c
  | Binop b ->
    check_operand ctxt b.left;
    check_operand ctxt b.right
  | Jmp j ->
    begin 
      match j with
      | Test t -> check_operand ctxt t.op
      | _ -> () end

let ctxt_allocregs (ctxt: ctxt) (i: insts): unit =
  Array.iter
    (fun (inst: inst) ->
      alloc_reg_for_inst ctxt inst;
      ) i;
  ()
