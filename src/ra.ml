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
    mutable vreg: int; (* first available virtual register *)
    args: reg list; (* function arguments *)
  }

let ctxt_new (smod: smod): ctxt =
  {smod = smod;
   nregs = (Il.get_arch_nregister smod.arch);
   sp = 0;
   reg = 0;
   args = (Il.get_arch_funcargs smod.arch);
   vreg = 0;}

let ctxt_stack_alloc (ctxt: ctxt) (n: int): unit =
  ctxt.sp <- ctxt.sp - n; ()

(* used when a leave instruction is reached *)
let free_all (ctxt: ctxt): unit =
  ctxt.reg <- 0;
  ctxt.vreg <- 0;
  ctxt.sp <- 0;
  ()
;;

let alloc_call (ctxt: ctxt) (c: call): unit =
  let f = (fun (i: int) _: unit ->
    c.regs <- Array.append c.regs [|(List.nth ctxt.args i)|];
    ())
  in
  Array.iteri f c.args
;;

let alloc_mreg (ctxt: ctxt): mreg = 
  if ctxt.reg >= ctxt.nregs
  then -1
  else
    let mreg = ctxt.reg in
    ctxt.reg <- ctxt.reg + 1;
    mreg
;;

(* get one virtual register *)
let ctxt_alloc_vreg (ctxt: ctxt): mem =
  let r = ctxt.vreg in
  ctxt.vreg <- ctxt.vreg + 1;
  Reg (Vreg r)
;;

let inst_used_vregs (i: inst): vreg list =
  let used_vrs = ref [] in
  let visit_reg _ r =
    match r with
    | Vreg v -> used_vrs := (v :: (!used_vrs)); r
    | _ -> r
  in
  let iv = {Il.standard_visitor with
            visit_reg = visit_reg} in
  drop @@ visit_inst iv i;
  !used_vrs
;;

(*
 * Calculate virtual registers lifetimes
 * and return a tuple with two arrays which
 * the first element contains the locations when they were first used
 * and the second element contains when they were lastly used
 *)
let calculate_live_points (ctxt: ctxt) (is: insts): (int array * int array) =
  let nvregs = ctxt.vreg in
  let ninsts = Array.length is in
  let new_live _ = -1 in
  let new_ints _ = ints_new nvregs (-1) in
  let used_vregs = Array.init ninsts new_ints in
  let live_start = Array.init ninsts new_live in
  let live_end = Array.init ninsts new_live in
  for i = 0 to ninsts - 1 do
    let _i = is.(i) in
    List.iteri (fun j v -> ints_set used_vregs.(i) j v) (inst_used_vregs _i)
  done;
  for i = ninsts - 1 downto 0 do
    let used_set = used_vregs.(i) in
    for j = 0 to used_set.size - 1 do
      let used = ints_get used_set j in
      match used with
      | -1 -> ();
      | _ when live_end.(used) = -1 -> live_end.(used) <- i; live_start.(used) <- i;
      | _ when live_end.(used) > -1 -> live_start.(used) <- i;
      | _ -> unreachable "vreg does not match any expected pattern" "ra"
    done
  done;
  (live_start, live_end)
;;

(*
 * map virtual registers to
 * real machine registers
 * still can't do spilling
 *)
let regalloc (ctxt: ctxt) (is: insts): insts =
  let ninsts = Array.length is in
  let new_inst _ = Nop in
  let new_insts = Array.init ninsts new_inst in
  let active = ref [] in (* active mregs *)
  let inactive = ref [] in (* inactive mregs *)
  let vreg_to_mreg = Hashtbl.create 0 in
  let mreg_to_vreg = Hashtbl.create 0 in
  let (live_start, live_end) = calculate_live_points ctxt is in

  let inactivate_mreg (mreg: mreg) = 
    if Hashtbl.mem mreg_to_vreg mreg then
      let vreg = Hashtbl.find mreg_to_vreg mreg in
      Hashtbl.remove vreg_to_mreg vreg;
      Hashtbl.remove mreg_to_vreg mreg;
      active := List.filter (fun x -> x != mreg) (!active);
      inactive := mreg :: (!inactive)
    else
      ()
  in

  let activate_vreg (vreg: vreg): mreg =
    if Hashtbl.mem vreg_to_mreg vreg then
      Hashtbl.find vreg_to_mreg vreg
    else
      let mreg = alloc_mreg ctxt in
      assert (mreg != -1); (* TODO: spilling *)
      Hashtbl.add vreg_to_mreg vreg mreg;
      Hashtbl.add mreg_to_vreg mreg vreg;
      active := mreg :: (!active);
      inactive := List.filter (fun x -> x != mreg) (!inactive);
      mreg
  in

  let expire_interval (i: vreg) =
    Hashtbl.iter
    (fun j _ ->
      if live_end.(j) >= live_start.(i) hen ()
      else
        let mreg = Hashtbl.find vreg_to_mreg j in
        inactivate_mreg mreg) vreg_to_mreg
  in

  let visit_reg _ r =
    match r with
    | Vreg v ->
      expire_interval v;
      Mreg(activate_vreg v)
    | _ -> r
  in

  let iv = {Il.standard_visitor with
            visit_reg = visit_reg} in

  Array.iteri (fun i v -> new_insts.(i) <- visit_inst iv v) is;

  new_insts
;;
