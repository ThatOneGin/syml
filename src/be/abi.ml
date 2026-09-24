(*
 * Syml:
 *  abi defs
 *)
open Common;;
open Il;;

type t = {
    nth_reg_arg: t -> int -> mem option;
    fnarg_regs: int;
    farg_regs: mem list;
  }

let rax = 0;;
let rbx = 1;;
let rcx = 2;;
let rdx = 3;;
let rdi = 4;;
let rsi = 5;;
let r8 = 6;;
let r9 = 7;;
let r10 = 8;;
let r11 = 9;;
let r12 = 10;;
let r13 = 11;;
let r14 = 12;;
let r15 = 13;;

let x86_64_nth_reg_arg (a: t) (i: int): Il.mem option =
  List.nth_opt a.farg_regs i
;;

let create (a: target_arch): t =
  match a with
  | Linux_X86_64 ->
    let farg_regs = [
      Reg (Mreg rdi); Reg (Mreg rsi); Reg (Mreg rdx);
      Reg (Mreg rcx); Reg (Mreg r8); Reg (Mreg r9)
    ] in
    {
      farg_regs = farg_regs;
      fnarg_regs = List.length farg_regs;
      nth_reg_arg = x86_64_nth_reg_arg;
    }
;;