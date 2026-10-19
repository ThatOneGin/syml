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
    sizeof: ref_ty -> int;
    create_struct: ref_ty list -> struct_layout;
    reg_max_siz: bits;
  }

module Abi_x86 = struct
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

  let nth_reg_arg (a: t) (i: int): Il.mem option =
    List.nth_opt a.farg_regs i
  ;;

  let sizeof (t: ref_ty): int =
    match t with
    | Val_ty b -> bits2size b
    | Ptr_ty _ | Func_ty -> 8
    | Nil_ty -> 1
    | Struct_ty s -> s.size
  ;;

  let alignof = function
    | Val_ty b -> bits2size b
    | Ptr_ty _ | Func_ty -> 8
    | Nil_ty -> 1
    | Struct_ty s -> s.align
  ;;

  let align_of_list (li: ref_ty list): int =
    let ili = List.map alignof li in
    List.fold_left max 1 ili
  ;;

  let size_of_list (align: int) (li: ref_ty list): int =
    let ili = List.map sizeof li in
    let sz =
      Common.fold_lefti (fun i h _ ->
        let fi = List.nth li i in
        let s = sizeof fi in
        let a = alignof fi in
        let nh = Common.round h a in
        nh + s
      ) 0 ili
    in
    Common.round sz align
  ;;

  let offset_of_list (li: ref_ty list): int list =
    let ili = List.map sizeof li in
    let oli = Array.init (List.length ili) (fun _ -> 0) in
    let _ = 
      Common.fold_lefti (fun i h _ ->
        let fi = List.nth li i in
        let s = sizeof fi in
        let a = alignof fi in
        let nh = Common.round h a in
        oli.(i) <- nh;
        nh + s
      ) 0 ili
    in
    List.init (Array.length oli) (fun i -> oli.(i))
  ;;

  let create_struct (li: ref_ty list): struct_layout =
    let align = align_of_list li in
    let size = size_of_list align li in
    let offsets = offset_of_list li in
    {
      fields = li;
      offsets = offsets;
      align = align;
      size = size;
    }
  ;;

  let farg_regs = [
    Reg (Mreg rdi); Reg (Mreg rsi); Reg (Mreg rdx);
    Reg (Mreg rcx); Reg (Mreg r8); Reg (Mreg r9)
  ];;

  let create (): t =
    {
      farg_regs = farg_regs;
      fnarg_regs = List.length farg_regs;
      nth_reg_arg = nth_reg_arg;
      sizeof = sizeof;
      create_struct = create_struct;
      reg_max_siz = Bits64;
    }
  ;;
end

let create (a: target_arch): t =
  match a with
  | Linux_X86_64 -> Abi_x86.create ()
;;