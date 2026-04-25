(*
 * Syml:
 *  Intermediate language
 *)

type bits = 
  | Bits8
  | Bits16
  | Bits32
  | Bits64

(* type of a reference (operand, immediate) *)
type ref_ty =
  | Val_ty of bits
  | Ptr_ty of ref_ty
  | Func_ty (* we will probably want this to be typed later *)
  | Nil_ty

type vreg = int
type mreg = int
type stack = int
type imm = int64

type reg =
  | Vreg of vreg
  | Mreg of mreg

type mem =
  | Addr of int (* constant *)
  | Reg of reg
  | Stack of stack
  | Name of string (* if it is typed_mem, the bits type does not matter *)

type typed_imm = (imm * ref_ty)
type typed_mem = (mem * ref_ty)

(* operand = mem | immediate *)
type operand =
  | Mem of typed_mem
  | Imm of typed_imm

(* pseudo-instruction used to allocate a stack-slot for vreg 'dest' *)
type alloca = {
    ty: Dtypes.datatype;
    dest: vreg;
  }

type move = {
    dest: mem;
    src: operand;
  }

type ltype =
  | Lfunction
  | Lobject
  | Lnone

type label =
  | Named_label of {
      name: string;
      global: bool;
      ltype: ltype;
    }
  | Unnamed_label of int

type ret = {
    ty: Dtypes.datatype;
    value: operand;
    mutable pc: int;
  }

(*
 * call arguments are pushed to the stack
 * in reverse order.
 *
 * 'f' field should be changed to a 'operand' later on
 * but for simplicity it will be a string for now
 *)
type call = {
    f: operand;
    args: operand array;
  }

type binop = {
    left: operand;
    right: operand;
    op: Ast.operator;
    ty: Dtypes.datatype;
    return_val: bool;
  }

type test = {
    op: operand; (* operand to test *)
    jit: int; (* jump if true *)
  }

type jmp =
  (* conditional jumps  *)
  | Je of int
  | Jne of int
  | Test of test
  | Jump of int (* inconditional jump *)

(* generate a lea or a lea equivalent instruction *)
type lea = {
    src: operand;
    dest: mem;
  }

type asm = {
    code: string;
    inputs: operand array;
  }

type inst =
  | Alloca of alloca
  | Move of move
  | Ret of ret
  | Enter of int64 (* enter next stack frame *)
  | Leave          (* leave current stack frame *)
  | Label of label
  | Asm of asm
  | Lea of lea
  | Call of call
  | Binop of binop
  | Jmp of jmp
  | Nop

type insts = inst array

type smod = {
    modname: string;
    arch: Common.target_arch;
    nregs: int;
    mutable asm_buf: out_channel option;
    mutable labelcount: int;
    mutable constants: string array;
  }

let get_arch_nregister (a: Common.target_arch): int =
  match a with
  | Linux_X86_64 -> 14 (* amount of general-purpose registers *)
;;

let get_arch_stack_align (a: Common.target_arch): int =
  match a with
  | Linux_X86_64 -> 16
;;

let smod_create (name: string) (arch: Common.target_arch): smod =
   {modname = name;
    arch = arch;
    asm_buf = None;
    nregs = (get_arch_nregister arch);
    labelcount = 0;
    constants = [||]}
;;

let smod_newlabel (smod: smod) (global: bool) =
  if not global then begin
    smod.labelcount <- smod.labelcount + 1; ()
  end else
    ()
;;

let smod_open_out (s: smod) (name: string): unit =
  let fd = open_out name in
  s.asm_buf <- (Some fd); ()
;;

let smod_close_out (s: smod): unit =
  match s.asm_buf with
  | Some fd -> close_out fd; ()
  | None -> ()
;;

let smod_emit (s: smod) (b: string): unit =
  let buf =
    match s.asm_buf with
    | Some fd -> fd;
    | None -> Common.syml_errorf
      "trying to write on a non existent buffer.\n"
  in
  Printf.fprintf buf "%s" b; ()
;;

(* push a value to the constants table *)
let smod_push_const (s: smod) (k: string): int =
  let len = Array.length s.constants in
  let rec aux (i: int) =
    if i >= len then None
    else if Array.get s.constants i = k then Some i
    else aux (i+1)
  in
  match aux 0 with
  | Some i -> i
  | None ->
    s.constants <- Array.append s.constants [|k|]; 
    len
;;

(* pre-made types *)
let (int_t:ref_ty) = Val_ty Bits32;;
let (i8_t:ref_ty) = Val_ty Bits8;;
let (i16_t:ref_ty) = Val_ty Bits16;;
let (i32_t:ref_ty) = Val_ty Bits32;;
let (i64_t:ref_ty) = Val_ty Bits64;;
let (str_t:ref_ty) = Ptr_ty (Val_ty Bits64);;
let (nil_t:ref_ty) = Nil_ty;;
let (fptr_t:ref_ty) = Func_ty;;
let ptr_t (t:ref_ty): ref_ty = Ptr_ty t

let rec ref_of_type (ty: Dtypes.datatype): ref_ty =
  match ty with
  | Int -> int_t
  | I8 -> i8_t
  | I16 -> i16_t
  | I32 -> i32_t
  | I64 -> i64_t
  | Ptr t -> ptr_t @@ ref_of_type t
  | Str -> str_t
  | Nil -> nil_t
  | Fptr _ -> fptr_t
;;

let type2bits (ty: Dtypes.datatype): bits =
  match ty with
  | I32 | Int -> Bits32
  | Fptr _ | Ptr _ | I64 | Str -> Bits64
  | Nil | I8 -> Bits8
  | I16 -> Bits16
;;

let bits2size (b: bits): int =
  match b with
  | Bits8 -> 1
  | Bits16 -> 2
  | Bits32 -> 4
  | Bits64 -> 8
;;

let bits_of_ref_ty (rt: ref_ty): bits =
  match rt with
  | Val_ty b -> b
  | Nil_ty -> Bits8
  | Func_ty -> Bits64
  | Ptr_ty _ -> Bits64

let size_of_ref_ty (rt: ref_ty): int =
  match rt with
  | Nil_ty -> 0
  | Val_ty b -> bits2size b
  | _ -> 8

let ref_of_operand (o: operand): ref_ty =
  match o with
  | Mem (_, rt) -> rt
  | Imm (_, rt) -> rt
;;

let vreg_of_mem (m: mem): vreg =
  match m with
  | (Reg (Vreg v)) -> v
  | _ -> assert false
;;

(* IL printer for visualization *)

let reg2str (r: reg): string =
  match r with 
  | Vreg v -> Printf.sprintf "%%%d" v
  | Mreg m -> Printf.sprintf "R%d" m
;;

let mem2str (m: mem): string =
  match m with
  | Addr a -> Printf.sprintf "LK[%d]" a
  | Reg r -> reg2str r
  | Stack s -> Printf.sprintf "[sp:%d]" s
  | Name s -> s
;;

let op2str (o: operand): string =
  match o with
  | Imm (i, _) -> Printf.sprintf "%Ld" i
  | Mem (m, _) -> mem2str m
;;

let label2str (l: label): string =
  match l with
  | Named_label nl -> nl.name
  | Unnamed_label id -> "LC<" ^ (string_of_int id) ^ ">"
;;

let print_jmp (j: jmp) =
  match j with
  | Je i -> Printf.printf "je %d" i
  | Jne i -> Printf.printf "jne %d" i
  | Test t -> Printf.printf "test %s, LC<%d>" (op2str t.op) t.jit
  | Jump i -> Printf.printf "jmp %d" i
;;

let print_insts (is: insts): unit =
  let f =
    fun (i: inst): unit ->
      print_char '\t';
      begin
        match i with
          | Alloca a ->
            Printf.printf "%s <- alloca %s"
              (reg2str (Vreg a.dest)) (Dtypes.type2str a.ty)
          | Move m ->
            Printf.printf "%s <- %s"
              (mem2str m.dest)
              (op2str m.src)
          | Ret r ->
            Printf.printf "ret %s"
              (op2str r.value)
          | Enter _ -> print_endline "Enter"
          | Leave -> print_endline "Leave"
          | Label l ->
            Printf.printf "\r%s:"
              (label2str l)
          | Asm a -> Printf.printf "%s" a.code
          | Lea l ->
            Printf.printf "lea %s, %s"
              (mem2str l.dest) (op2str l.src)
          | Call c -> Printf.printf "call %s" @@ op2str c.f
          | Binop b ->
            let op = match b.op with
              | Ast.OADD -> "add"
              | Ast.ODIV -> "div"
              | Ast.OMUL -> "mul"
              | Ast.OSUB -> "sub"
              | Ast.OEQU -> "equ"
              | Ast.ONEQ -> "neq"
              | _ -> "invalid"
            in
            Printf.printf "%s %s, %s"
              op
              (op2str b.left)
              (op2str b.right)
          | Jmp j -> print_jmp j
          | Nop -> print_endline "nop"
      end;
      print_newline ()
  in
  Array.iter f is;
  ()
;;

(*
 * Traverse an inst array with pre defined routines
 * also used to modify certain instructions
 **)
type inst_visitor = {
    visit_reg: (inst_visitor -> reg -> reg);
    visit_mem: (inst_visitor -> mem -> mem);
    visit_opr: (inst_visitor -> operand -> operand);
  }

(*
 * standard visitor:
 *  contains the base functions to traverse the IL instructions
 *  they really don't do much unless you modify them
 *)
let standard_visitor =
  let visit_reg _ r = r in
  let visit_mem iv m =
    match m with
    | Reg r -> Reg (iv.visit_reg iv r)
    | _ -> m
  in
  let visit_opr iv o = match o with
    | Mem (m, t) -> Mem (iv.visit_mem iv m, t)
    | _ -> o
  in
  {visit_reg = visit_reg;
   visit_mem = visit_mem;
   visit_opr = visit_opr}
;;

let visit_inst (iv: inst_visitor) (i: inst): inst =
  match i with
  | Alloca _ -> i (* pseudo-instruction *)
  | Move m -> Move {dest = iv.visit_mem iv m.dest;
                    src = iv.visit_opr iv m.src}
  | Ret r -> Ret {r with
                  value = iv.visit_opr iv r.value;}
  | Enter x -> Enter x
  | Leave -> Leave
  | Label l -> Label l
  | Asm a -> Asm {a with
                  inputs = Array.map (fun x -> iv.visit_opr iv x) a.inputs;}
  | Lea l -> Lea {dest = iv.visit_mem iv l.dest;
                  src = iv.visit_opr iv l.src}
  | Call c -> Call {f = iv.visit_opr iv c.f;
                    args = Array.map (fun x -> iv.visit_opr iv x) c.args;}
  | Binop b -> Binop {b with
                      left = iv.visit_opr iv b.left;
                      right = iv.visit_opr iv b.right;}
  | Jmp j -> Jmp j
  | Nop -> Nop
;;
