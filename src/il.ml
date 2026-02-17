(*
 * Syml:
 *  Intermediate language
 *)

type bits = 
  | Bits8
  | Bits16
  | Bits32
  | Bits64

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

type typed_imm = (imm * bits)
type typed_mem = (mem * bits)

(* operand = mem | immediate *)
type operand =
  | Mem of typed_mem
  | Imm of typed_imm

type move = {
    dest: mem;
    src: operand;
  }

type label =
  | Named_label of {
      name: string;
      global: bool;
    }
  | Unnamed_label of int

type ret = {
    ty: Dtypes.datatype;
    value: operand;
    mutable pc: int;
  }

type call = {
    f: string;
    mutable args: operand array; (* function arguments *)
    mutable regs: reg array; (* function arguments destination *)
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

type inst =
  | Move of move
  | Ret of ret
  | Enter (* enter next stack frame *)
  | Leave (* leave current stack frame *)
  | Label of label
  | Asm of string
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

let get_arch_funcargs (a: Common.target_arch): reg list =
  match a with
  | Linux_X86_64 ->
    [Mreg 4; Mreg 5; (* rdi  rsi *)
     Mreg 3; Mreg 2; (* rdx, rcx *)
     Mreg 6; Mreg 7] (* r8, r9   *)

let smod_create (name: string) (arch: Common.target_arch): smod =
   {modname = name;
    arch = arch;
    asm_buf = None;
    nregs = (get_arch_nregister arch);
    labelcount = 0;
    constants = [||]}

let smod_newlabel (smod: smod) (global: bool) =
  if not global then begin
    smod.labelcount <- smod.labelcount + 1; ()
  end else
    ()

let smod_open_out (s: smod) (name: string): unit =
  let fd = open_out name in
  s.asm_buf <- (Some fd); ()

let smod_close_out (s: smod): unit =
  match s.asm_buf with
  | Some fd -> close_out fd; ()
  | None -> ()

let smod_emit (s: smod) (b: string): unit =
  let buf =
    match s.asm_buf with
    | Some fd -> fd;
    | None -> Common.syml_errorf
      "trying to write on a non existent buffer.\n"
  in
  Printf.fprintf buf "%s" b; ()

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

let type2bits (ty: Dtypes.datatype): bits =
  match ty with
  | I32 | Int -> Bits32
  | Ptr _ | I64 | Str -> Bits64
  | Nil | I8 -> Bits8
  | I16 -> Bits16

let bits2size (b: bits): int =
  match b with
  | Bits8 -> 1
  | Bits16 -> 2
  | Bits32 -> 4
  | Bits64 -> 8

let get_operand_type (o: operand): bits =
  match o with
  | Mem (_, b)
  | Imm (_, b) -> b

(* IL printer for visualization *)

let reg2str (r: reg): string =
  match r with 
  | Vreg v -> Printf.sprintf "%%%d" v
  | Mreg m -> Printf.sprintf "R%d" m

let mem2str (m: mem): string =
  match m with
  | Addr a -> Printf.sprintf "LK[%d]" a
  | Reg r -> reg2str r
  | Stack s -> Printf.sprintf "[sp:%d]" s

let op2str (o: operand): string =
  match o with
  | Imm (i, _) -> Printf.sprintf "%Ld" i
  | Mem (m, _) -> mem2str m

let label2str (l: label): string =
  match l with
  | Named_label nl -> nl.name
  | Unnamed_label id -> "LC<" ^ (string_of_int id) ^ ">"

let print_jmp (j: jmp) =
  match j with
  | Je i -> Printf.printf "je %d" i
  | Jne i -> Printf.printf "jne %d" i
  | Test t -> Printf.printf "test %s, LC<%d>" (op2str t.op) t.jit
  | Jump i -> Printf.printf "jmp %d" i

let print_insts (is: insts): unit =
  let f =
    fun (i: inst): unit ->
      print_char '\t';
      begin
        match i with
          | Move m ->
            Printf.printf "%s <- %s"
              (mem2str m.dest)
              (op2str m.src)
          | Ret r ->
            Printf.printf "ret %s"
              (op2str r.value)
          | Enter -> print_endline "Enter"
          | Leave -> print_endline "Leave"
          | Label l ->
            Printf.printf "\r%s:"
              (label2str l)
          | Asm s -> Printf.printf "%s" s
          | Call c -> Printf.printf "call %s" c.f
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
  | Move m -> Move {dest = iv.visit_mem iv m.dest;
                    src = iv.visit_opr iv m.src}
  | Ret r -> Ret {r with
                  value = iv.visit_opr iv r.value;}
  | Enter -> Enter
  | Leave -> Leave
  | Label l -> Label l
  | Asm s -> Asm s
  | Call c -> Call c
  | Binop b -> Binop {b with
                      left = iv.visit_opr iv b.left;
                      right = iv.visit_opr iv b.right;}
  | Jmp j -> Jmp j
  | Nop -> Nop