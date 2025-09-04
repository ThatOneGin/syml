(*
 * Syml:
 *  Intermediate language
 *)

type bits = 
  | Bits8
  | Bits16
  | Bits32
  | Bits64

type value =
  | Str of string
  | Int of int
and reg =
  | Spill of int
  | Rreg of int * bits
and operand =
  | Reg of reg option
  | Val of value

type move = {
    mutable dest: reg option;
    ty: Dtypes.datatype;
    src: operand;
  }

type ret = {
    ty: Dtypes.datatype;
    value: operand;
  }

type inst =
  | Move of move
  | Ret of ret
  | Enter (* enter next stack frame *)
  | Leave (* leave current stack frame *)

type insts = inst array

type smod = {
  modname: string;
  arch: Common.target_arch;
  nregs: int;
  mutable allocd_regs: int; (* unused *)
  mutable asm_buf: out_channel option;
  mutable sp: int; (* unused *)
}

let get_arch_nregister (a: Common.target_arch): int =
  match a with
  | Linux_X86_64 -> 16

let smod_create (name: string) (arch: Common.target_arch): smod =
   {modname = name;
    arch = arch;
    asm_buf = None;
    nregs = (get_arch_nregister arch);
    allocd_regs = 0;
    sp = 0;}

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

let type2bits (ty: Dtypes.datatype): bits =
  match ty with
  | I32 | Int -> Bits32
  | I64 | Str -> Bits64
  | Nil | I8 -> Bits8
  | I16 -> Bits16
