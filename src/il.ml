(*
 * Syml:
 *  Intermediate language
 *)

type value =
  | Str of string
  | Int of int
and reg =
  | Spill of int
  | Rreg of int
and operand =
  | Reg of reg
  | Val of value

type inst =
  | Move of {
      mutable dest: reg;
      src: operand;
    }
  | Ret of {
      ty: Dtypes.datatype;
      value: operand;
    }

type smod = {
  modname: string;
  arch: Common.target_arch;
  nregs: int;
  mutable allocd_regs: int;
  mutable asm_buf: out_channel option;
  mutable sp: int;
}

type block = {
  mutable body: inst array;
}
and sfunc = {
  name: string;
  mutable body: block array;
  ty: Dtypes.datatype;
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

let block_new: block =
  {
    body = [||];
  }

let block_append (b: block) (i: inst): unit =
  b.body <- Array.append b.body [|i|]; ()

let sfunc_new (name: string) (ty: Dtypes.datatype): sfunc =
  {
    name = name;
    ty = ty;
    body = [||];
  }

let sfunc_append (f: sfunc) (b: block): unit =
    f.body <- Array.append f.body [|b|]; ()