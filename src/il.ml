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
  | Var of vardesc
and reg =
  | Spill of int
  | Rreg of int * bits
and operand =
  | Reg of reg option
  | Val of value
and vardesc = {
    name: string;
    mutable reg: reg option;
  }

type move = {
    ty: Dtypes.datatype;
    mutable dest: reg option;
    mutable src: operand;
    name: string; (* additional data to use in ra.ml *)
  }

type label =
  | Named_label of {
      name: string;
      global: bool;
    }
  | Unnamed_label of int

type ret = {
    ty: Dtypes.datatype;
    mutable value: operand;
    mutable pc: int;
  }

type inst =
  | Move of move
  | Ret of ret
  | Enter (* enter next stack frame *)
  | Leave (* leave current stack frame *)
  | Label of label
  | Asm of string

type insts = inst array

type smod = {
  modname: string;
  arch: Common.target_arch;
  nregs: int;
  mutable asm_buf: out_channel option;
  mutable labelcount: int;
  mutable constants: value array;
}

let get_arch_nregister (a: Common.target_arch): int =
  match a with
  | Linux_X86_64 -> 14 (* amount of general-purpose registers *)

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
let smod_push_const (s: smod) (k: value): int =
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
  | I64 | Str -> Bits64
  | Nil | I8 -> Bits8
  | I16 -> Bits16

let bits2size (b: bits): int =
  match b with
  | Bits8 -> 1
  | Bits16 -> 2
  | Bits32 -> 4
  | Bits64 -> 8

(* IL printer for visualization *)

let reg2str (r: reg): string =
  match r with
  | Spill i -> Printf.sprintf "SP<%d>" i
  | Rreg (i, _) -> Printf.sprintf "R<%d>" i

let opt_reg2str (r: reg option): string =
  match r with
  | Some rr -> reg2str rr
  | None -> "R<nil>"

let val2str (v: value): string =
  match v with
  | Str s -> "\"" ^ s ^ "\""
  | Int i -> string_of_int i
  | Var v -> v.name

let op2str (o: operand): string =
  match o with
  | Reg r -> opt_reg2str r
  | Val v -> val2str v

let label2str (l: label): string =
  match l with
  | Named_label nl -> nl.name
  | Unnamed_label id -> "LC<" ^ (string_of_int id) ^ ">"

let print_insts (is: insts): unit =
  let f =
    fun (i: inst): unit ->
      print_char '\t';
      begin
        match i with
          | Move m ->
            Printf.printf "%s <- %s ; %s"
              m.name
              (op2str m.src)
              (opt_reg2str m.dest)
          | Ret r ->
            Printf.printf "ret %s"
              (op2str r.value)
          | Enter -> print_endline "Enter"
          | Leave -> print_endline "Leave"
          | Label l ->
            Printf.printf "\r%s:"
              (label2str l)
          | Asm s -> Printf.printf "%s" s
      end;
      print_newline ()
  in
  Array.iter f is;
  ()
