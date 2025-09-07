(*
 * Syml:
 *  x86_64 IL code generator
 *)

 (*
  * these tables are temporary, later will be only one
  * to avoid this madness.
  *)
let reg_table8 = [
  "al"; "bl";
  "cl"; "dl";
  "sil"; "dil";
  "bpl"; "spl";
  "r8b"; "r9b";
  "r10b"; "r11b";
  "r12b"; "r13b";
  "r14b"; "r15b";
]
and reg_table16 = [
  "ax"; "bx";
  "cx"; "dx";
  "si"; "di";
  "bp"; "sp";
  "r8w"; "r9w";
  "r10w"; "r11w";
  "r12w"; "r13w";
  "r14w"; "r15w";
]
and reg_table32 = [
  "eax"; "ebx";
  "ecx"; "edx";
  "esi"; "edi";
  "ebp"; "esp";
  "r8d"; "r9d";
  "r10d"; "r11d";
  "r12d"; "r13d";
  "r14d"; "r15d";
]
and reg_table64 = [
  "rax"; "rbx";
  "rcx"; "rdx";
  "rsp"; "rbp";
  "rsi"; "rdi";
  "r8"; "r9";
  "r10"; "r11";
  "r12"; "r13";
  "r14"; "r15";
]

exception Code_x86_64 of string

let bits2reglist (b: Il.bits): string list =
  match b with
  | Bits8 -> reg_table8
  | Bits16 -> reg_table16
  | Bits32 -> reg_table32
  | Bits64 -> reg_table64

(* to get instruction mnemonic suffix (if any) *)
let getmnemonicsuffix (b: Il.bits): char =
  match b with
  | Bits8 -> 'b'
  | Bits16 -> 'w'
  | Bits32 -> 'l'
  | Bits64 -> 'q'

let emit_indent (s: Il.smod): unit =
  Il.smod_emit s "\t"

let emit_newline (s: Il.smod): unit =
  Il.smod_emit s "\n"

let emit_reg (r: Il.reg): string =
  match r with
  | Spill i -> Printf.sprintf "-%d(%%rbp)" i
  | Rreg (i, b) -> List.nth (bits2reglist b) i

let emit_val (v: Il.value): string =
  match v with
  | Int i -> "$" ^ string_of_int i
  | Str _ -> ".LK0" (* not implemented *)
  | Var v -> begin
    match v.reg with
    | Some r -> emit_reg r
    | None -> Common.unreachable
      "x86_64 codegen"
      "missing register in variable descriptor"
  end

let emit_operand (o: Il.operand): string =
  match o with
  | Reg ro -> begin
    match ro with
      | Some r -> emit_reg r
      | None -> raise
        (Code_x86_64
          "No register in instruction operand.")
  end
  | Val v -> emit_val v

let emit_inst (s: Il.smod) (i: Il.inst): unit =
  emit_indent s;
  let () =
  match i with
  | Move m ->
    let dest: Il.reg =
      match m.dest with
      | Some r -> r
      | _ -> raise
        (Code_x86_64
          "No register found in instruction.")
    in
    Il.smod_emit s
      (Printf.sprintf "mov%c\t%s, %%%s"
        (getmnemonicsuffix (Il.type2bits m.ty))
        (emit_operand m.src)
        (emit_reg dest))
  | Ret r ->
    let b: Il.bits = Il.type2bits r.ty in
    (* This doesn't handle values with size greater than 64-bits *)
    Il.smod_emit s
      (Printf.sprintf "mov%c\t%s, %%%s"
        (getmnemonicsuffix b)
        (emit_operand r.value)
        (List.nth (bits2reglist b) 0))
  | Enter ->
    Il.smod_emit s "pushq\t%rbp\n";
    Il.smod_emit s "\tmovq\t%rsp, %rbp"
  | Leave ->
    Il.smod_emit s "popq\t%rbp\n";
    Il.smod_emit s "\tret"
  in
  emit_newline s

let emit_insts (s: Il.smod) (is: Il.insts): unit =
  let iterator = fun (i: Il.inst): unit ->
    emit_inst s i;
  in
  Array.iter iterator is;
  ()

let emit_label (s: Il.smod) (l: Il.label): unit =
  let () =
    match l.name with
    | Some name -> Il.smod_emit s (Printf.sprintf "%s:\n" name);
    | None -> Il.smod_emit s (Printf.sprintf ".LC%d:\n" s.labelcount);
  in
  emit_insts s l.body
