(*
 * Syml:
 *  x86_64 IL code generator
 *)

let reg_table = [
  "rax"; "rbx";
  "rcx"; "rdx";
  "rsp"; "rbp";
  "rsi"; "rdi";
  "r8"; "r9";
  "r10"; "r11";
  "r12"; "r13";
  "r14"; "r15";
]

let emit_indent (s: Il.smod): unit =
  Il.smod_emit s "\t"

let emit_label (s: Il.smod) (name: string) =
  Il.smod_emit s (Printf.sprintf "%s:\n" name)

let emit_reg (r: Il.reg): string =
  match r with
  | Spill i -> Printf.sprintf "-%d(%%rbp)" i
  | Rreg i -> List.nth reg_table i

let emit_val (v: Il.value): string =
  match v with
  | Int i -> "$" ^ string_of_int i
  | Str s -> s

let emit_operand (o: Il.operand): string =
  match o with
  | Reg r -> emit_reg r
  | Val v -> emit_val v

let emit_inst (s: Il.smod) (i: Il.inst): unit =
  emit_indent s;
  match i with
  | Move m ->
    Il.smod_emit s
      (Printf.sprintf "movq %s, %s" (emit_operand m.src) (emit_reg m.dest))
  | Ret r ->
    Il.smod_emit s
      (Printf.sprintf "movq %s, %%rax" (emit_operand r.value))

let emit_block (s: Il.smod) (b: Il.block): unit =
  Array.iter (fun (i: Il.inst): unit -> emit_inst s i) b.body; ()

let emit_func_prologue (s: Il.smod) (f: Il.sfunc): unit =
  emit_label s f.name;
  Il.smod_emit s (Printf.sprintf "\tpushq %%rbp\n\tmovq %%rsp, %%rbp\n"); ()

let emit_func_epilogue (s: Il.smod): unit =
  Il.smod_emit s (Printf.sprintf "\tpopq %%rbp\n\tret"); ()

let emit_func (s: Il.smod) (f: Il.sfunc): unit =
  emit_func_prologue s f;
  Array.iter (fun (b: Il.block): unit -> emit_block s b) f.body;
  emit_func_epilogue s;
