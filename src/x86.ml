(*
 * Syml:
 *  x86_64 IL code generator
 *)

(*
 * List of the majority/all general-purpose registers
 * should follow the order:
 * [8-bit; 16-bit; 32-bit; 64-bit]
 *)
let reg_table = [
  ["al"; "ax"; "eax"; "rax"];
  ["bl"; "bx"; "ebx"; "rbx"];
  ["cl"; "cx"; "ecx"; "rcx"];
  ["dl"; "dx"; "edx"; "rdx"];
  ["dil"; "di"; "edi"; "rdi"];
  ["sil"; "si"; "esi"; "rsi"];
  ["r8b"; "r8w"; "r8d"; "r8"];
  ["r9b"; "r9w"; "r9d"; "r9"];
  ["r10b"; "r10w"; "r10d"; "r10"];
  ["r11b"; "r11w"; "r11d"; "r11"];
  ["r12b"; "r12w"; "r12d"; "r12"];
  ["r13b"; "r13w"; "r13d"; "r13"];
  ["r14b"; "r14w"; "r14d"; "r14"];
  ["r15b"; "r15w"; "r15d"; "r15"];
]

exception Code_x86_64 of string

let getreg (b: Il.bits) (r: int): string =
  let i = match b with
    | Bits8 -> 0
    | Bits16 -> 1
    | Bits32 -> 2
    | Bits64 -> 3
  in
  (List.nth (List.nth reg_table r) i)

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
  | Rreg (i, b) -> "%" ^ getreg b i

let emit_val (s: Il.smod) (v: Il.value): string =
  match v with
  | Int i -> "$" ^ string_of_int i
  | Str _ ->
    let i: int = Il.smod_push_const s v in
    Printf.sprintf "$.LK%d" i
  | Var v -> begin
    match v.reg with
    | Some r -> emit_reg r
    | None -> Common.unreachable
      "x86_64 codegen"
      "missing register in variable descriptor"
  end

let emit_operand (s: Il.smod) (o: Il.operand): string =
  match o with
  | Reg ro -> begin
    match ro with
      | Some r -> emit_reg r
      | None -> raise
        (Code_x86_64
          "No register in instruction operand.")
  end
  | Val v -> emit_val s v

let get_cond_suffix (o: Ast.operator) (swap: bool): string =
  if swap then
    match o with
    | Ast.OEQU -> "ne"
    | Ast.ONEQ -> "e"
    | _ -> ""
  else
    match o with
    | Ast.OEQU -> "e"
    | Ast.ONEQ -> "ne"
    | _ -> ""

(* get result of a binary expression *)
let finish_binop (s: Il.smod) (b: Il.binop): unit =
  if not b.return_val then ()
  else
    let suffix = getmnemonicsuffix (Il.type2bits b.ty) in
    match b.op with
    | Ast.OEQU | Ast.ONEQ ->
      Il.smod_emit s
        (Printf.sprintf "\n\tset%s\t%%al"
          (get_cond_suffix b.op false));
      (if suffix != 'b' then
        Il.smod_emit s
          (Printf.sprintf "\n\tmovzb%c\t%%al,\t%s"
            suffix (emit_operand s b.left)))
    | _ -> ()

let emit_binop (s: Il.smod) (b: Il.binop): unit =
  let ins = match b.op with
    | Ast.OADD -> "add"
    | Ast.ODIV -> "idiv"
    | Ast.OMUL -> "imul"
    | Ast.OSUB -> "sub"
    | Ast.OEQU | Ast.ONEQ -> "cmp"
    | _ -> "; "
  in
  Il.smod_emit s
    (Printf.sprintf "%s%c\t%s,\t%s"
      ins
      (getmnemonicsuffix (Il.type2bits b.ty))
      (emit_operand s b.right)
      (emit_operand s b.left));
  finish_binop s b

let emit_jmp (s: Il.smod) (j: Il.jmp): unit =
  match j with
  | Je i -> Il.smod_emit s (Printf.sprintf "je\t.LC%d" i)
  | Jne i -> Il.smod_emit s (Printf.sprintf "jne\t.LC%d" i)
  | Test t ->
    let op = emit_operand s t.op in
    Il.smod_emit s (Printf.sprintf "cmpq\t$0,\t%s\n" op);
    Il.smod_emit s (Printf.sprintf "\tje\t.LC%d" t.jit)

let emit_move (s: Il.smod) (m: Il.move): unit =
  let dest: Il.reg =
    match m.dest with
    | Some r -> r
    | _ -> raise
      (Code_x86_64
        "No register found in instruction.")
  in
  Il.smod_emit s
    (Printf.sprintf "mov%c\t%s,\t%s"
      (getmnemonicsuffix (Il.type2bits m.ty))
      (emit_operand s m.src)
      (emit_reg dest))

let emit_ret (s: Il.smod) (r: Il.ret): unit = 
  let b: Il.bits = Il.type2bits r.ty in
    (* This doesn't handle values with size greater than 64-bits *)
    let op: string = emit_operand s r.value in
    let rr: string = "%" ^ getreg b 0 in
    if String.equal op rr then
      Il.smod_emit s "nop\n"
    else begin 
      Il.smod_emit s
        (Printf.sprintf "mov%c\t%s,\t%s\n"
          (getmnemonicsuffix b)
          (emit_operand s r.value)
          ("%" ^ getreg b 0))
    end;
    Il.smod_emit s (Printf.sprintf "\tjmp\t.LC%d" r.pc)

let emit_label (s: Il.smod) (l: Il.label): unit =
  match l with
    | Named_label nl ->
      (if nl.global then Il.smod_emit s (".globl " ^ nl.name ^ "\n"));
      Il.smod_emit s (Printf.sprintf "%s:" nl.name);
    | Unnamed_label id ->
      Il.smod_emit s (Printf.sprintf
        "/* label constant %d */\n.LC%d:" id id)

let emit_call (s: Il.smod) (c: Il.call): unit =
  if (Array.length c.args) > 0 then begin
    let f = fun (i: int) (o: Il.operand): unit ->
      Il.smod_emit s
      (Printf.sprintf "movq\t%%%s,\t%%%s"
      (emit_operand s o)
      (emit_reg c.regs.(i)))
    in
    Array.iteri f c.args;
    Il.smod_emit s (Printf.sprintf "\tcall\t%s" c.f)
  end else
    Il.smod_emit s (Printf.sprintf "call\t%s" c.f)

let emit_inst (s: Il.smod) (i: Il.inst): unit =
  emit_indent s;
  let () =
  match i with
  | Move m -> emit_move s m
  | Ret r -> emit_ret s r
  | Enter -> Il.smod_emit s "pushq\t%rbp\n\tmovq\t%rsp, %rbp"
  | Leave -> Il.smod_emit s "popq\t%rbp\n\tret";
  | Label l -> emit_label s l
  | Asm str -> Il.smod_emit s (Printf.sprintf "%s\t/* inline */" str)
  | Call c -> emit_call s c
  | Binop b -> emit_binop s b
  | Jmp j -> emit_jmp s j
  in
  emit_newline s

let emit_insts (s: Il.smod) (is: Il.insts): unit =
  let iterator = fun (i: Il.inst): unit ->
    emit_inst s i;
  in
  Array.iter iterator is;
  ()
let emit_constants (s: Il.smod): unit =
  let len: int = Array.length s.constants in
  let rec aux (i: int): unit =
    if i >= len then ()
    else
      match Array.get s.constants i with
      | Str str -> Il.smod_emit s
        (Printf.sprintf ".LK%d:\n\t.asciz \"%s\"\n" i str);
        aux (i + 1)
      | Int d -> Il.smod_emit s
        (Printf.sprintf ".LK%d:\n\t.long %d\n" i d);
        aux (i + 1)
      | _ -> ()
  in
  aux 0
