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

let abi =
  Abi.create Common.Linux_X86_64
;;

exception Code_x86_64 of string

let getreg (b: Il.bits) (r: Il.reg): string =
  let i = match b with
    | Bits8 -> 0
    | Bits16 -> 1
    | Bits32 -> 2
    | Bits64 -> 3
  in
  match r with
  | Mreg m -> (List.nth (List.nth reg_table m) i)
  | _ -> Common.unreachable "x86_64 assembly generation" "Invalid IL register"
;;

(* to get instruction mnemonic suffix (if any) *)
let getmnemonicsuffix (b: Il.bits): char =
  match b with
  | Bits8 -> 'b'
  | Bits16 -> 'w'
  | Bits32 -> 'l'
  | Bits64 -> 'q'
;;

let emit_indent (s: Il.smod): unit = Il.smod_emit s "\t";;

let emit_newline (s: Il.smod): unit = Il.smod_emit s "\n";;

let emit_imm (i: Il.imm): string = Printf.sprintf "$%Ld" i;;

let emit_mem (m: Il.mem) (ty: Il.bits): string =
  match m with
  | Addr a -> Printf.sprintf "$.LK%d" a
  | Reg r -> "%" ^ getreg ty r
  | Stack s -> Printf.sprintf "%d(%%rbp)" s
  | Name s -> s
;;

let emit_operand (o: Il.operand): string =
  match o with
  | Mem (m, t) -> emit_mem m @@ Il.bits_of_ref_ty t
  | Imm (i, _) -> emit_imm i
;;

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
;;

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
          (Printf.sprintf "\n\tmovzb%c\t%%al, %s"
            suffix (emit_operand b.left)))
    | _ -> ()
;;

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
    (Printf.sprintf "\t%s%c\t%s,\t%s"
      ins
      (getmnemonicsuffix (Il.type2bits b.ty))
      (emit_operand b.right)
      (emit_operand b.left));
  finish_binop s b
;;

let emit_jmp (s: Il.smod) (j: Il.jmp): unit =
  match j with
  | Je i -> Il.smod_emit s (Printf.sprintf "\tje\t.LC%d" i)
  | Jne i -> Il.smod_emit s (Printf.sprintf "\tjne\t.LC%d" i)
  | Test t ->
    let op = emit_operand t.op in
    Il.smod_emit s (Printf.sprintf "\tcmpq\t$0, %s\n" op);
    Il.smod_emit s (Printf.sprintf "\tje\t.LC%d" t.jit)
  | Jump i -> Il.smod_emit s (Printf.sprintf "jmp\t.LC%d" i)
;;

let emit_move (s: Il.smod) (m: Il.move): unit =
  let bits = Il.bits_of_ref_ty @@ Il.ref_of_operand m.src in
  Il.smod_emit s
    (Printf.sprintf "\tmov%c\t%s,\t%s"
      (getmnemonicsuffix bits)
      (emit_operand m.src)
      (emit_mem m.dest bits))
;;

let emit_ret (s: Il.smod) (r: Il.ret): unit = 
  let b: Il.bits = Il.type2bits r.ty in
    (* This doesn't handle values with size greater than 64-bits *)
    let op: string = emit_operand r.value in
    let rr: string = "%" ^ getreg b (Mreg 0) in
    if String.equal op rr then
      Il.smod_emit s "\tnop\n"
    else begin 
      Il.smod_emit s
        (Printf.sprintf "\tmov%c\t%s,\t%s\n"
          (getmnemonicsuffix b)
          (emit_operand r.value)
          ("%" ^ getreg b (Mreg 0)))
    end;
    Il.smod_emit s (Printf.sprintf "\tjmp\t.LC%d" r.pc)
;;

let emit_label (s: Il.smod) (l: Il.label): unit =
  match l with
  | Named_label name ->
    Il.smod_emit s (Printf.sprintf "%s:" name);
  | Unnamed_label id ->
    Il.smod_emit s (Printf.sprintf
      ".LC%d:" id)
;;

let emit_arg (s: Il.smod) (a: Il.operand) (i: int): unit =
  match abi.nth_reg_arg abi i with
  | Some r ->
    Il.smod_emit s
      (Printf.sprintf "\tmovq\t%s,\t%s\n"
      (emit_operand a)
      (emit_mem r Bits64))
  | None -> ()
;;

let emit_args (s: Il.smod) (a: Il.operand array): unit =
  let len = Array.length a in
  let nreg_args = min len abi.fnarg_regs in
  for i = 0 to nreg_args - 1 do
    emit_arg s a.(i) i
  done;
  (* push the rest in the inverse order *)
  for i = len - 1 downto nreg_args do
    Il.smod_emit s
      (Printf.sprintf "\tpushq\t%s\n"
        (emit_operand a.(i)))
  done
;;

let emit_call (s: Il.smod) (c: Il.call): unit =
  emit_args s c.args;
  Il.smod_emit s (Printf.sprintf "\tcall\t%s" @@ emit_operand c.f)
;;

let emit_lea (s: Il.smod) (l: Il.lea): unit =
  let dest =
    emit_mem l.dest Bits64
  in
  let src = 
    match l.src with
    | Mem ((Addr a), _) -> Printf.sprintf ".LK%d(%%rip)" a
    | _ -> emit_operand l.src
  in
  Il.smod_emit s (Printf.sprintf "\tlea%c\t%s,\t%s" (getmnemonicsuffix Bits64) src dest)
;;

let fepilogue (s: Il.smod) (n: int64): unit =
  Il.smod_emit s "\tpushq\t%rbp\n";
  Il.smod_emit s "\tmovq\t%rsp,\t%rbp";
  if n > 0L
  then begin
    emit_newline s;
    Il.smod_emit s @@
      Printf.sprintf "\tsubq\t$%Ld,\t%%rsp" n
  end else ()
;;

let fprologue (s: Il.smod): unit =
  Il.smod_emit s "\tleave\n";
  Il.smod_emit s "\tret"
;;

(* TODO: not use Str package to do this *)
let emit_asm_re = Str.regexp "{\\([0-9]+\\)}";;
let emit_asm (s: Il.smod) (a: Il.asm): unit =
  let res =
    Str.global_substitute emit_asm_re (fun s ->
      let v = int_of_string (Str.matched_group 1 s) in
      if v >= Array.length a.inputs
      then Common.syml_errorf "invalid inline assembly index %d" v
      else emit_operand (a.inputs.(v))
    ) a.code
  in
  emit_indent s;
  Il.smod_emit s res
;;

let string_of_ltype (l: Il.ltype) =
  match l with
  | Lfunction -> Some "@function"
  | Lobject -> Some "@object"
  | Lnone -> None
;;

let emit_directive (s: Il.smod) (d: Il.directive): unit =
  match d with
  | File name -> Il.smod_emit s (Printf.sprintf ".file\t\"%s\"" name)
  | Size (symbol, size) -> Il.smod_emit s (Printf.sprintf".size\t%s,\t%s" symbol size)
  | Align (symbol, align) -> Il.smod_emit s (Printf.sprintf ".align\t%s,\t%d" symbol align)
  | Global symbol -> Il.smod_emit s (Printf.sprintf ".globl\t%s" symbol)
  | Local symbol -> Il.smod_emit s (Printf.sprintf ".local\t%s" symbol)
  | Type (symbol, t) ->
    begin match string_of_ltype t with
      | Some st -> Il.smod_emit s (Printf.sprintf ".type\t%s,\t%s" symbol st)
      | None -> Il.smod_emit s (Printf.sprintf "/* .type\t%s, %s*/" symbol "@none")
    end
  | Text -> Il.smod_emit s ".text"
;;

let emit_section (s: Il.smod) (se: string list): unit =
  assert (List.length se > 0);
  Il.smod_emit s (Printf.sprintf ".section\t" );
  Il.smod_emit s (String.concat "," se)
;;

let emit_prop (s: Il.smod) (p: Il.prop): unit =
  match p with
  | Directive d -> emit_directive s d
  | Section se -> emit_section s se
;;

let emit_inst (s: Il.smod) (i: Il.inst): unit =
  let () =
  match i with
  | Move m -> emit_move s m
  | Ret r -> emit_ret s r
  | Enter x -> fepilogue s x
  | Leave -> fprologue s
  | Label l -> emit_label s l
  | Asm a -> emit_asm s a
  | Call c -> emit_call s c
  | Binop b -> emit_binop s b
  | Jmp j -> emit_jmp s j
  | Nop -> Il.smod_emit s "/* nop */"
  | Prop p -> emit_prop s p
  | Alloca a -> Il.smod_emit s (Printf.sprintf "/* alloca %%%d %s */" a.dest (Dtypes.type2str a.ty))
  | Lea l -> emit_lea s l
  in
  emit_newline s
;;

let emit_insts (s: Il.smod) (is: Il.insts): unit =
  let iterator = fun (i: Il.inst): unit ->
    emit_inst s i;
  in
  Array.iter iterator is;
;;

let rec emit_const (s: Il.smod) (name: string) (k: Il.const): unit =
  match k with
  | K_string str -> emit_const_string s name str
  | K_integer (int, bits) -> emit_const_int s name int bits

and emit_const_string (s: Il.smod) (name: string) (str: string): unit =
  Il.smod_emit s
    (Printf.sprintf "%s:\n\t.asciz\t\"%s\"\n" name str)

and emit_const_int (s: Il.smod) (name: string) (i: int64) (b: Il.bits): unit =
  let directive =
    match b with
    | Bits8 -> ".byte"
    | Bits16 -> ".hword"
    | Bits32 -> ".long"
    | Bits64 -> ".quad"
  in
  Il.smod_emit s
    (Printf.sprintf "%s:\n\t%s\t%Ld\n" name directive i)
;;

let emit_constants (s: Il.smod): unit =
  let len: int = Array.length s.constants in
  let rec aux (i: int): unit =
    if i >= len then ()
    else
      let const = Array.get s.constants i in
      emit_const s (Printf.sprintf ".LK%d" i) const;
      aux (i + 1)
  in
  if len > 0 then Il.smod_emit s ".section\t.rodata\n"
  else ();
  aux 0

let emit_globals (s: Il.smod): unit =
  let len: int = Array.length s.globals in
  let rec aux (i: int): unit =
    if i >= len then ()
    else
      let (name, const) = Array.get s.globals i in
      emit_const s name const;
      aux (i + 1)
  in
  if len > 0 then Il.smod_emit s ".section\t.rodata\n"
  else ();
  aux 0
;;