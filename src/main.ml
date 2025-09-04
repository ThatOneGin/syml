(*
 * Syml:
 *  Compiler driver
 *)

let () =
(*let ls: lex_State = lex_new "main" "def main(): nil {let x: i32 = 3;}" in
  let ps: parser_State = ps_new ls in
  let _: Ast.toplevel = parse_func ps in*)

  let smod: Il.smod = Il.smod_create "main" Common.Linux_X86_64 in
  Il.smod_open_out smod "main.s";
  let i: Il.inst = Move { (* movl $0, %eax *)
    dest = Some (Rreg (0, Bits32));
    ty = I32;
    src = Val (Il.Int 0);
  } in
  X86.emit_inst smod Enter;
  X86.emit_inst smod i;
  X86.emit_inst smod Leave;
  Il.smod_close_out smod;
  ()
