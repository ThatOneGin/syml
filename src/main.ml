(* Syml:
 *  main routines
 *)

open Common

let (target:target_arch) =
  match Sys.os_type with
  (* TODO: differentiate between Unix-like systems (free-bsd, macos and linux) *)
  | "Unix" -> Common.Linux_X86_64
  | _ -> Common.Linux_X86_64
;;

let (opts:Comp_state.t) = {
    no_rt = false;
    keep_asm_files = false;
    compile_only = false;
    log_il = false;
    log_ra = false;
    out_file = ref "";
    in_files = [];
    target = target;
  }
;;

(* custom set flag *)
let flag fl fn dsc =
  (fl, Arg.Unit fn, dsc)
;;

let flag_str fl st dsc = 
  (fl, Arg.Set_string st, dsc)
;;

let specs = [
  flag "-no-rt" (fun _ -> opts.no_rt <- true) "don't link with the runtime";
  flag "-c" (fun _ -> opts.compile_only <- true) "stop at compilation-phase (no linking)";
  flag "-S" (fun _ -> opts.keep_asm_files <- true) "keep assembly files (DEBUG)";
  flag "-lil" (fun _ -> opts.log_il <- true) "log IL codegen phase (DEBUG)";
  flag "-lra" (fun _ -> opts.log_ra <- true) "log register allocation phase (DEBUG)";

  flag_str "-o" opts.out_file "set output file";
];;

let usage = "usage: syml [OPTIONS] [INPUTS]";;
let anon file = Comp_state.infile opts file;;

let () =
  Arg.parse specs anon usage;
  Printexc.record_backtrace true;
  let res = Driver.dofiles opts opts.in_files in
  exit (if res then 0 else 1)
;;
