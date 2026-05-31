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
    log_il = false;
    log_ra = false;
    in_files = [];
    target = target;
  }
;;

(* custom set flag *)
let flag fl fn dsc =
  (fl, Arg.Unit fn, dsc)
;;

let specs = [
  flag "-lil" (fun _ -> opts.log_il <- true) "log IL codegen phase (DEBUG)";
  flag "-lra" (fun _ -> opts.log_ra <- true) "log register allocation phase (DEBUG)";
];;

let usage = "usage: syml [OPTIONS] [INPUTS]";;
let anon file = Comp_state.infile opts file;;

let () =
  Arg.parse specs anon usage;
  Printexc.record_backtrace true;
  let res = Driver.dofiles opts opts.in_files in
  exit (if res then 0 else 1)
;;
