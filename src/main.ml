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

let (comp_opts:Driver.comp_opt) = {
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
  flag "-lil" (fun _ -> comp_opts.log_il <- true) "log IR (TODO)";
  flag "-lra" (fun _ -> comp_opts.log_ra <- true) "log register allocation phase (TODO)";
];;

let usage = "usage: syml [OPTIONS] [INPUTS]";;
let anon file = comp_opts.in_files <- file::comp_opts.in_files;;

let () =
  Arg.parse specs anon usage;
  let res = Driver.dofiles comp_opts comp_opts.in_files in
  exit (if res then 0 else 1)
;;
