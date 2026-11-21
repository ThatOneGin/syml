(*
 * Syml:
 *  struct that carries the compiler state
 *)

type t = {
    mutable no_rt: bool;
    mutable keep_asm_files: bool;
    mutable compile_only: bool;
    mutable log_il: bool;
    mutable log_ra: bool;
    mutable in_files: string list;
    out_file: string ref;
    target: Common.target_arch;
  }

let ifdo flag (fn: unit -> unit) =
  if flag then fn ()
  else ()
;;

let infile (opts: t) (file: string) = 
  opts.in_files <- file::opts.in_files
;;