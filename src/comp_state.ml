(*
 * Syml:
 *  struct that carries the compiler state
 *)

type t = {
    mutable log_il: bool;
    mutable log_ra: bool;
    mutable in_files: string list;
    target: Common.target_arch;
  }

let ifdo flag (fn: unit -> unit) =
  if flag then fn ()
  else ()
;;

let infile (opts: t) (file: string) = 
  opts.in_files <- file::opts.in_files
;;