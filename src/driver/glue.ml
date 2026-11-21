(*
 * Syml:
 *  Run assembler and linker for our inputs
 *)
open Common

(* we should not rely on the Unix module *)
let run_process (prog: string) (_argv: string list): unit =
  match Sys.os_type with
  | "Unix" ->
    let argv = [prog] @ _argv in
    String.concat " " argv |> print_endline;
    let pid = 
      Unix.create_process
        prog
        (Array.init (List.length argv) (fun i -> List.nth argv i))
        Unix.stdin
        Unix.stdout
        Unix.stderr
    in
    begin match Unix.waitpid [] pid with
    | _, Unix.WEXITED 0 -> ()
    | _, Unix.WEXITED code -> syml_errorf "%s exited with code %d" prog code
    | _, Unix.WSIGNALED signal -> syml_errorf "%s killed by signal %d" prog signal
    | _, Unix.WSTOPPED signal -> syml_errorf "%s stopped by signal %d" prog signal
    end;
    ()
  | _ ->
    syml_errorf "unsupported os type %s" Sys.os_type;
;;

(*
 * our toolchain
 *
 * defines what we'll use to compile and link our
 * files. Currently, we use GCC for simplicity reasons
 * , but we should replace it with an assembler
 * and linker individually in the future
 * (not a distant future, actually)
 *)
type tc = {
    tc_cc: string; (* C compiler *)
    tc_cflags: string list; (* C compiler flags *)
    tc_rm: string; (* remove files *)
  }

let unix_find_runtime (): string =
  match Sys.getenv_opt "SYML_RUNTIME_PATH" with
  | Some s -> s
  | None ->
    let sys_path = "/usr/lib/libsyml.so" in
    if Sys.file_exists sys_path then "/usr/lib/"
    else syml_errorf "could not locate runtime path"
;;

let create_toolchain (opts:Comp_state.t): tc = 
  match opts.target with
  | Linux_X86_64 ->
    let runtime_path = unix_find_runtime () in
    {
      tc_cc = "cc";
      tc_cflags = ["-L" ^ runtime_path; "-lsyml"];
      tc_rm = "rm";
    }
;;

let run (opts:Comp_state.t) (inputs: string list) (output: string): unit =
  let tc = create_toolchain opts in
  run_process tc.tc_cc (tc.tc_cflags @ inputs @ ["-o"; output]);
  run_process tc.tc_rm inputs
;;
