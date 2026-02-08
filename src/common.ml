(*
 * Syml:
 *  Common functions/types
 *)

open Printf

exception Common_error of string

type target_arch =
  | Linux_X86_64

(* unused *)
type location = {
    name: string;
    line: int;
  }

let location_new (name: string) (line: int): location = {
    name = name;
    line = line;
  }
and location2str (loc: location): string =
  sprintf "%s:%d" loc.name loc.line

let syml_errorf fmt: 'a =
  ksprintf
    (fun s ->
      printf "[Error] ";
      print_endline s;
      print_newline ();
      raise (Common_error "Aborting due to previous error."))
    fmt

let unreachable (where: string) (what: string) =
  syml_errorf "Unreachable state reached: %s at %s." what where

let todo (what: string) =
  syml_errorf "TODO: %s is not implemented." what

let drop _: unit = ()

(* just an array of integers to use in ra.ml *)
type ints = {
    data: int array;
    size: int;
  }

let ints_new (s: int) (v: int): ints =
  {data = Array.init s (fun _ -> v);
   size = s;}

let ints_set (is: ints) (i: int) (v: int): unit =
  assert (is.size > i);
  assert (i >= 0);
  is.data.(i) <- v

let ints_get (is: ints) (i: int): int =
  assert (is.size > i);
  assert (i >= 0);
  is.data.(i)