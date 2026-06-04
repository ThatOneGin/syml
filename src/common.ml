(*
 * Syml:
 *  Common functions/types
 *)

open Printf

exception Common_error of string
exception Unreachable_error of string * string
exception Todo_error of string

type target_arch =
  | Linux_X86_64

(* unused *)
type location = {
    name: string;
    line: int;
    col: int;
  }

let location_new (name: string) (line: int) (col: int): location = {
    name = name;
    line = line;
    col = col
  }
and location2str (loc: location): string =
  sprintf "%s:%d:%d" loc.name loc.line loc.col
;;

let syml_errorf fmt: 'a =
  ksprintf (fun s -> raise (Common_error s)) fmt;;

let unreachable (where: string) (what: string) =
  raise (Unreachable_error (what, where));;

let todo (what: string) =
  raise (Todo_error what);;

let drop _: unit = ();;

(* just an array of integers to use in ra.ml *)
type ints = {
    data: int array;
    size: int;
  }

let ints_new (s: int) (v: int): ints =
  {data = Array.init s (fun _ -> v);
   size = s;}
;;

let ints_set (is: ints) (i: int) (v: int): unit =
  assert (is.size > i);
  assert (i >= 0);
  is.data.(i) <- v
;;

let ints_get (is: ints) (i: int): int =
  assert (is.size > i);
  assert (i >= 0);
  is.data.(i)
;;