(*
 * Syml:
 *  General data types
 *)

open Common

type datatype =
  | Int
  | Str
  | Nil
  | I8
  | I16
  | I32
  | I64
  | Ptr of datatype

let typecmp
  (t1: datatype)
  (t2: datatype): bool = t1 == t2

let rec type2str (t: datatype): string =
  match t with
  | Int -> "int"
  | Str -> "string"
  | Nil -> "nil"
  | I8 -> "i8"
  | I16 -> "i16"
  | I32 -> "i32"
  | I64 -> "i64"
  | Ptr t -> "ptr<" ^ (type2str t) ^ ">"

let type_error (what: string): 'a =
  syml_errorf "type error: %s" what
