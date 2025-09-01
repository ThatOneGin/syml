(*
 * Syml:
 *  Syml's abstract syntax tree
 *)

type operator =
  | OADD | OSUB | ODIV | OMUL | OINVALID
type expr =
  | Number of int
  | String of string
  | Binop of expr * operator * expr
  | Ident of string
type stat =
  | Var of vard
  | Return of expr
and toplevel = (* unused *)
  | Func of funct
  | Globvar of vard
and funct = {
  name: string;
  mutable body: stat array;
  ty: Dtypes.datatype;
}
and vard = {
    name: string;
    ty: Dtypes.datatype;
    value: expr;
  }

let func_append (f: funct) (s: stat): unit =
  f.body <- Array.append f.body [|s|]; ()
