(*
 * Syml:
 *  Syml's abstract syntax tree
 *)

type operator =
  | OADD | OSUB | ODIV | OMUL | OEQU | ONEQ | OINVALID
type expr =
  | Number of int
  | String of string
  | Binop of expr * operator * expr
  | Ident of string
type stat =
  | Var of vard
  | Return of expr
  | Asm of string
  | Voidcall of vcall
  | Block of block
  | Ifstat of ifstat
and toplevel = (* unused *)
  | Func of funct
  | Globvar of vard
and block = {
    mutable body: stat array;
  }
and funct = {
  name: string;
  blk: block;
  ty: Dtypes.datatype;
}
and vard = {
    name: string;
    ty: Dtypes.datatype;
    value: expr;
  }
and vcall = {
  name: string; (* for now it's only a string *)
  args: expr array;
}
and ifstat = {
  cond: expr;
  blk: block;
}

let block_append (b: block) (s: stat): unit =
  b.body <- Array.append b.body [|s|]; ()
