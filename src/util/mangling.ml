(* Syml:
 *  name mangling functions
 *)
open Dtypes

let mangle_name s =
  Printf.sprintf "_S%d%s" (String.length s) s
;;

let rec name_of_type = function
  | Int -> "i"
  | Str -> "s"
  | Nil -> "n"
  | I8 -> "b"
  | I16 -> "d"
  | I32 -> "l"
  | I64 -> "q"
  | Ptr p -> "P" ^ (name_of_type p)
  | Fptr f -> "f" ^ String.concat "" (List.map name_of_type f.args)
  | Struct li ->
      "S" ^
        String.concat ""
          (List.map
            (fun (name, t) ->
              (mangle_name name) ^ (name_of_type t)) li)
  | Name s -> mangle_name s
  | _ -> assert false
;;

let mangle_name_with_type name pref ty =
  mangle_name name ^ pref ^ (String.concat "" @@ List.map name_of_type ty)
;;

let mangle_func name ty = mangle_name_with_type name "F" ty;;
let mangle_var name ty = mangle_name_with_type name "V" [ty];;
let mangle_template name ty = mangle_name_with_type name "T" ty;;