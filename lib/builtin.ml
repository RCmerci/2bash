open Core
open Ir

exception Built_in_fun_arg_err of string

let printf arg_l = String.concat ~sep:" " ("printf" :: arg_l)

let println arg_l = String.concat ~sep:" " ("echo -e" :: arg_l)

let builtin_fun_l =
  [ ("printf", printf, `Indefinite ([Str_type], Str_type))
  ; ("println", println, `Normal ([Str_type], Str_type)) ]


let gen name args =
  let open Option in
  List.find builtin_fun_l ~f:(fun (name', _, _) -> name = name')
  >>= fun (_, f, _) -> return @@ f args


let get_fun_tp name =
  let open Option in
  List.find builtin_fun_l ~f:(fun (name', _, _) -> name = name')
  >>= fun (_, _, tp) -> return tp


let inject_builtin_tp scope =
  let module S = Scope.StrMapScope in
  List.fold builtin_fun_l ~init:() ~f:(fun () (name, _, tp) ->
      S.add scope name tp )
