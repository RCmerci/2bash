open Core
open Ir

exception Built_in_fun_arg_err of string

let printf arg_l result_var =
  let l1 = String.concat ~sep:" " ("printf" :: arg_l) in
  let l2 = result_var ^ "=$?" in
  [l1; l2] |> String.concat ~sep:"\n"


let println arg_l result_var =
  let l1 = String.concat ~sep:" " ("echo -e" :: arg_l) in
  let l2 = result_var ^ "=$?" in
  [l1; l2] |> String.concat ~sep:"\n"


let sprintf arg_l result_var =
  let p = String.concat ~sep:" " ("printf" :: arg_l) in
  result_var ^ "=$(" ^ p ^ ")"


let call arg_l result_var =
  let arg =
    List.hd_exn arg_l
    |> String.strip ~drop:(fun e ->
           match e with '"' | ' ' | '\t' -> true | _ -> false )
  in
  result_var ^ "=$(" ^ arg ^ ")"


let builtin_fun_l =
  [ ("printf", printf, `Indefinite ([[Str_type]], Num_type))
  ; ("println", println, `Normal ([[Str_type; Num_type; Bool_type]], Num_type))
  ; ("sprintf", sprintf, `Indefinite ([[Str_type]], Str_type))
  ; ("call", call, `Normal ([[Str_type]], Str_type)) ]


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
