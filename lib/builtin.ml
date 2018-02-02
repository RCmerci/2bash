open Core
open Ir

exception Built_in_fun_arg_err of string

let printf arg_l result_var =
  let format, arg_l' =
    match arg_l with a :: b -> (a, b) | _ -> assert false
  in
  let arg_l'' = List.map arg_l' ~f:(fun e -> "\"" ^ e ^ "\"") in
  let l1 = String.concat ~sep:" " (["printf"; format] @ arg_l'') in
  let l2 = result_var ^ "=$?" in
  [l1; l2] |> String.concat ~sep:"\n"


let println arg_l result_var =
  let l1 = String.concat ~sep:" " ("echo -e" :: arg_l) in
  let l2 = result_var ^ "=$?" in
  [l1; l2] |> String.concat ~sep:"\n"


let sprintf arg_l result_var =
  let format, arg_l' =
    match arg_l with a :: b -> (a, b) | _ -> assert false
  in
  let arg_l'' = List.map arg_l' ~f:(fun e -> "\"" ^ e ^ "\"") in
  let p = String.concat ~sep:" " (["printf"; format] @ arg_l'') in
  result_var ^ "=$(" ^ p ^ ")"


let call arg_l result_var =
  let arg_l' = String.substr_replace_all (List.hd_exn arg_l) "\\\"" "\"" in
  let arg' = String.substr_replace_first arg_l' "\"" "" in
  let arg'' =
    String.substr_replace_first
      ~pos:(String.length arg' - 1)
      arg' ~pattern:"\"" ~with_:""
  in
  result_var ^ "=$(" ^ arg'' ^ ")"


let exists arg_l result_var =
  let l1 = Printf.sprintf "[ -f %s ]" (List.hd_exn arg_l) in
  let l2 = result_var ^ "=$?" in
  [l1; l2] |> String.concat ~sep:"\n"


let list arg_l result_var = result_var ^ "=($(echo " ^ List.hd_exn arg_l ^ "))"

let num arg_l result_var =
  let arg = List.hd_exn arg_l in
  let err_info =
    Printf.sprintf "func num failed: %s can not convert to num type" arg
  in
  Printf.sprintf
    "[ %s -eq %s ] 2>/dev/null && { %s=%s; } || { echo \"%s\";exit 1; }" arg
    arg result_var arg err_info


let builtin_fun_l =
  [ ("printf", printf, `Indefinite ([[Str_type]], Num_type))
  ; ("println", println, `Normal ([[Str_type; Num_type; Bool_type]], Num_type))
  ; ("sprintf", sprintf, `Indefinite ([[Str_type]], Str_type))
  ; ("call", call, `Normal ([[Str_type]], Str_type))
  ; ("exists", exists, `Normal ([[Str_type]], Bool_type))
  ; ( "list"
    , list
    , `Normal ([[Str_type; Num_type; Bool_type]], List_type Str_type) )
  ; ("num", num, `Normal ([[Str_type; Bool_type; Num_type]], Num_type)) ]


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
