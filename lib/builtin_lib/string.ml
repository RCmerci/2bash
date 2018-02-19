(* this file includes builtin string operation functions *)
open Core
open Utils

let s_is_empty =
  func_arg_1 (fun arg result_var ->
      let cmd =
        Printf.sprintf
          "[[ -z $(echo -e %s | tr -d '[:space:]') ]] && echo 1 || echo 0" arg
      in
      result_var ^ "=$(" ^ cmd ^ ")" )


let s_trim_left =
  func_arg_1 (fun arg result_var ->
      let cmd = Printf.sprintf "echo -e %s | sed -e 's/^[[:space:]]*//'" arg in
      result_var ^ "=$(" ^ cmd ^ ")" )


let s_trim_right =
  func_arg_1 (fun arg result_var ->
      let cmd = Printf.sprintf "echo -e %s | sed -e 's/[[:space:]]*$//'" arg in
      result_var ^ "=$(" ^ cmd ^ ")" )


let s_trim =
  func_arg_1 (fun arg result_var ->
      let cmd =
        Printf.sprintf
          "echo -e %s | sed -e 's/^[[:space:]]*//' | sed -e 's/[[:space:]]*$//'"
          arg
      in
      result_var ^ "=$(" ^ cmd ^ ")" )


let s_collapse_whitespace =
  func_arg_1 (fun arg result_var ->
      let cmd =
        Printf.sprintf "echo -e %s | sed -e 's/[[:space:]]\\{1,\\}/ /'" arg
      in
      result_var ^ "=$(" ^ cmd ^ ")" )


(* let s_split arg_l result_var = *)
(*   func_arg_2 (fun sep s -> *)
(*       let old_IFS = "_old_IFS=" *)
(*     ) *)
