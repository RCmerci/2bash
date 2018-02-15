(* this file includes builtin string operation functions *)
open Core
open Utils

let s_is_empty =
  one_arg_func (fun arg result_var ->
      let cmd =
        Printf.sprintf
          "[[ -z $(echo -e %s | tr -d '[:space:]') ]] && echo 1 || echo 0" arg
      in
      result_var ^ "=$(" ^ cmd ^ ")" )


let s_trim_left =
  one_arg_func (fun arg result_var ->
      let cmd = Printf.sprintf "echo -e %s | sed -e 's/^[:space:]*//'" arg in
      result_var ^ "=$(" ^ cmd ^ ")" )

let s_trim_right =
  one_arg_func (fun arg result_var ->
      let cmd = Printf.sprintf "echo -e %s"
    )
