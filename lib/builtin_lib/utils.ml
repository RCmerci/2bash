open Core

let func_arg_1 def arg_l result_var =
  (* aux func for define only one argument builtin-func *)
  let arg = List.nth_exn arg_l 0 in
  def arg result_var


let func_arg_2 def arg_l result_var =
  let arg1 = List.nth_exn arg_l 0 in
  let arg2 = List.nth_exn arg_l 1 in
  def arg1 arg2 result_var


let func_arg_3 def arg_l result_var =
  let arg1 = List.nth_exn arg_l 0 in
  let arg2 = List.nth_exn arg_l 1 in
  let arg3 = List.nth_exn arg_l 2 in
  def arg1 arg2 arg3 result_var


let func_arg_4 def arg_l result_var =
  let arg1 = List.nth_exn arg_l 0 in
  let arg2 = List.nth_exn arg_l 1 in
  let arg3 = List.nth_exn arg_l 2 in
  let arg4 = List.nth_exn arg_l 3 in
  def arg1 arg2 arg3 arg4 result_var
