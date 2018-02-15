open Core

let one_arg_func def arg_l result_var =
  (* aux func for define only one argument builtin-func *)
  let arg = List.nth_exn arg_l 0 in
  def arg result_var
