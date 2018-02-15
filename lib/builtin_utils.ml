open Sbash_type

type builtin_func_type =
  [`Infinite of Ir.tp list list * Ir.tp | `Normal of Ir.tp list list * Ir.tp]

type builtin_func_def =
  (* name *     definition                     *    type   *)
  string * (string list -> string -> string) * builtin_func_type

type builtin_func_def_set = builtin_func_def list

let add_builtin_func (fun_list: builtin_func_def_set) (add: builtin_func_def)
    : builtin_func_def_set =
  add :: fun_list
