open Sbash_type

type builtin_func_type =
  [`Infinite of Ir.tp list list * Ir.tp | `Normal of Ir.tp list list * Ir.tp]

type builtin_func_def =
  (* name *     definition                     *    type   *)
  string * (string list -> string -> string) * builtin_func_type

type builtin_func_def_set

val add_builtin_func :
  builtin_func_def_set -> builtin_func_def -> builtin_func_def_set
