open Sbash_type

val gen : name:string -> args:string list -> (string -> string) option

val get_fun_tp :
  string
  -> [> `Infinite of Ir.tp list list * Ir.tp
     | `Normal of Ir.tp list list * Ir.tp ]
     option

val inject_builtin_tp :
  Builtin_utils.builtin_func_type Scope.StrMapScope.t -> unit
