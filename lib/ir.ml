type meta = {pos: Position.position_region option} [@@deriving show]

type tp =
  | Num_type
  | Str_type
  | List_type of tp
  | Bool_type
  | Unknown_type
  [@@deriving show]

(* Indefinite:
   e.g. printf
 *)
type fun_tp =
  [`Normal of tp list list * tp | `Indefinite of tp list list * tp]
  [@@deriving show]

type num_binary =
  | Num of int
  | Num_binary of (num_binary_op * num_binary * num_binary)
  | Num_leftvalue_binary of (num_binary_op * leftvalue * leftvalue)
  [@@deriving show]

and num_binary_op = Plus | Minus | Mul | Div [@@deriving show]

and leftvalue =
  (* (name, type, local) *)
  | Identifier of {v: (string * tp * bool); meta: meta}
  | ListAccess of {v: ((leftvalue * num_binary) * tp); meta: meta}
  [@@deriving show]

and str_binary =
  | Str of string
  | Str_binary of (str_binary_op * str_binary * str_binary)
  | Str_leftvalue_binary of (str_binary_op * leftvalue * leftvalue)
  [@@deriving show]

and str_binary_op = Str_plus [@@deriving show]

and bool_binary =
  | Bool of bool
  | Num_bool_binary of (bool_binary_op * num_binary * num_binary)
  | Str_bool_binary of (bool_binary_op * str_binary * str_binary)
  | Bool_bool_binary of (bool_binary_op * bool_binary * bool_binary)
  | Bool_leftvalue_binary of (bool_binary_op * leftvalue * leftvalue)
  [@@deriving show]

and bool_binary_op = Gt | Lt | Ge | Le | Neq | Eq [@@deriving show]

and list_binary =
  | List_binary of (list_binary_op * list_binary * list_binary)
  | List_leftvalue_binary of (list_binary_op * leftvalue * leftvalue)
  [@@deriving show]

and list_binary_op = List_plus [@@deriving show]

type value =
  | Left_value of {v: leftvalue; meta: meta}
  | Num_value of {v: num_binary; meta: meta}
  | Str_value of {v: str_binary; meta: meta}
  | Bool_value of {v: bool_binary; meta: meta}
  | List_binary_value of {v: list_binary; meta: meta}
  | List of {v: value list; meta: meta}
  | Fun_call of {v: (string * value list); meta: meta}
  [@@deriving show]

and statement =
  | Assignment of (leftvalue * value)
  | If of (value * statements)
  | If_else of (value * statements * statements)
  | For of (leftvalue * value * statements)
  | While of (value * statements)
  | Fun_def of (string * string list * statements * fun_tp)
  | Return of value
  | Value of value
  [@@deriving show]

and statements = statement list [@@deriving show]
