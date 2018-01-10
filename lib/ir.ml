type num_binary =
  | Num of int
  | Num_binary of (num_binary_op * num_binary * num_binary)
  | Num_left_value of leftvalue
  [@@deriving show]

and num_binary_op = Plus | Minus | Mul | Div [@@deriving show]

and leftvalue =
  | Identifier of string
  | ListAccess of (leftvalue * num_binary)
  [@@deriving show]

and str_binary =
  | Str of string
  | Str_binary of (str_binary_op * str_binary * str_binary)
  | Str_left_value of leftvalue
  [@@deriving show]

and str_binary_op = Str_plus [@@deriving show]

and bool_binary =
  | Bool of bool
  | Num_bool_binary of (bool_binary_op * num_binary * num_binary)
  | Str_bool_binary of (bool_binary_op * str_binary * str_binary)
  | Bool_bool_binary of (bool_binary_op * bool_binary * bool_binary)
  | Bool_left_value of leftvalue
  [@@deriving show]

and bool_binary_op = Gt | Lt | Ge | Le | Neq | Eq [@@deriving show]

and list_binary =
  | List of value list
  | List_binary of (list_binary_op * list_binary * list_binary)
  | List_left_value of leftvalue
  [@@deriving show]

and list_binary_op = List_plus [@@deriving show]

and value =
  | Left_value of leftvalue
  | Num_value of num_binary
  | Str_value of str_binary
  | Bool_value of bool_binary
  | List_value of list_binary
  | Fun_call of (string * value list)
  [@@deriving show]

and statement =
  | Assignment of (leftvalue * value)
  | If of (value * statements)
  | If_else of (value * statements * statements)
  | For of (string * value * statements)
  | While of (value * statements)
  | Fun_def of (string * string list * statements)
  | Return of value
  | Value of value
  [@@deriving show]

and statements = statement list [@@deriving show]
