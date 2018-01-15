type symbol = string [@@deriving show]

type num = int [@@deriving show]

type string' = string [@@deriving show]

type bool' = bool [@@deriving show]

type op =
  | String_plus
  | List_plus
  | Plus
  | Minus
  | Mul
  | Div
  | Gt
  | Lt
  | Ge
  | Le
  | Neq
  | Eq
  [@@deriving show]

type list' = value list [@@deriving show]

and basic_value =
  | Num of num
  | String of string'
  | Bool of bool'
  | List of list'
  | Symbol of symbol
  [@@deriving show]

and op_value = Op of (value * op * value) [@@deriving show]

and value =
  | Fun_call of (symbol * value list)
  | Op_value of op_value
  | Basic_value of basic_value
  [@@deriving show]

type statement =
  | Assignment of (symbol * value)
  | If of (value * statements * statements option)
  | For of (symbol * value * statements)
  | While of (value * statements)
  | Function of (symbol * symbol list * statements * string list)
  | Return of value
  | Value of value
  [@@deriving show]

and statements = statement list [@@deriving show]

type token =
  | INT of int
  | SYMBOL of symbol
  | STRING of string
  | TRUE
  | FALSE
  | LBRACKET
  | RBRACKET
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | PLUS
  | STRING_PLUS
  | LIST_PLUS
  | MINUS
  | MUL
  | DIV
  | SEMICOLON
  | GE
  | LE
  | NEQ
  | GT
  | LT
  | EQ
  | ASSIGN
  | FUNCTION
  | IF
  | ELSE
  | WHILE
  | FOR
  | IN
  | RETURN
  | COMMA
  | ARROW
  | COLON
  | EOF
  [@@deriving show]
