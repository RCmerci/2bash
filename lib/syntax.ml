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
  | Fun_call of {v: (symbol * value list); pos: Position.position_region}
  | Op_value of {v: op_value; pos: Position.position_region}
  | Basic_value of {v: basic_value; pos: Position.position_region}
  [@@deriving show]

type statement =
  | Assignment of {v: (symbol * value); symbol_pos: Position.position_region}
  | If of (value * statements * statements option)
  | For of
      { v: (symbol * value * statements)
      ; symbol_pos: Position.position_region }
  | While of (value * statements)
  | Function of (symbol * symbol list * statements * string list)
  | Return of value
  | Value of value
  | Break of {pos: Position.position_region}
  | Continue of {pos: Position.position_region}
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
  | BREAK
  | CONTINUE
  | FOR
  | IN
  | RETURN
  | COMMA
  | ARROW
  | COLON
  | EOF
  [@@deriving show]
