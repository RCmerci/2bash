type position = Lexing.position =
  {pos_fname: string; pos_lnum: int; pos_bol: int; pos_cnum: int}
  [@@deriving show]

type position_region =
  {start_pos: position; end_pos: position}
  [@@deriving show]
