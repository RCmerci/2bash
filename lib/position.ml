open Core
open Sbash_type.Position

let show_position p =
  Printf.sprintf "line: %d, bol:%d, cnum:%d" p.pos_lnum p.pos_bol p.pos_cnum


let show_position_region r =
  Printf.sprintf "start: [%s], end:[%s]"
    (show_position r.start_pos)
    (show_position r.end_pos)


let next_line (lexbuf: Lexing.lexbuf) =
  let curr_pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p
  <- {curr_pos with pos_bol= 0; pos_lnum= curr_pos.pos_lnum + 1}


(* update position when swallow each token *)
let next_token (lexbuf: Lexing.lexbuf) =
  let start_pos = lexbuf.lex_start_p in
  let curr_pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p
  <- { curr_pos with
       pos_bol= curr_pos.pos_cnum - start_pos.pos_cnum + curr_pos.pos_bol }
