open Core
open Sbash_lib

let print_pos (lexbuf: Lexing.lexbuf) =
  let p = lexbuf.lex_start_p in
  let () = Out_channel.printf "fname: %s\n" p.pos_fname in
  let () = Out_channel.printf "lnum: %d\n" p.pos_lnum in
  let () = Out_channel.printf "bol: %d\n" p.pos_bol in
  let () = Out_channel.printf "cnum: %d\n" p.pos_cnum in
  let p1 = lexbuf.lex_curr_p in
  let () = Out_channel.printf "fname: %s\n" p1.pos_fname in
  let () = Out_channel.printf "lnum: %d\n" p1.pos_lnum in
  let () = Out_channel.printf "bol: %d\n" p1.pos_bol in
  let () = Out_channel.printf "cnum: %d\n" p1.pos_cnum in
  ()


let () = Out_channel.print_endline "=================="

let () =
  let src = In_channel.create "test.2bash" in
  let lexbuf = Lexing.from_channel src in
  let stat =
    try Parser.prog Lexer.read lexbuf with Parser.Error as e ->
      print_pos lexbuf ; raise e
  in
  Out_channel.print_endline (Syntax.show_statements stat)
