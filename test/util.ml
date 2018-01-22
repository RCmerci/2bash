open Core
open Sbash_lib

let print_pos (lexbuf: Lexing.lexbuf) =
  let p = lexbuf.lex_curr_p in
  let () = Out_channel.printf "fname: %s\n" p.pos_fname in
  let () = Out_channel.printf "lnum: %d\n" p.pos_lnum in
  let () = Out_channel.printf "bol: %d\n" p.pos_bol in
  let () = Out_channel.printf "cnum: %d\n" p.pos_cnum in
  ()


let parse src =
  let lexbuf = Lexing.from_string src in
  let stats =
    try Parser.prog Lexer.read lexbuf with Parser.Error as e ->
      print_pos lexbuf ; raise e
  in
  stats


let tmp_file name content =
  let uuid = Uuid.create () |> Uuid.to_string in
  let file_name = name ^ "-" ^ uuid in
  let file = Out_channel.create file_name in
  Out_channel.output_string file content ;
  Out_channel.close file ;
  file_name


let run_shell ?(tmp_file_name= "tmp") ?(rm_tmp_file= false) s =
  let tmpf = tmp_file tmp_file_name s in
  let cmd = Unix.open_process_in ("bash ./" ^ tmpf) in
  let r = In_channel.input_lines cmd in
  In_channel.close cmd ;
  if rm_tmp_file then Sys.remove tmpf ;
  r


let assert_string_contains s sub =
  Option.is_some (String.substr_index s sub) |> OUnit2.assert_bool s


let assert_position (p: Position.position option) cnum =
  if Option.is_none p then OUnit2.assert_bool "none position" false
  else
    let p' = Option.value_exn p in
    let desc = Position.show_position p' in
    OUnit2.assert_bool desc (phys_equal p'.pos_cnum cnum)


let assert_position_region (p: Position.position_region option) cnum =
  let p' = Option.(p >>= fun p' -> return p'.start_pos) in
  assert_position p' cnum
