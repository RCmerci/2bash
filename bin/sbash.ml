open Core
open Sbash_lib

let compile =
  let spec =
    let open Command.Spec in
    empty +> anon ("file" %: file)
    +> flag "-o" (optional_with_default "2bash.sh" file) ~doc:"output filename"
  in
  Command.basic ~summary:"compile 2bash file" spec (fun src dst () ->
      let src' = In_channel.create src in
      let lexbuf = Lexing.from_channel src' in
      let stats =
        try Parser.prog Lexer.read lexbuf with Parser.Error as e ->
          Out_channel.print_endline "parser error" ;
          Position.show_position lexbuf.lex_start_p
          |> Out_channel.print_endline ;
          raise e
      in
      let compile_ctx = Compile.make_ctx () in
      let typecheck_ctx = Type_check.make_ctx () in
      let stats' =
        try
          Compile.compile_statements compile_ctx stats
          |> Type_check.check_statements typecheck_ctx
        with Type_check.Type_err (desc, pos) as e ->
          Out_channel.print_endline desc ;
          let open Option in
          let _ =
            pos
            >>= fun pos' ->
            Out_channel.print_endline (Position.show_position_region pos') ;
            None
          in
          raise e
      in
      let result = Generate.gen_statements stats' 0 in
      let dst' = Out_channel.create dst in
      Out_channel.output_lines dst' result )


let group = Command.group ~summary:"" [("compile", compile)]

let () = Command.run group
