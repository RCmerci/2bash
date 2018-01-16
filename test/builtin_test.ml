open Core
open OUnit2
open Sbash_lib

(*
a=call("echo aaaa");
println(a);
 *)
let fixture_call = Util.parse "a=call(\"echo aaaa\");\nprintln(a);"

(*
r=sprintf("%s, %d, %d", "str", 123, 1);
println(r);
 *)

let fixture_sprintf =
  Util.parse "r=sprintf(\"%s, %d, %d\", \"str\", 123, 1);\nprintln(r);"


let test_call _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats =
    Compile.compile_statements ctx fixture_call
    |> Type_check.check_statements ctx'
  in
  let result =
    Generate.gen_statements stats 0 |> String.concat
    |> Util.run_shell ~tmp_file_name:"test-builtin-call" |> String.concat
  in
  assert_equal "aaaa" result


let test_sprintf _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats =
    Compile.compile_statements ctx fixture_sprintf
    |> Type_check.check_statements ctx'
  in
  let result =
    Generate.gen_statements stats 0 |> String.concat
    |> Util.run_shell ~tmp_file_name:"test-builtin-sprintf" |> String.concat
  in
  assert_equal "str, 123, 1" result


let suite = ["test_call" >:: test_call; "test_sprintf" >:: test_sprintf]
