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


(*
println(exists("not-exist-file"));
 *)

let fixture_exists = Util.parse "println(exists(\"not-exist-file\"));"

(*
l=list("1 2 3");
for (i in l) {
   printf("%d ", i);
}
 *)

let fixture_list =
  Util.parse "l=list(\"1 2 3\");\nfor (i in l) {\n   printf(\"%d \", i);\n}"


(*
a=list("123");
b=a@@["1","2","3"];
for (i in b) {
   printf("%d ", i);
}
 *)
let fixture_list_2 =
  Util.parse
    "a=list(\"123\");\nb=a@@[\"1\",\"2\",\"3\"];\nfor (i in b) {\n   printf(\"%d \", i);\n}"


(*
a="233";
b=num(a);
b=b+1;
println(b);
 *)

let fixture_num = Util.parse "a=\"233\";\nb=num(a);\nb=b+1;\nprintln(b);"

let test_call _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_ctx () in
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
  let ctx' = Type_check.make_ctx () in
  let stats =
    Compile.compile_statements ctx fixture_sprintf
    |> Type_check.check_statements ctx'
  in
  let result =
    Generate.gen_statements stats 0 |> String.concat
    |> Util.run_shell ~tmp_file_name:"test-builtin-sprintf" |> String.concat
  in
  assert_equal "str, 123, 1" result


let test_exists _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_ctx () in
  let stats =
    Compile.compile_statements ctx fixture_exists
    |> Type_check.check_statements ctx'
  in
  let result =
    Generate.gen_statements stats 0 |> String.concat
    |> Util.run_shell ~tmp_file_name:"test-builtin-exists" |> String.concat
  in
  assert_equal "1" result


let test_list _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_ctx () in
  let stats =
    Compile.compile_statements ctx fixture_list
    |> Type_check.check_statements ctx'
  in
  let result =
    Generate.gen_statements stats 0 |> String.concat
    |> Util.run_shell ~tmp_file_name:"test-builtin-list" |> String.concat
  in
  assert_equal "1 2 3 " result


let test_list_2 _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_ctx () in
  let stats =
    Compile.compile_statements ctx fixture_list_2
    |> Type_check.check_statements ctx'
  in
  let result =
    Generate.gen_statements stats 0 |> String.concat
    |> Util.run_shell ~tmp_file_name:"test-builtin-list-2" |> String.concat
  in
  assert_equal "123 1 2 3 " result


let test_num _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_ctx () in
  let stats =
    Compile.compile_statements ctx fixture_num
    |> Type_check.check_statements ctx'
  in
  let result =
    Generate.gen_statements stats 0 |> String.concat
    |> Util.run_shell ~tmp_file_name:"test-builtin-num" |> String.concat
  in
  assert_equal "234" result


let suite =
  [ "test_call" >:: test_call
  ; "test_sprintf" >:: test_sprintf
  ; "test_exists" >:: test_exists
  ; "test_list" >:: test_list
  ; "test_list_2" >:: test_list_2
  ; "test_num" >:: test_num ]
