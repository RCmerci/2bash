open Core
open OUnit2
open Sbash_lib
open Util

(*
a=1+2*3-4;
println(a);
 *)

let fixture_num_binary = Util.parse "a=1+2*3-4;\nprintln(a);"

(*
a="1"+2;
 *)

let fixture_num_binary_1 = Util.parse "a=\"1\"+2;"

(*
fun foo():string {
   return "233";
}
a=1*foo();
 *)
let fixture_num_binary_2 =
  Util.parse "fun foo():string {\n   return \"233\";\n}\na=1*foo();"


(*
a="dede"++"tttt";
b=a++"8888";
println(b);
 *)
let fixture_str_binary =
  Util.parse "a=\"dede\"++\"tttt\";\nb=a++\"8888\";\nprintln(b);"


(*
a=1++"de" ;
 *)

let fixture_str_binary_1 = Util.parse "a=1++\"de\" ;"

(*
a=true;
b=false;
c=a==b;
println(c);
 *)

let fixture_bool_binary = Util.parse "a=true;\nb=false;\nc=a==b;\nprintln(c);"

(*
a=1>2;
println(a);
 *)

let fixture_bool_binary_1 = Util.parse "a=1>2;\nprintln(a);"

(*
a="aaa";
c=a=="bb";
println(c);
 *)

let fixture_bool_binary_2 = Util.parse "a=\"aaa\";\nc=a==\"bb\";\nprintln(c);"

(*
a=1;
println(a);
 *)
let fixture_assign = Util.parse "a=1;\nprintln(a);"

(*
b=a;
 *)
let fixture_not_defined = Util.parse "b=a;"

(*
a=1;
b=2;
if (a<b) {
   c=2;
} else {
   c=3;
}
println(c);
 *)
let fixture_if =
  Util.parse
    "a=1;\nb=2;\nif (a<b) {\n   c=2;\n} else {\n   c=3;\n}\nprintln(c);"


(*
if (1==0) {
   a=1;
}
println(a);
 *)
let fixture_if_1 = Util.parse "if (1==0) {\n   a=1;\n}\nprintln(a);"

(*
a=0;
for (i in [1,2,3]) {
   a=a+i;
}
printf("%d", a);
 *)

let fixture_for =
  Util.parse "a=0;\nfor (i in [1,2,3]) {\n   a=a+i;\n}\nprintln(a);"


(*
a="";
for (i in "1 2 3") {
   a=a++i;
}
 *)
let fixture_for_1 = Util.parse "a=\"\";\nfor (i in \"1 2 3\") {\n   a=a++i;\n}"

(*
a=0;
while (a < 5) {
   a=a+1;
}
println(a);
 *)
let fixture_while =
  Util.parse "a=0;\nwhile (a < 5) {\n   a=a+1;\n}\nprintln(a);"


(*
fun foo(a, b , c): string-> num -> bool -> string
 {
   return sprintf("%s,%d,%d", a, b , c);

}
r=foo("a", 1, true);
println(r);
 *)
let fixture_fun_def =
  Util.parse
    "fun foo(a, b , c): string-> num -> bool -> string\n {\n   return sprintf(\"%s,%d,%d\", a, b , c);\n}\nr=foo(\"a\", 1, true);\nprintln(r);"


(*
fun foo(a,b): string -> num -> num {
   printf("%s, %d", a, b);
   return "";
}
 *)
let fixture_fun_def_1 =
  Util.parse
    "fun foo(a,b): string -> num -> num {\n   printf(\"%s, %d\", a, b);\n   return \"\";\n}"


(*
fun foo() : string {
    return "aaa";
}
println(foo());
 *)

let fixture_return =
  Util.parse "fun foo() : string {\n    return \"aaa\";\n}\nprintln(foo());"


(*
fun foo() : string {
   println("aaa");
}
foo();
 *)
let fixture_value =
  Util.parse "fun foo() : string {\n   println(\"aaa\");\n}\nfoo();"


(*
// this is comment
a=1; // another comment
println(a);
//
 *)

let fixture_comment =
  Util.parse "// this is comment\na=1; // another comment\nprintln(a);\n// "


let gen_stats_num_binary _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats =
    Compile.compile_statements ctx fixture_num_binary
    |> Type_check.check_statements ctx'
  in
  let result =
    Generate.gen_statements stats 0 |> String.concat
    |> Util.run_shell ~tmp_file_name:"test-num-binary" |> String.concat
  in
  assert_equal "3" result


let gen_stats_num_binary_1 _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats () =
    Compile.compile_statements ctx fixture_num_binary_1
    |> Type_check.check_statements ctx' |> ignore
  in
  try stats () with Type_check.Type_err s ->
    Util.assert_string_contains s "expect type Ir.Num_type, got Ir.Str_type"


let gen_stats_num_binary_2 _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats () =
    Compile.compile_statements ctx fixture_num_binary_2
    |> Type_check.check_statements ctx' |> ignore
  in
  try stats () with Type_check.Type_err s ->
    Util.assert_string_contains s "expect type Ir.Num_type, got Ir.Str_type"


let gen_stats_str_binary _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats =
    Compile.compile_statements ctx fixture_str_binary
    |> Type_check.check_statements ctx'
  in
  let result =
    Generate.gen_statements stats 0 |> String.concat
    |> Util.run_shell ~tmp_file_name:"test-str-binary" |> String.concat
  in
  assert_equal "dedetttt8888" result


let gen_stats_str_binary_1 _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats () =
    Compile.compile_statements ctx fixture_str_binary_1
    |> Type_check.check_statements ctx' |> ignore
  in
  try stats () with Type_check.Type_err s ->
    Util.assert_string_contains s "expect type Ir.Str_type, got Ir.Num_type"


let gen_stats_bool_binary _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats =
    Compile.compile_statements ctx fixture_bool_binary
    |> Type_check.check_statements ctx'
  in
  let result =
    Generate.gen_statements stats 0 |> String.concat
    |> Util.run_shell ~tmp_file_name:"test-bool-binary" |> String.concat
  in
  assert_equal "1" result


let gen_stats_bool_binary_1 _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats =
    Compile.compile_statements ctx fixture_bool_binary_1
    |> Type_check.check_statements ctx'
  in
  let result =
    Generate.gen_statements stats 0 |> String.concat
    |> Util.run_shell ~tmp_file_name:"test-bool-binary-1" |> String.concat
  in
  assert_equal "1" result


let gen_stats_bool_binary_2 _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats =
    Compile.compile_statements ctx fixture_bool_binary_2
    |> Type_check.check_statements ctx'
  in
  let result =
    Generate.gen_statements stats 0 |> String.concat
    |> Util.run_shell ~tmp_file_name:"test-bool-binary-2" |> String.concat
  in
  assert_equal "1" result


let gen_stats_assign _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats =
    Compile.compile_statements ctx fixture_assign
    |> Type_check.check_statements ctx'
  in
  let result =
    Generate.gen_statements stats 0 |> String.concat
    |> Util.run_shell ~tmp_file_name:"test-assign" |> String.concat
  in
  assert_equal "1" result


let gen_stats_not_defined _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats () =
    Compile.compile_statements ctx fixture_not_defined
    |> Type_check.check_statements ctx' |> ignore
  in
  try stats () with Type_check.Type_err s ->
    Util.assert_string_contains s "not defined yet"


let gen_stats_if _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats =
    Compile.compile_statements ctx fixture_if
    |> Type_check.check_statements ctx'
  in
  let result =
    Generate.gen_statements stats 0 |> String.concat
    |> Util.run_shell ~tmp_file_name:"test-if" |> String.concat
  in
  assert_equal "2" result


let gen_stats_if_1 _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats =
    Compile.compile_statements ctx fixture_if_1
    |> Type_check.check_statements ctx'
  in
  let result =
    Generate.gen_statements stats 0 |> String.concat
    |> Util.run_shell ~tmp_file_name:"test-if-1" |> String.concat
  in
  assert_equal "" result


let gen_stats_for _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats =
    Compile.compile_statements ctx fixture_for
    |> Type_check.check_statements ctx'
  in
  let result =
    Generate.gen_statements stats 0 |> String.concat
    |> Util.run_shell ~tmp_file_name:"test-for" |> String.concat
  in
  assert_equal "6" result


let gen_stats_for_1 _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats () =
    Compile.compile_statements ctx fixture_for_1
    |> Type_check.check_statements ctx' |> ignore
  in
  try stats () with Type_check.Type_err s ->
    Util.assert_string_contains s
      "expect type (Ir.List_type Ir.Unknown_type), got Ir.Str_type"


let gen_stats_while _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats =
    Compile.compile_statements ctx fixture_while
    |> Type_check.check_statements ctx'
  in
  let result =
    Generate.gen_statements stats 0 |> String.concat
    |> Util.run_shell ~tmp_file_name:"test-while" |> String.concat
  in
  assert_equal "5" result


let gen_stats_fun_def _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats =
    Compile.compile_statements ctx fixture_fun_def
    |> Type_check.check_statements ctx'
  in
  let result =
    Generate.gen_statements stats 0 |> String.concat
    |> Util.run_shell ~tmp_file_name:"test-fun-def" |> String.concat
  in
  assert_equal "a,1,0" result


let gen_stats_fun_def_1 _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats () =
    Compile.compile_statements ctx fixture_fun_def_1
    |> Type_check.check_statements ctx' |> ignore
  in
  try stats () with Type_check.Type_err s ->
    Util.assert_string_contains s "expect type Ir.Num_type, got Ir.Str_type"


let gen_stats_return _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats =
    Compile.compile_statements ctx fixture_return
    |> Type_check.check_statements ctx'
  in
  let result =
    Generate.gen_statements stats 0 |> String.concat
    |> Util.run_shell ~tmp_file_name:"test-return" |> String.concat
  in
  assert_equal "aaa" result


let gen_stats_value _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats =
    Compile.compile_statements ctx fixture_value
    |> Type_check.check_statements ctx'
  in
  let result =
    Generate.gen_statements stats 0 |> String.concat
    |> Util.run_shell ~tmp_file_name:"test-value" |> String.concat
  in
  assert_equal "aaa" result


let gen_comment _ =
  let ctx = Compile.make_ctx () in
  let ctx' = Type_check.make_context () in
  let stats =
    Compile.compile_statements ctx fixture_comment
    |> Type_check.check_statements ctx'
  in
  let result =
    Generate.gen_statements stats 0 |> String.concat
    |> Util.run_shell ~tmp_file_name:"test-comment" |> String.concat
  in
  assert_equal "1" result


let suite =
  "suite"
  >::: [ "gen_stats_assign" >:: gen_stats_assign
       ; "gen_stats_not_defined" >:: gen_stats_not_defined
       ; "gen_stats_if" >:: gen_stats_if
       ; "gen_stats_if_1" >:: gen_stats_if_1
       ; "gen_stats_for" >:: gen_stats_for
       ; "gen_stats_for_1" >:: gen_stats_for_1
       ; "gen_stats_num_binary" >:: gen_stats_num_binary
       ; "gen_stats_num_binary_1" >:: gen_stats_num_binary_1
       ; "gen_stats_num_binary_2" >:: gen_stats_num_binary_2
       ; "gen_stats_str_binary" >:: gen_stats_str_binary
       ; "gen_stats_str_binary_1" >:: gen_stats_str_binary_1
       ; "gen_stats_bool_binary" >:: gen_stats_bool_binary
       ; "gen_stats_bool_binary_1" >:: gen_stats_bool_binary_1
       ; "gen_stats_bool_binary_2" >:: gen_stats_bool_binary_2
       ; "gen_stats_while" >:: gen_stats_while
       ; "gen_stats_fun_def" >:: gen_stats_fun_def
       ; "gen_stats_fun_def_1" >:: gen_stats_fun_def_1
       ; "gen_stats_return" >:: gen_stats_return
       ; "gen_stats_value" >:: gen_stats_value
       ; "gen_comment" >:: gen_comment ]
       @ Builtin_test.suite


let () = run_test_tt_main suite
