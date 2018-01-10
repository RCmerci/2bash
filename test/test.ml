open Core
open OUnit2
open Sbash_lib

let fixture_value =
  Syntax.Op_value
    (Syntax.Op
       ( Syntax.Op_value
           (Syntax.Op
              ( Syntax.Basic_value (Syntax.Num 1)
              , Syntax.Plus
              , Syntax.Basic_value (Syntax.Num 2) ))
       , Syntax.Gt
       , Syntax.Op_value
           (Syntax.Op
              ( Syntax.Basic_value (Syntax.Num 3)
              , Syntax.Plus
              , Syntax.Fun_call
                  ("call", [Syntax.Basic_value (Syntax.String "1")]) )) ))


let fixture_stat =
  Syntax.Value
    (Syntax.Op_value
       (Syntax.Op
          ( Syntax.Op_value
              (Syntax.Op
                 ( Syntax.Basic_value (Syntax.Num 1)
                 , Syntax.Plus
                 , Syntax.Basic_value (Syntax.Num 2) ))
          , Syntax.Gt
          , Syntax.Op_value
              (Syntax.Op
                 ( Syntax.Basic_value (Syntax.Num 3)
                 , Syntax.Plus
                 , Syntax.Fun_call
                     ("call", [Syntax.Basic_value (Syntax.String "1")]) )) )))


let fixture_stats =
  [ Syntax.If
      ( Syntax.Op_value
          (Syntax.Op
             ( Syntax.Op_value
                 (Syntax.Op
                    ( Syntax.Basic_value (Syntax.Num 1)
                    , Syntax.Plus
                    , Syntax.Basic_value (Syntax.Num 2) ))
             , Syntax.Gt
             , Syntax.Op_value
                 (Syntax.Op
                    ( Syntax.Basic_value (Syntax.Num 3)
                    , Syntax.Plus
                    , Syntax.Fun_call
                        ("call", [Syntax.Basic_value (Syntax.String "1")]) )) ))
      , [ Syntax.Value
            (Syntax.Fun_call
               ( "print"
               , [ Syntax.Op_value
                     (Syntax.Op
                        ( Syntax.Basic_value (Syntax.Num 1)
                        , Syntax.Plus
                        , Syntax.Basic_value (Syntax.Num 2) )) ] ))
        ; Syntax.Value (Syntax.Fun_call ("foo", [])) ]
      , Some
          [ Syntax.Value (Syntax.Fun_call ("ahhaha", []))
          ; Syntax.Value (Syntax.Basic_value (Syntax.Symbol "a")) ] ) ]


let compile_value_1 ctx =
  let context = Compile.make_ctx in
  Compile.compile_value context fixture_value |> ignore


let compile_statement_1 _ctx =
  let ctx = Compile.make_ctx in
  Compile.compile_statement ctx fixture_stat |> ignore


let compile_statements_1 _ctx =
  let ctx = Compile.make_ctx in
  Compile.compile_statements ctx fixture_stats |> ignore


let suite =
  "suite"
  >::: [ "compile_value_1" >:: compile_value_1
       ; "compile_statement_1" >:: compile_statement_1
       ; "compile_statements_1" >:: compile_statements_1 ]


let () = run_test_tt_main suite
