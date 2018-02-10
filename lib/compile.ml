open Core
open Syntax

exception Compile_err of string

type context =
  { mutable scope: Ir.statement Scope.ListScope.t
  ; mutable local_vars: unit Scope.StrMapScope.t
  ; mutable gen_var: int
  ; mutable while_loop_statements: Ir.statement Scope.ListScope.t
  (* ; mutable result_statements: Ir.statement list *) }

let make_ctx () =
  { scope= Scope.ListScope.make ()
  ; gen_var= 0 (* ; result_statements= [] *)
  ; local_vars= Scope.StrMapScope.make ()
  ; while_loop_statements= Scope.ListScope.make () }


let new_scope ctx =
  let origin = ctx.scope in
  let () = ctx.scope <- Scope.ListScope.new_level ctx.scope in
  origin


let recover_scope ctx scope = ctx.scope <- scope

let with_scope ctx (f: unit -> 'a) : 'a =
  let origin = new_scope ctx in
  let r = f () in
  let () = recover_scope ctx origin in
  r


let add_statement ctx stat = Scope.ListScope.add ctx.scope () stat

let get_statements ctx = Scope.ListScope.level_values ctx.scope |> List.rev

let generate_tmp_var ctx =
  let origin = ctx.gen_var in
  let () = ctx.gen_var <- ctx.gen_var + 1 in
  "_var_" ^ Int.to_string origin


let add_local_vars ctx vars =
  List.map vars ~f:(fun var -> Scope.StrMapScope.add ctx.local_vars var ())
  |> ignore


let is_local_var ctx var =
  Option.is_some (Scope.StrMapScope.find ctx.local_vars var)


let with_while_loop_statements ctx statements (f: unit -> 'a) : 'a =
  let origin = ctx.while_loop_statements in
  let () =
    ctx.while_loop_statements
    <- Scope.ListScope.new_level ctx.while_loop_statements
  in
  let () =
    List.map statements ~f:(fun s ->
        Scope.ListScope.add ctx.while_loop_statements () s )
    |> ignore
  in
  let r = f () in
  let () = ctx.while_loop_statements <- origin in
  r


let get_while_loop_statements ctx =
  Scope.ListScope.level_values ctx.while_loop_statements


let string_to_tp s =
  let replace_spaces s =
    String.split_on_chars ~on:[' '; '\t'] s
    |> List.filter ~f:(fun e -> e <> "") |> String.concat ~sep:" "
  in
  let s' = String.strip s |> replace_spaces in
  match s' with
  | "num" -> Ir.Num_type
  | "string" -> Ir.Str_type
  | "bool" -> Ir.Bool_type
  | "num list" -> Ir.List_type Ir.Num_type
  | "string list" -> Ir.List_type Ir.Str_type
  | "bool list" -> Ir.List_type Ir.Bool_type
  | _ -> raise (Compile_err ("unsupported type: " ^ s'))


(*
   extract_leftvalue:
   1. false:
         caller dont need leftvalue
   2. true
         caller need leftvalue
   3. inherit
         caller dont care
 *)

let rec compile_value ?(extract_leftvalue= false) ctx (value: value) : Ir.value =
  match value with
  | Fun_call {v= symbol, value_l; pos} ->
      let value_l' =
        List.map value_l ~f:(compile_value ~extract_leftvalue:false ctx)
      in
      let tmp_var = generate_tmp_var ctx in
      let () =
        add_statement ctx
          (Ir.Assignment
             ( Ir.Identifier
                 { v= (tmp_var, Ir.Unknown_type, is_local_var ctx tmp_var)
                 ; meta= {pos= Some pos} }
             , Ir.Fun_call {v= (symbol, value_l'); meta= {pos= Some pos}} ))
      in
      Ir.Left_value
        { v=
            Ir.Identifier
              { v= (tmp_var, Ir.Unknown_type, is_local_var ctx tmp_var)
              ; meta= {pos= Some pos} }
        ; meta= {pos= Some pos} }
  | Basic_value {v; pos} ->
      let is_symbol v = match v with Symbol _ -> true | _ -> false in
      let value' =
        match v with
        | Num num -> Ir.Num_value {v= Ir.Num num; meta= {pos= Some pos}}
        | String s -> Ir.Str_value {v= Ir.Str s; meta= {pos= Some pos}}
        | Bool v -> Ir.Bool_value {v= Ir.Bool v; meta= {pos= Some pos}}
        | List vl ->
            Ir.List
              { v=
                  List.rev
                    (List.fold vl ~init:[] ~f:(fun r v ->
                         compile_value ~extract_leftvalue:false ctx v :: r ))
              ; meta= {pos= Some pos} }
        | Symbol v ->
            Ir.Left_value
              { v=
                  Ir.Identifier
                    { v= (v, Ir.Unknown_type, is_local_var ctx v)
                    ; meta= {pos= Some pos} }
              ; meta= {pos= Some pos} }
      in
      if extract_leftvalue && not (is_symbol v) then
        let tmp_var = generate_tmp_var ctx in
        let () =
          add_statement ctx
            (Ir.Assignment
               ( Ir.Identifier
                   { v= (tmp_var, Ir.Unknown_type, is_local_var ctx tmp_var)
                   ; meta= {pos= Some pos} }
               , value' ))
        in
        Ir.Left_value
          { v=
              Ir.Identifier
                { v= (tmp_var, Ir.Unknown_type, is_local_var ctx tmp_var)
                ; meta= {pos= Some pos} }
          ; meta= {pos= Some pos} }
      else value'
  | Op_value {v= Op (v1, op, v2); pos} ->
      let v1' = compile_value ~extract_leftvalue:true ctx v1 in
      let v2' = compile_value ~extract_leftvalue:true ctx v2 in
      let result_value =
        match (v1', v2', op) with
        | Left_value {v= v1}, Left_value {v= v2}, String_plus ->
            Ir.Str_value
              { v= Ir.Str_leftvalue_binary (Ir.Str_plus, v1, v2)
              ; meta= {pos= Some pos} }
        | Left_value {v= v1}, Left_value {v= v2}, List_plus ->
            Ir.List_binary_value
              { v= Ir.List_leftvalue_binary (Ir.List_plus, v1, v2)
              ; meta= {pos= Some pos} }
        | Left_value {v= v1}, Left_value {v= v2}, Plus ->
            Ir.Num_value
              { v= Ir.Num_leftvalue_binary (Ir.Plus, v1, v2)
              ; meta= {pos= Some pos} }
        | Left_value {v= v1}, Left_value {v= v2}, Minus ->
            Ir.Num_value
              { v= Ir.Num_leftvalue_binary (Ir.Minus, v1, v2)
              ; meta= {pos= Some pos} }
        | Left_value {v= v1}, Left_value {v= v2}, Mul ->
            Ir.Num_value
              { v= Ir.Num_leftvalue_binary (Ir.Mul, v1, v2)
              ; meta= {pos= Some pos} }
        | Left_value {v= v1}, Left_value {v= v2}, Div ->
            Ir.Num_value
              { v= Ir.Num_leftvalue_binary (Ir.Div, v1, v2)
              ; meta= {pos= Some pos} }
        | Left_value {v= v1}, Left_value {v= v2}, Gt ->
            Ir.Bool_value
              { v= Ir.Bool_leftvalue_binary (Ir.Gt, v1, v2)
              ; meta= {pos= Some pos} }
        | Left_value {v= v1}, Left_value {v= v2}, Lt ->
            Ir.Bool_value
              { v= Ir.Bool_leftvalue_binary (Ir.Lt, v1, v2)
              ; meta= {pos= Some pos} }
        | Left_value {v= v1}, Left_value {v= v2}, Ge ->
            Ir.Bool_value
              { v= Ir.Bool_leftvalue_binary (Ir.Ge, v1, v2)
              ; meta= {pos= Some pos} }
        | Left_value {v= v1}, Left_value {v= v2}, Le ->
            Ir.Bool_value
              { v= Ir.Bool_leftvalue_binary (Ir.Le, v1, v2)
              ; meta= {pos= Some pos} }
        | Left_value {v= v1}, Left_value {v= v2}, Neq ->
            Ir.Bool_value
              { v= Ir.Bool_leftvalue_binary (Ir.Neq, v1, v2)
              ; meta= {pos= Some pos} }
        | Left_value {v= v1}, Left_value {v= v2}, Eq ->
            Ir.Bool_value
              { v= Ir.Bool_leftvalue_binary (Ir.Eq, v1, v2)
              ; meta= {pos= Some pos} }
        | _, _, _ -> assert false
      in
      if extract_leftvalue then
        let tmp_var = generate_tmp_var ctx in
        let () =
          add_statement ctx
            (Ir.Assignment
               ( Ir.Identifier
                   { v= (tmp_var, Ir.Unknown_type, is_local_var ctx tmp_var)
                   ; meta= {pos= Some pos} }
               , result_value ))
        in
        Ir.Left_value
          { v=
              Ir.Identifier
                { v= (tmp_var, Ir.Unknown_type, is_local_var ctx tmp_var)
                ; meta= {pos= Some pos} }
          ; meta= {pos= Some pos} }
      else result_value


let zero_value tp =
  match tp with
  | Ir.Num_type -> Ir.Num_value {v= Ir.Num 0; meta= {pos= None}}
  | Ir.Str_type -> Ir.Str_value {v= Ir.Str ""; meta= {pos= None}}
  | Ir.List_type _ -> Ir.List {v= []; meta= {pos= None}}
  | Ir.Bool_type -> Ir.Bool_value {v= Ir.Bool false; meta= {pos= None}}
  | Ir.Unknown_type -> assert false


let rec compile_statement ctx (statement: statement) : Ir.statements =
  let f () =
    match statement with
    | Assignment {v= symbol, value; symbol_pos} ->
        let ir_value = compile_value ctx value in
        let () =
          add_statement ctx
            (Ir.Assignment
               ( Ir.Identifier
                   { v= (symbol, Ir.Unknown_type, is_local_var ctx symbol)
                   ; meta= {pos= Some symbol_pos} }
               , ir_value ))
        in
        get_statements ctx
    | If (value, statements, Some else_statements) ->
        let condition = compile_value ctx value in
        let cond_statements = get_statements ctx in
        let statements' = compile_statements ctx statements in
        let else_statements' = compile_statements ctx else_statements in
        cond_statements
        @ [Ir.If_else (condition, statements', else_statements')]
    | If (value, statements, None) ->
        let condition = compile_value ctx value in
        let cond_statements = get_statements ctx in
        let statements' = compile_statements ctx statements in
        cond_statements @ [Ir.If (condition, statements')]
    | For {v= symbol, value, statements; symbol_pos} ->
        let loop_value = compile_value ~extract_leftvalue:true ctx value in
        let loop_statements = get_statements ctx in
        let statements' = compile_statements ctx statements in
        let lv =
          Ir.Identifier
            { v= (symbol, Ir.Unknown_type, is_local_var ctx symbol)
            ; meta= {pos= Some symbol_pos} }
        in
        loop_statements @ [Ir.For (lv, loop_value, statements')]
    | While (value, statements) ->
        let loop_value = compile_value ctx value in
        let loop_statements = get_statements ctx in
        let statements' =
          with_while_loop_statements ctx loop_statements (fun () ->
              compile_statements ctx statements )
        in
        (* here append loop_statements in WHILE body because
           the WHOLE loop_value is separated into LOOP_VALUE and LOOP_STATEMENTS
         *)
        loop_statements @ [Ir.While (loop_value, statements' @ loop_statements)]
    | Function (symbol, arg_l, statements, fun_tp) ->
        add_local_vars ctx arg_l ;
        let statements' = compile_statements ctx statements in
        let fun_tp_arg =
          List.sub fun_tp ~len:(List.length fun_tp - 1) ~pos:0
          |> List.map ~f:(fun t -> [string_to_tp t])
        in
        let fun_tp_result =
          List.nth_exn fun_tp (List.length fun_tp - 1) |> string_to_tp
        in
        let append_return_statement =
          let open Option in
          List.last statements'
          >>= (fun s ->
                match s with
                | Ir.Return _ -> None
                | _ -> return @@ Ir.Return (zero_value fun_tp_result))
          |> to_list
        in
        [ Ir.Fun_def
            ( symbol
            , arg_l
            , statements' @ append_return_statement
            , Ir.(`Normal (fun_tp_arg, fun_tp_result)) ) ]
    | Return value ->
        let value' = compile_value ctx value in
        let statements = get_statements ctx in
        statements @ [Ir.Return value']
    | Value value ->
        let value' = compile_value ctx value in
        let statements = get_statements ctx in
        statements @ [Ir.Value value']
    | Break {pos} ->
        let loop_statements = get_while_loop_statements ctx in
        loop_statements @ [Ir.Break {meta= {pos= Some pos}}]
    | Continue {pos} ->
        let loop_statements = get_while_loop_statements ctx in
        loop_statements @ [Ir.Continue {meta= {pos= Some pos}}]
  in
  with_scope ctx f


and compile_statements ctx (statements: statements) : Ir.statements =
  List.fold statements ~init:[] ~f:(fun r statement ->
      r @ compile_statement ctx statement )
