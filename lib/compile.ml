open Core
open Syntax

type context =
  { mutable scope: Ir.statement Scope.ListScope.t
  ; mutable gen_var: int
  ; mutable result_statements: Ir.statement list }

let make_ctx =
  {scope= Scope.ListScope.make (); gen_var= 0; result_statements= []}


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

let emit_statements ctx =
  let level_values = Scope.ListScope.level_values ctx.scope in
  ctx.result_statements <- ctx.result_statements @ List.rev level_values


let generate_tmp_var ctx =
  let origin = ctx.gen_var in
  let () = ctx.gen_var <- ctx.gen_var + 1 in
  "_var_" ^ Int.to_string origin


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
  | Fun_call (symbol, value_l) ->
      let value_l' =
        List.map value_l ~f:(compile_value ~extract_leftvalue:false ctx)
      in
      if extract_leftvalue then
        let tmp_var = generate_tmp_var ctx in
        let () =
          add_statement ctx
            (Ir.Assignment
               (Ir.Identifier tmp_var, Ir.Fun_call (symbol, value_l')))
        in
        Ir.Left_value (Ir.Identifier tmp_var)
      else Ir.Fun_call (symbol, value_l')
  | Basic_value v ->
      let value' =
        match v with
        | Num num -> Ir.Num_value (Ir.Num num)
        | String s -> Ir.Str_value (Ir.Str s)
        | Bool v -> Ir.Bool_value (Ir.Bool v)
        | List vl ->
            Ir.List_value
              (Ir.List
                 (List.fold vl ~init:[] ~f:(fun r v ->
                      compile_value ~extract_leftvalue:false ctx v :: r )))
        | Symbol v -> Ir.Left_value (Ir.Identifier v)
      in
      if extract_leftvalue then
        let tmp_var = generate_tmp_var ctx in
        let () =
          add_statement ctx (Ir.Assignment (Ir.Identifier tmp_var, value'))
        in
        Ir.Left_value (Ir.Identifier tmp_var)
      else value'
  | Op_value Op (v1, op, v2) ->
      let v1' = compile_value ~extract_leftvalue:true ctx v1 in
      let v2' = compile_value ~extract_leftvalue:true ctx v2 in
      let result_value =
        match (v1', v2', op) with
        | Left_value v1, Left_value v2, String_plus ->
            Ir.Str_value
              (Ir.Str_binary
                 (Ir.Str_plus, Ir.Str_left_value v1, Ir.Str_left_value v2))
        | Left_value v1, Left_value v2, List_plus ->
            Ir.List_value
              (Ir.List_binary
                 (Ir.List_plus, Ir.List_left_value v1, Ir.List_left_value v2))
        | Left_value v1, Left_value v2, Plus ->
            Ir.Num_value
              (Ir.Num_binary
                 (Ir.Plus, Ir.Num_left_value v1, Ir.Num_left_value v2))
        | Left_value v1, Left_value v2, Minus ->
            Ir.Num_value
              (Ir.Num_binary
                 (Ir.Minus, Ir.Num_left_value v1, Ir.Num_left_value v2))
        | Left_value v1, Left_value v2, Mul ->
            Ir.Num_value
              (Ir.Num_binary
                 (Ir.Mul, Ir.Num_left_value v1, Ir.Num_left_value v2))
        | Left_value v1, Left_value v2, Div ->
            Ir.Num_value
              (Ir.Num_binary
                 (Ir.Div, Ir.Num_left_value v1, Ir.Num_left_value v2))
        | Left_value v1, Left_value v2, Gt ->
            Ir.Bool_value
              (Ir.Bool_bool_binary
                 (Ir.Gt, Ir.Bool_left_value v1, Ir.Bool_left_value v2))
        | Left_value v1, Left_value v2, Lt ->
            Ir.Bool_value
              (Ir.Bool_bool_binary
                 (Ir.Lt, Ir.Bool_left_value v1, Ir.Bool_left_value v2))
        | Left_value v1, Left_value v2, Ge ->
            Ir.Bool_value
              (Ir.Bool_bool_binary
                 (Ir.Ge, Ir.Bool_left_value v1, Ir.Bool_left_value v2))
        | Left_value v1, Left_value v2, Le ->
            Ir.Bool_value
              (Ir.Bool_bool_binary
                 (Ir.Le, Ir.Bool_left_value v1, Ir.Bool_left_value v2))
        | Left_value v1, Left_value v2, Neq ->
            Ir.Bool_value
              (Ir.Bool_bool_binary
                 (Ir.Neq, Ir.Bool_left_value v1, Ir.Bool_left_value v2))
        | Left_value v1, Left_value v2, Eq ->
            Ir.Bool_value
              (Ir.Bool_bool_binary
                 (Ir.Eq, Ir.Bool_left_value v1, Ir.Bool_left_value v2))
        | _, _, _ -> assert false
      in
      if extract_leftvalue then
        let tmp_var = generate_tmp_var ctx in
        let () =
          add_statement ctx
            (Ir.Assignment (Ir.Identifier tmp_var, result_value))
        in
        Ir.Left_value (Ir.Identifier tmp_var)
      else result_value


let rec compile_statement ctx (statement: statement) : Ir.statements =
  let f () =
    match statement with
    | Assignment (symbol, value) ->
        let ir_value = compile_value ctx value in
        let () =
          add_statement ctx (Ir.Assignment (Ir.Identifier symbol, ir_value))
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
    | For (symbol, value, statements) ->
        let loop_value = compile_value ctx value in
        let loop_statements = get_statements ctx in
        let statements' = compile_statements ctx statements in
        loop_statements @ [Ir.For (symbol, loop_value, statements')]
    | While (value, statements) ->
        let loop_value = compile_value ctx value in
        let loop_statements = get_statements ctx in
        let statements' = compile_statements ctx statements in
        loop_statements @ [Ir.While (loop_value, statements')]
    | Function (symbol, arg_l, statements) ->
        let statements' = compile_statements ctx statements in
        [Ir.Fun_def (symbol, arg_l, statements')]
    | Return value ->
        let value' = compile_value ctx value in
        let statements = get_statements ctx in
        statements @ [Ir.Return value']
    | Value value ->
        let value' = compile_value ctx value in
        let statements = get_statements ctx in
        statements @ [Ir.Value value']
  in
  with_scope ctx f


and compile_statements ctx (statements: statements) : Ir.statements =
  List.fold statements ~init:[] ~f:(fun r statement ->
      r @ compile_statement ctx statement )
