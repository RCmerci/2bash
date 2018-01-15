open Core
open Ir
module S = Scope.StrMapScope

type context = {mutable scope: tp S.t; mutable fun_scope: fun_tp S.t}

exception Type_err of string

let make_context () =
  let fun_scope = S.make () in
  Builtin.inject_builtin_tp fun_scope ;
  {scope= S.make (); fun_scope}


let raise_type_err t expect =
  raise
    (Type_err
       (Printf.sprintf "expect %s , got %s" (show_tp expect) (show_tp t)))


let find_var_tp_local ctx name =
  let r = S.find_local ctx.scope name in
  Option.(r >>= fun (tp, _) -> return tp)


let find_var_tp ctx var_name =
  let r = S.find ctx.scope var_name in
  Option.(r >>= fun (tp, _) -> return tp)


let find_fun_tp ctx fun_name =
  let r = S.find ctx.fun_scope fun_name in
  Option.(r >>= fun (tp, _) -> return tp)


let with_scope ctx (f: unit -> 'a) : 'a =
  let new_scope ctx =
    let origin = ctx.scope in
    let () = ctx.scope <- S.new_level ctx.scope in
    origin
  in
  let recover_scope ctx scope = ctx.scope <- scope in
  let origin = new_scope ctx in
  let r = f () in
  let () = recover_scope ctx origin in
  r


let add_fun_tp ctx name tp = S.add ctx.fun_scope name tp

let add_leftvalue_tp ctx name tp = S.add ctx.scope name tp

let rec get_leftvalue_tp_local ctx v =
  match v with
  | Identifier (name, _, _) -> find_var_tp_local ctx name
  | ListAccess ((v', _), _) ->
      let open Option in
      get_leftvalue_tp_local ctx v'
      >>= fun e -> match e with List_type tp' -> return tp' | _ -> None


let rec get_leftvalue_tp ctx v =
  match v with
  | Identifier (name, _, _) -> find_var_tp ctx name
  | ListAccess ((v', _), _) ->
      let open Option in
      get_leftvalue_tp ctx v'
      >>= fun e -> match e with List_type tp' -> return tp' | _ -> None


let rec extract_leftvalue v =
  match v with
  | Identifier (name, _, _) -> name
  | ListAccess ((lv, _), _) -> extract_leftvalue lv ^ "[i]"


let get_leftvalue_tp_exn ctx v =
  try Option.value_exn (get_leftvalue_tp ctx v) with _ ->
    raise (Type_err ("can not infer type of " ^ extract_leftvalue v))


let get_fun_tp = find_fun_tp

let expect_tp ~name t expect =
  match (expect, t) with
  | List_type _, List_type Unknown_type -> ()
  | List_type Unknown_type, List_type _ -> ()
  | _ ->
      if not (t = expect) then
        raise
          (Type_err
             (Printf.sprintf "[%s] expect type %s, got %s" name
                (show_tp expect) (show_tp t)))


let rec leftvalue_name v =
  match v with
  | Identifier (name, _, _) -> name
  | ListAccess ((name, index), _) -> leftvalue_name name ^ "[i]"


let rec inject_leftvalue_tp v tp =
  match v with
  | Identifier (name, _, is_local) -> Identifier (name, tp, is_local)
  | ListAccess ((lv, index), _) ->
      ListAccess ((inject_leftvalue_tp lv (List_type tp), index), tp)


let rec get_list_binary_tp ctx v =
  match v with
  | List_binary (_, v1, _) -> get_list_binary_tp ctx v1
  | List_leftvalue_binary (_, v1, _) -> get_leftvalue_tp_exn ctx v1


let rec get_value_tp ctx v =
  match v with
  | Left_value v -> get_leftvalue_tp_exn ctx v
  | Num_value _ -> Num_type
  | Str_value _ -> Str_type
  | Bool_value _ -> Bool_type
  | List_binary_value v -> get_list_binary_tp ctx v
  | List vl -> (
      let length = List.length vl in
      match length with
      | 0 -> List_type Unknown_type
      | 1 ->
          let tp = List.nth_exn vl 0 |> get_value_tp ctx in
          tp
      | _ ->
          let tp = List.nth_exn vl 0 |> get_value_tp ctx in
          List.map vl ~f:(fun e ->
              expect_tp ~name:"list" (get_value_tp ctx e) tp )
          |> ignore ;
          tp )
  | Fun_call (name, vl) ->
      let fun_tp = get_fun_tp ctx name in
      if Option.is_none fun_tp then
        raise (Type_err ("can not infer function type: " ^ name))
      else
        let fun_tp' = Option.value_exn fun_tp in
        match fun_tp' with
        | `Normal (argl, r) ->
            List.map2 vl argl ~f:(fun a b ->
                let desc = Printf.sprintf "func[%s] arg" name in
                expect_tp ~name:desc (get_value_tp ctx a) b )
            |> ignore ;
            r
        | `Indefinite (argl, r) ->
            if List.length argl > List.length vl then
              raise
                (Type_err
                   (Printf.sprintf "func[%s] expect arg length > given args"
                      name))
            else
              List.mapi vl ~f:(fun ind e ->
                  let desc = Printf.sprintf "func[%s] arg" name in
                  expect_tp ~name:desc (get_value_tp ctx e)
                    (List.nth_exn argl ind) )
              |> ignore ;
            r


let rec check_num_binary ctx v =
  match v with
  | Num_leftvalue_binary (op, v1, v2) ->
      expect_tp ~name:(leftvalue_name v1)
        (get_leftvalue_tp_exn ctx v1)
        Num_type ;
      expect_tp ~name:(leftvalue_name v2)
        (get_leftvalue_tp_exn ctx v2)
        Num_type ;
      let v1' = inject_leftvalue_tp v1 Num_type in
      let v2' = inject_leftvalue_tp v2 Num_type in
      Num_leftvalue_binary (op, v1', v2')
  | Num_binary (op, v1, v2) ->
      let v1' = check_num_binary ctx v1 in
      let v2' = check_num_binary ctx v2 in
      Num_binary (op, v1', v2')
  | Num _ -> v


and check_str_binary ctx v =
  match v with
  | Str_leftvalue_binary (op, v1, v2) ->
      expect_tp ~name:(leftvalue_name v1)
        (get_leftvalue_tp_exn ctx v1)
        Str_type ;
      expect_tp ~name:(leftvalue_name v2)
        (get_leftvalue_tp_exn ctx v2)
        Str_type ;
      let v1' = inject_leftvalue_tp v1 Str_type in
      let v2' = inject_leftvalue_tp v2 Str_type in
      Str_leftvalue_binary (op, v1', v2')
  | Str_binary (op, v1, v2) ->
      let v1' = check_str_binary ctx v1 in
      let v2' = check_str_binary ctx v2 in
      Str_binary (op, v1', v2')
  | Str _ -> v


and check_bool_binary ctx v =
  match v with
  | Bool_leftvalue_binary (op, v1, v2) ->
      expect_tp ~name:(leftvalue_name v1)
        (get_leftvalue_tp_exn ctx v1)
        (get_leftvalue_tp_exn ctx v2) ;
      let v1' = inject_leftvalue_tp v1 (get_leftvalue_tp_exn ctx v1) in
      let v2' = inject_leftvalue_tp v2 (get_leftvalue_tp_exn ctx v2) in
      Bool_leftvalue_binary (op, v1', v2')
  | Bool_bool_binary (op, v1, v2) ->
      let v1' = check_bool_binary ctx v1 in
      let v2' = check_bool_binary ctx v2 in
      Bool_bool_binary (op, v1', v2')
  | Str_bool_binary (op, v1, v2) ->
      let v1' = check_str_binary ctx v1 in
      let v2' = check_str_binary ctx v2 in
      Str_bool_binary (op, v1', v2')
  | Num_bool_binary (op, v1, v2) ->
      let v1' = check_num_binary ctx v1 in
      let v2' = check_num_binary ctx v2 in
      Num_bool_binary (op, v1', v2')
  | Bool _ -> v


and check_list_binary ctx v =
  match v with
  | List_leftvalue_binary (op, v1, v2) ->
      expect_tp ~name:(leftvalue_name v1)
        (get_leftvalue_tp_exn ctx v1)
        (List_type Unknown_type) ;
      expect_tp ~name:(leftvalue_name v2)
        (get_leftvalue_tp_exn ctx v2)
        (List_type Unknown_type) ;
      expect_tp ~name:(leftvalue_name v1)
        (get_leftvalue_tp_exn ctx v1)
        (Option.value_exn (get_leftvalue_tp ctx v2)) ;
      let v1' =
        inject_leftvalue_tp v1 (Option.value_exn (get_leftvalue_tp ctx v1))
      in
      let v2' =
        inject_leftvalue_tp v2 (Option.value_exn (get_leftvalue_tp ctx v2))
      in
      List_leftvalue_binary (op, v1', v2')
  | List_binary (op, v1, v2) ->
      let v1' = check_list_binary ctx v1 in
      let v2' = check_list_binary ctx v2 in
      List_binary (op, v1', v2')


and check_value ctx v =
  match v with
  | Left_value v ->
      Left_value
        (inject_leftvalue_tp v (Option.value_exn (get_leftvalue_tp ctx v)))
  | Num_value v -> Num_value (check_num_binary ctx v)
  | Str_value v -> Str_value (check_str_binary ctx v)
  | Bool_value v -> Bool_value (check_bool_binary ctx v)
  | List_binary_value v -> List_binary_value (check_list_binary ctx v)
  | List vl -> List (List.map vl ~f:(fun e -> check_value ctx e))
  | Fun_call (name, vl) ->
      Fun_call (name, List.map vl ~f:(fun e -> check_value ctx e))


and check_statement ctx s =
  match s with
  | Assignment (lv, v) ->
      let v' = check_value ctx v in
      let v_tp = get_value_tp ctx v' in
      let () =
        match lv with
        | Identifier (name, _, is_local) ->
            if is_local then (
              let tp = get_leftvalue_tp_local ctx lv in
              if Option.is_some tp then
                if Option.value_exn tp <> v_tp then
                  raise
                    (Type_err
                       (Printf.sprintf "var[%s] has type %s, can not be %s"
                          name
                          (show_tp (Option.value_exn tp))
                          (show_tp v_tp))) )
            else
              let tp = get_leftvalue_tp ctx lv in
              if Option.is_some tp then
                if Option.value_exn tp <> v_tp then
                  raise
                    (Type_err
                       (Printf.sprintf "var[%s] has type %s, can not be %s"
                          name
                          (show_tp (Option.value_exn tp))
                          (show_tp v_tp))) ;
              add_leftvalue_tp ctx name v_tp
        | ListAccess ((lv', _), _) ->
            let tp = get_leftvalue_tp_exn ctx lv' in
            let name = extract_leftvalue lv in
            let inner_name = extract_leftvalue lv' in
            match tp with
            | List_type inner ->
                if inner <> v_tp then
                  raise
                    (Type_err
                       (Printf.sprintf "var[%s] has type %s, can not be %s"
                          name (show_tp inner) (show_tp v_tp)))
            | _ ->
                raise
                  (Type_err
                     (Printf.sprintf
                        "var[%s] has type %s, can not apply list access"
                        inner_name (show_tp tp)))
      in
      Assignment (inject_leftvalue_tp lv v_tp, v')
  | If (v, stats) ->
      let v' = check_value ctx v in
      let stats' = check_statements ctx stats in
      If (v', stats')
  | If_else (v, stats1, stats2) ->
      let v' = check_value ctx v in
      let stats1' = check_statements ctx stats1 in
      let stats2' = check_statements ctx stats2 in
      If_else (v', stats1', stats2')
  | For (i, v, stats) ->
      let v' = check_value ctx v in
      let v_tp = get_value_tp ctx v' in
      let tp = get_leftvalue_tp ctx (Identifier (i, Unknown_type, false)) in
      if Option.is_some tp then
        if Option.value_exn tp <> v_tp then
          raise
            (Type_err
               (Printf.sprintf "var[%s] has type %s, can not be %s" i
                  (show_tp (Option.value_exn tp))
                  (show_tp v_tp))) ;
      add_leftvalue_tp ctx i v_tp ;
      let stats' = check_statements ctx stats in
      For (i, v', stats')
  | While (v, stats) ->
      let v' = check_value ctx v in
      let v_tp = get_value_tp ctx v' in
      expect_tp ~name:"while condition" v_tp Bool_type ;
      let stats' = check_statements ctx stats in
      While (v', stats')
  | Return v ->
      let v' = check_value ctx v in
      let v_tp = get_value_tp ctx v' in
      if v_tp <> Str_type then
        raise (Type_err "user-defined function return type must be string") ;
      Return v'
  | Value v ->
      let v' = check_value ctx v in
      Value v'
  | Fun_def (fun_name, argl, stats, fun_tp) ->
      add_fun_tp ctx fun_name fun_tp ;
      let arg_tp =
        match fun_tp with `Normal (arg_tp, _) -> arg_tp | _ -> assert false
      in
      if List.length arg_tp <> List.length argl then
        raise
          (Type_err
             (Printf.sprintf
                "func[%s] num of args and type signature is not Compatible "
                fun_name)) ;
      let stats' =
        with_scope ctx (fun () ->
            List.map2 argl arg_tp ~f:(fun a b -> add_leftvalue_tp ctx a b)
            |> ignore ;
            check_statements ctx stats )
      in
      Fun_def (fun_name, argl, stats', fun_tp)


and check_statements ctx ss =
  List.fold ss ~init:[] ~f:(fun r e -> check_statement ctx e :: r) |> List.rev
