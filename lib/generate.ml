open Core
open Ir

exception Generate_error of string

exception Generate_type_error of string

type generated =
  [ `Quote of string
  | `Var of string
  | `Raw of string
  | `Fun_call of string -> string ]
  [@@deriving show]

let eval ?(fun_call_result_var= "") v =
  match v with
  | `Quote v' -> v'
  | `Var v' -> "${" ^ v' ^ "[@]}"
  | `Fun_call v' -> v' fun_call_result_var
  | `Raw v' -> "$(" ^ v' ^ ")"


let eval_leftvalue (v, tp, is_local) =
  let v' = match v with `Var v' -> "${" ^ v' ^ "[@]}" | _ -> assert false in
  (v', tp, is_local)


let extract v =
  match v with
  | `Quote v -> v
  | `Var v -> v
  | `Fun_call v -> v "???"
  | `Raw v -> v


let extract_leftvalue (v, tp, is_local) =
  match v with
  | `Quote v -> v
  | `Var v -> v
  | `Fun_call v -> v "???"
  | `Raw v -> v


let gen_num_bool_binary_op = function
  | Gt -> ">"
  | Lt -> "<"
  | Ge -> ">="
  | Le -> "<="
  | Neq -> "!="
  | Eq -> "=="


and gen_str_bool_binary_op = function
  | Gt -> ">"
  | Lt -> "<"
  | Neq -> "!="
  | Eq -> "="
  | _ as v ->
      raise
        (Generate_error
           ("string comparison not supprt: " ^ Ir.show_bool_binary_op v))


let gen_bool_bool_binary_op = gen_num_bool_binary_op

let rec gen_leftvalue (v: leftvalue) =
  match v with
  | Identifier {v= s, tp, is_local} -> (`Var s, tp, is_local)
  | ListAccess {v= (lv, num), tp} ->
      let lv', _, is_local = eval_leftvalue @@ gen_leftvalue lv in
      let num' = eval @@ gen_num_binary num in
      (`Var (lv' ^ "[" ^ num' ^ "]"), tp, is_local)


and gen_num_binary_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Mul -> "*"
  | Div -> "/"


and gen_num_binary (v: num_binary) =
  match v with
  | Num n -> `Quote (Int.to_string n)
  | Num_binary (op, v1, v2) ->
      let op' = gen_num_binary_op op in
      let v1' = eval @@ gen_num_binary v1 in
      let v2' = eval @@ gen_num_binary v2 in
      `Raw ("(" ^ String.concat ~sep:" " [v1'; op'; v2'] ^ ")")
  | Num_leftvalue_binary (op, v1, v2) ->
      let op' = gen_num_binary_op op in
      let v1', _, _ = eval_leftvalue @@ gen_leftvalue v1 in
      let v2', _, _ = eval_leftvalue @@ gen_leftvalue v2 in
      `Raw ("(" ^ String.concat ~sep:" " [v1'; op'; v2'] ^ ")")


(* : [`Raw of string] :> generated *)
and gen_str_binary_op = function Str_plus -> ""

and gen_str_binary (v: str_binary) =
  match v with
  | Str s -> `Quote ("\"" ^ s ^ "\"")
  | Str_binary (op, v1, v2) ->
      let op' = gen_str_binary_op op in
      let v1' = eval @@ gen_str_binary v1 in
      let v2' = eval @@ gen_str_binary v2 in
      `Quote (v1' ^ op' ^ v2')
  | Str_leftvalue_binary (op, v1, v2) ->
      let op' = gen_str_binary_op op in
      let v1', _, _ = eval_leftvalue @@ gen_leftvalue v1 in
      let v2', _, _ = eval_leftvalue @@ gen_leftvalue v2 in
      `Quote (v1' ^ op' ^ v2')


and gen_bool_binary (v: bool_binary) =
  let num_compose op v1 v2 =
    Printf.sprintf "[ 1 -eq $(echo \"%s %s %s\" | bc) ] && echo 0 || echo 1" v1
      op v2
  in
  let other_compose op v1 v2 =
    Printf.sprintf "[ %s %s %s ] && echo 0 || echo 1" v1 op v2
  in
  match v with
  | Bool true -> `Quote "0"
  | Bool false -> `Quote "1"
  | Num_bool_binary (op, v1, v2) ->
      let op' = gen_num_bool_binary_op op in
      let v1' = eval @@ gen_num_binary v1 in
      let v2' = eval @@ gen_num_binary v2 in
      `Raw (num_compose op' v1' v2')
  | Str_bool_binary (op, v1, v2) ->
      let op' = gen_str_bool_binary_op op in
      let v1' = eval @@ gen_str_binary v1 in
      let v2' = eval @@ gen_str_binary v2 in
      `Raw (other_compose op' v1' v2')
  | Bool_bool_binary (op, v1, v2) ->
      let op' = gen_bool_bool_binary_op op in
      let v1' = eval @@ gen_bool_binary v1 in
      let v2' = eval @@ gen_bool_binary v2 in
      `Raw (other_compose op' v1' v2')
  | Bool_leftvalue_binary (op, v1, v2) ->
      let op' = gen_bool_bool_binary_op op in
      let v1', tp, _ = eval_leftvalue @@ gen_leftvalue v1 in
      let v2', _, _ = eval_leftvalue @@ gen_leftvalue v2 in
      match tp with
      | Num_type -> `Raw (num_compose op' v1' v2')
      | Str_type -> `Raw (other_compose op' v1' v2')
      | Bool_type -> `Raw (other_compose op' v1' v2')
      | _ ->
          raise
            (Generate_type_error
               (Printf.sprintf "%s can not do bool-binary-op" (show_tp tp)))


and gen_list_binary (v: list_binary) =
  match v with
  | List_binary (op, v1, v2) ->
      let v1' = eval @@ gen_list_binary v1 in
      let v2' = eval @@ gen_list_binary v2 in
      `Quote ("(${" ^ v1' ^ "[@]} " ^ "${" ^ v2' ^ "[@]})")
  | List_leftvalue_binary (op, v1, v2) ->
      let v1' = extract_leftvalue @@ gen_leftvalue v1 in
      let v2' = extract_leftvalue @@ gen_leftvalue v2 in
      `Quote ("(${" ^ v1' ^ "[@]} " ^ "${" ^ v2' ^ "[@]})")


and gen_value (v: value) =
  match v with
  | Left_value {v} ->
      let v', _, _ = gen_leftvalue v in
      v'
  | Num_value {v} -> gen_num_binary v
  | Str_value {v} -> gen_str_binary v
  | Bool_value {v} -> gen_bool_binary v
  | List_binary_value {v} -> gen_list_binary v
  | List {v= vl} ->
      let vl' =
        List.fold vl ~init:[] ~f:(fun r e ->
            let e' = eval @@ gen_value e in
            e' :: r )
        |> List.rev
      in
      `Quote ("(" ^ String.concat ~sep:" " vl' ^ ")")
  | Fun_call {v= name, vl} ->
      let vl' =
        List.fold vl ~init:[] ~f:(fun r e ->
            let e' = eval @@ gen_value e in
            e' :: r )
        |> List.rev
      in
      let builtin = Builtin.gen name vl' in
      if Option.is_none builtin then
        `Fun_call
          (fun result_var ->
            String.concat ~sep:" " (name :: vl') ^ " " ^ result_var)
      else `Fun_call (Option.value_exn builtin)


let with_indent indent_level v = String.make (indent_level * 4) ' ' ^ v ^ "\n"

let with_indent_lines indent_level v =
  List.map (String.split_lines v) ~f:(fun e -> with_indent indent_level e)
  |> String.concat


let rec gen_statement (v: statement) ~(indent: int) =
  let eval_loop_value v =
    match v with `Var v' -> "${" ^ v' ^ "[@]}" | _ as v' -> eval v'
  in
  match v with
  | Assignment (lv, v) -> (
      let lv' = extract_leftvalue @@ gen_leftvalue lv in
      let v' = gen_value v in
      match v' with
      | `Fun_call _ ->
          let v'' = eval ~fun_call_result_var:lv' v' in
          v'' |> with_indent_lines indent
      | _ -> lv' ^ "=" ^ eval v' |> with_indent_lines indent )
  | If (v, stats) ->
      let v' = eval @@ gen_value v in
      let cond =
        Printf.sprintf "if [ 0 -eq %s ]; then" v' |> with_indent_lines indent
      in
      let stats' = gen_statements stats (indent + 1) in
      cond ^ String.concat stats' ^ with_indent_lines indent "fi"
  | If_else (v, stats1, stats2) ->
      let v' = eval @@ gen_value v in
      let cond =
        Printf.sprintf "if [ %s ]; then" v' |> with_indent_lines indent
      in
      let stats1' = gen_statements stats1 (indent + 1) in
      let stats2' = gen_statements stats2 (indent + 1) in
      cond ^ String.concat stats1' ^ with_indent_lines indent "else"
      ^ String.concat stats2' ^ with_indent_lines indent "fi"
  | For (i, v, stats) ->
      let i' = extract_leftvalue @@ gen_leftvalue i in
      let v' = eval_loop_value @@ gen_value v in
      let stats' = gen_statements stats (indent + 1) in
      let loop =
        Printf.sprintf "for %s in %s; do" i' v' |> with_indent_lines indent
      in
      loop ^ String.concat stats' ^ with_indent_lines indent "done"
  | While (v, stats) ->
      let v' = eval @@ gen_value v in
      let stats' = gen_statements stats (indent + 1) in
      let while' =
        Printf.sprintf "while [ 0 -eq %s ]; do" v' |> with_indent_lines indent
      in
      while' ^ String.concat stats' ^ with_indent_lines indent "done"
  | Fun_def (name, args, stats, fun_tp) ->
      let def = Printf.sprintf "%s() {" name |> with_indent_lines indent in
      let args' =
        List.mapi (args @ ["__fun_result_var"]) ~f:(fun i e ->
            Printf.sprintf "local %s=$%d" e (i + 1)
            |> with_indent_lines (indent + 1) )
      in
      let stats' = gen_statements stats (indent + 1) in
      def ^ String.concat (args' @ stats') ^ with_indent_lines indent "}"
  | Return v ->
      let v' = eval @@ gen_value v in
      let value_add_backslash v =
        let open String in
        match prefix v 1 with
        | "(" ->
            let t1 = substr_replace_first v "(" "\\(" in
            let t2 =
              substr_replace_first
                ~pos:(length t1 - 1)
                t1 ~pattern:")" ~with_:"\\)"
            in
            t2
        | "\"" ->
            let t1 = substr_replace_first v "\"" "\\\"" in
            let t2 =
              substr_replace_first
                ~pos:(length t1 - 1)
                t1 ~pattern:"\"" ~with_:"\\\""
            in
            t2
        | _ -> v
      in
      "eval $__fun_result_var=" ^ value_add_backslash v'
      |> with_indent_lines indent
  | Value v ->
      let v' = gen_value v in
      match v' with
      | `Fun_call s -> s "__unused__fun_result_var" |> with_indent_lines indent
      | _ -> ""


and gen_statements (v: statements) ~(indent: int) : string list =
  List.fold v ~init:[] ~f:(fun r e -> gen_statement e indent :: r) |> List.rev
