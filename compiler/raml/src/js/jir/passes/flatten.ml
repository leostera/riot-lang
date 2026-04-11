open Std
module Jir = Types

let ( let* ) = Option.and_then

type state = {
  used_names: string list;
}

let remember_name = fun state name ->
  if List.exists (String.equal name) state.used_names then
    state
  else
    { used_names = name :: state.used_names }

let rec collect_expr_names = fun state expr ->
  match expr with
  | Jir.Expr.Literal _ ->
      state
  | Jir.Expr.Identifier name ->
      remember_name state name
  | Jir.Expr.Imported requirement ->
      remember_name state (Jir.Imports.local requirement)
  | Jir.Expr.Runtime_helper helper ->
      remember_name state helper.local
  | Jir.Expr.Function function_ ->
      let state = List.fold_left remember_name state function_.params in
      collect_statement_names state function_.body
  | Jir.Expr.Member member ->
      collect_expr_names state member.object_
  | Jir.Expr.Call call ->
      let state = collect_expr_names state call.callee in
      collect_expr_name_list state call.arguments
  | Jir.Expr.Conditional conditional ->
      let state = collect_expr_names state conditional.condition in
      let state = collect_expr_names state conditional.then_ in
      collect_expr_names state conditional.else_
  | Jir.Expr.Assignment assignment ->
      let state = remember_name state assignment.target in
      collect_expr_names state assignment.value

and collect_expr_name_list = fun state exprs ->
  match exprs with
  | [] -> state
  | expr :: rest ->
      let state = collect_expr_names state expr in
      collect_expr_name_list state rest

and collect_statement_names = fun state statements ->
  match statements with
  | [] -> state
  | statement :: rest ->
      let state = collect_statement_name state statement in
      collect_statement_names state rest

and collect_statement_name = fun state statement ->
  match statement with
  | Jir.Statement.Declaration declaration ->
      let state = remember_name state declaration.name in
      Option.map (collect_expr_names state) declaration.init |> Option.unwrap_or ~default:state
  | Jir.Statement.Block statements ->
      collect_statement_names state statements
  | Jir.Statement.Expression expr ->
      collect_expr_names state expr
  | Jir.Statement.Return expr ->
      collect_expr_names state expr
  | Jir.Statement.If if_ ->
      let state = collect_expr_names state if_.condition in
      let state = collect_statement_names state if_.then_ in
      collect_statement_names state if_.else_

let collect_program_names = fun (program: Jir.Program.t) ->
  let state =
    List.fold_left
      (fun state import -> remember_name state (Jir.Imports.local import))
      { used_names = [] }
      program.imports
  in
  let state = collect_statement_names state program.body in
  List.fold_left
    (fun state (export: Jir.Export.t) -> remember_name state export.local)
    state
    program.exports

let fresh_name = fun state base ->
  let rec loop index =
    let candidate =
      if index = 0 then
        base
      else
        format Format.[ str base; str "$"; int index ]
    in
    if List.exists (String.equal candidate) state.used_names then
      loop (index + 1)
    else
      ({ used_names = candidate :: state.used_names }, candidate)
  in
  loop 0

let rec lower_expr_list = fun state exprs ->
  List.fold_left
    (fun (reversed, state) expr ->
      let (expr, state) = lower_expr state expr in
      (expr :: reversed, state))
    ([], state)
    exprs |> fun (reversed, state) -> (List.rev reversed, state)

and lower_expr = fun state expr ->
  match expr with
  | Jir.Expr.Literal _
  | Jir.Expr.Identifier _
  | Jir.Expr.Imported _
  | Jir.Expr.Runtime_helper _ ->
      (expr, state)
  | Jir.Expr.Function function_ ->
      let (body, state) = lower_block state function_.body in
      (Jir.Expr.Function Jir.Expr.{ params = function_.params; body }, state)
  | Jir.Expr.Member member ->
      let (object_, state) = lower_expr state member.object_ in
      (Jir.Expr.Member Jir.Expr.{ object_; property = member.property }, state)
  | Jir.Expr.Call call ->
      let (callee, state) = lower_expr state call.callee in
      let (arguments, state) = lower_expr_list state call.arguments in
      (Jir.Expr.Call Jir.Expr.{ callee; arguments }, state)
  | Jir.Expr.Conditional conditional ->
      let (condition, state) = lower_expr state conditional.condition in
      let (then_, state) = lower_expr state conditional.then_ in
      let (else_, state) = lower_expr state conditional.else_ in
      (Jir.Expr.Conditional Jir.Expr.{ condition; then_; else_ }, state)
  | Jir.Expr.Assignment assignment ->
      let (value, state) = lower_expr state assignment.value in
      (Jir.Expr.Assignment Jir.Expr.{ target = assignment.target; value }, state)

and lower_optional_expr = fun state expr ->
  match expr with
  | None -> (None, state)
  | Some expr ->
      let (expr, state) = lower_expr state expr in
      (Some expr, state)

and lower_declaration = fun state (declaration: Jir.Declaration.t) ->
  let (init, state) = lower_optional_expr state declaration.init in
  (Jir.Declaration.{ declaration with init }, state)

and lower_block = fun state statements ->
  match statements with
  | [] -> ([], state)
  | statement :: rest ->
      let (statement, state) = lower_statement state statement in
      let (rest, state) = lower_block state rest in
      (statement @ rest, state)

and lower_statement = fun state statement ->
  match statement with
  | Jir.Statement.Declaration declaration ->
      lower_declaration_statement state declaration
  | Jir.Statement.Block statements ->
      let (statements, state) = lower_block state statements in
      ([ Jir.Statement.Block statements ], state)
  | Jir.Statement.Expression expr ->
      lower_effect_expr state expr
  | Jir.Statement.Return expr ->
      let (expr, state) = lower_expr state expr in
      ([ Jir.Statement.Return expr ], state)
  | Jir.Statement.If if_ ->
      let (condition, state) = lower_expr state if_.condition in
      let (then_, state) = lower_block state if_.then_ in
      let (else_, state) = lower_block state if_.else_ in
      ([ Jir.Statement.If Jir.Statement.{ condition; then_; else_ } ], state)

and lower_declaration_statement = fun state (declaration: Jir.Declaration.t) ->
  match declaration.init with
  | Some (Jir.Expr.Call { callee=Jir.Expr.Function function_; arguments=[] }) -> (
      let base = format Format.[ str "__raml_init_"; str declaration.name ] in
      let (trial_state, target) = fresh_name state base in
      match lower_initializer_function_body trial_state ~target function_.body with
      | Some (body, state) -> (
        [
          Jir.Statement.Declaration Jir.Declaration.{
            kind = Jir.Declaration.Let;
            name = target;
            init = None
          };
          Jir.Statement.Block body;
          Jir.Statement.Declaration Jir.Declaration.{
            declaration
            with init = Some (Jir.Expr.Identifier target)
          };
        ],
        state
      )
      | None ->
          let (declaration, state) = lower_declaration state declaration in
          ([ Jir.Statement.Declaration declaration ], state)
    )
  | _ ->
      let (declaration, state) = lower_declaration state declaration in
      ([ Jir.Statement.Declaration declaration ], state)

and lower_effect_expr = fun state expr ->
  match expr with
  | Jir.Expr.Call { callee=Jir.Expr.Function function_; arguments=[] } -> (
      match lower_effect_function_body state function_.body with
      | Some (statements, state) -> (statements, state)
      | None ->
          let (expr, state) = lower_expr state expr in
          ([ Jir.Statement.Expression expr ], state)
    )
  | _ ->
      let (expr, state) = lower_expr state expr in
      ([ Jir.Statement.Expression expr ], state)

and lower_effect_function_body = fun state statements ->
  match statements with
  | [] ->
      Some ([], state)
  | [ statement ] ->
      lower_effect_tail_statement state statement
  | statement :: rest ->
      let* (head, state) = lower_non_tail_statement state statement in
      let* (tail, state) = lower_effect_function_body state rest in
      Some (head @ tail, state)

and lower_initializer_function_body = fun state ~target statements ->
  match statements with
  | [] ->
      None
  | [ statement ] ->
      lower_initializer_tail_statement state ~target statement
  | statement :: rest ->
      let* (head, state) = lower_non_tail_statement state statement in
      let* (tail, state) = lower_initializer_function_body state ~target rest in
      Some (head @ tail, state)

and lower_non_tail_statement = fun state statement ->
  match statement with
  | Jir.Statement.Declaration declaration ->
      let (statements, state) = lower_declaration_statement state declaration in
      Some (statements, state)
  | Jir.Statement.Block statements ->
      let (statements, state) = lower_block state statements in
      Some ([ Jir.Statement.Block statements ], state)
  | Jir.Statement.Expression expr ->
      let (statements, state) = lower_effect_expr state expr in
      Some (statements, state)
  | Jir.Statement.If if_ ->
      let (condition, state) = lower_expr state if_.condition in
      let* (then_, state) = lower_non_tail_block state if_.then_ in
      let* (else_, state) = lower_non_tail_block state if_.else_ in
      Some ([ Jir.Statement.If Jir.Statement.{ condition; then_; else_ } ], state)
  | Jir.Statement.Return _ ->
      None

and lower_non_tail_block = fun state statements ->
  match statements with
  | [] -> Some ([], state)
  | statement :: rest ->
      let* (head, state) = lower_non_tail_statement state statement in
      let* (tail, state) = lower_non_tail_block state rest in
      Some (head @ tail, state)

and lower_effect_tail_statement = fun state statement ->
  match statement with
  | Jir.Statement.Return expr ->
      let (statements, state) = lower_effect_expr state expr in
      Some (statements, state)
  | Jir.Statement.If if_ ->
      let (condition, state) = lower_expr state if_.condition in
      let* (then_, state) = lower_effect_function_body state if_.then_ in
      let* (else_, state) = lower_effect_function_body state if_.else_ in
      Some ([ Jir.Statement.If Jir.Statement.{ condition; then_; else_ } ], state)
  | Jir.Statement.Declaration declaration ->
      let (statements, state) = lower_declaration_statement state declaration in
      Some (statements, state)
  | Jir.Statement.Block statements ->
      let (statements, state) = lower_block state statements in
      Some ([ Jir.Statement.Block statements ], state)
  | Jir.Statement.Expression expr ->
      let (statements, state) = lower_effect_expr state expr in
      Some (statements, state)

and lower_initializer_tail_statement = fun state ~target statement ->
  match statement with
  | Jir.Statement.Return expr ->
      let (value, state) = lower_expr state expr in
      Some ([ Jir.Statement.Expression (Jir.Expr.Assignment Jir.Expr.{ target; value }) ], state)
  | Jir.Statement.If if_ ->
      let (condition, state) = lower_expr state if_.condition in
      let* (then_, state) = lower_initializer_function_body state ~target if_.then_ in
      let* (else_, state) = lower_initializer_function_body state ~target if_.else_ in
      Some ([ Jir.Statement.If Jir.Statement.{ condition; then_; else_ } ], state)
  | Jir.Statement.Declaration _
  | Jir.Statement.Block _
  | Jir.Statement.Expression _ ->
      None

let program = fun (program: Jir.Program.t) ->
  let state = collect_program_names program in
  let (body, _) = lower_block state program.body in
  { program with body }
