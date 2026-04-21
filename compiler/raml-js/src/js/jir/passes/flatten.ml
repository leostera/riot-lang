open Std
module Core = Raml_core.Core_ir
module Jir = Types

let ( let* ) value fn = Option.and_then value ~fn

module String_set = struct
  module Storage = Collections.Map.Make (struct
    type t = string

    let compare = String.compare
  end)

  type t = unit Storage.t

  let empty = Storage.empty

  let add = fun name set -> Storage.insert set ~key:name ~value:()

  let mem = fun name set -> Storage.has_key set ~key:name
end

type state = {
  used_names: String_set.t;
}

let remember_name = fun state name -> { used_names = String_set.add name state.used_names }

let remember_binder = fun state (binder: Jir.Binder.t) -> remember_name state binder.name

let remember_visible_entity = fun state entity ->
  match Core.Entity_id.binding_id entity with
  | Some _ -> state
  | None ->
      match Core.Entity_id.bare_name entity with
      | Some name -> remember_name state name
      | None -> state

let rec collect_array_element_names = fun state element ->
  match element with
  | Jir.Expr.Item expr
  | Jir.Expr.Spread expr -> collect_expr_names state expr

and collect_object_field_names = fun state (field: Jir.Expr.object_field) ->
  collect_expr_names state field.value

and collect_expr_names = fun state expr ->
  match expr with
  | Jir.Expr.Literal _ ->
      state
  | Jir.Expr.Global _ ->
      state
  | Jir.Expr.Identifier entity ->
      remember_visible_entity state entity
  | Jir.Expr.Imported requirement ->
      remember_binder state (Jir.Imports.local requirement)
  | Jir.Expr.Runtime_helper helper ->
      remember_binder state helper.local
  | Jir.Expr.Unary unary ->
      collect_expr_names state unary.operand
  | Jir.Expr.Binary binary ->
      let state = collect_expr_names state binary.left in
      collect_expr_names state binary.right
  | Jir.Expr.Array elements ->
      List.fold_left elements ~init:state ~fn:collect_array_element_names
  | Jir.Expr.Object fields ->
      List.fold_left fields ~init:state ~fn:collect_object_field_names
  | Jir.Expr.Function function_ ->
      let state = List.fold_left function_.params ~init:state ~fn:remember_binder in
      collect_statement_names state function_.body
  | Jir.Expr.Member member ->
      collect_expr_names state member.object_
  | Jir.Expr.Index index ->
      let state = collect_expr_names state index.object_ in
      collect_expr_names state index.index
  | Jir.Expr.Call call ->
      let state = collect_expr_names state call.callee in
      collect_expr_name_list state call.arguments
  | Jir.Expr.Conditional conditional ->
      let state = collect_expr_names state conditional.condition in
      let state = collect_expr_names state conditional.then_ in
      collect_expr_names state conditional.else_
  | Jir.Expr.Assignment assignment ->
      let state = remember_visible_entity state assignment.target in
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
      let state = remember_binder state declaration.binder in
      Option.map declaration.init ~fn:(collect_expr_names state) |> Option.unwrap_or ~default:state
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
      program.imports
      ~init:{ used_names = String_set.empty }
      ~fn:(fun state import -> remember_binder state (Jir.Imports.local import))
  in
  let state = collect_statement_names state program.body in
  List.fold_left
    program.exports
    ~init:state
    ~fn:(fun state (export: Jir.Export.t) -> remember_visible_entity state export.local)

let fresh_name = fun state base ->
  let rec loop index =
    let candidate =
      if index = 0 then
        base
      else
        format Format.[ str base; str "$"; int index ]
    in
    if String_set.mem candidate state.used_names then
      loop (index + 1)
    else
      ({ used_names = String_set.add candidate state.used_names }, candidate)
  in
  loop 0

let generated_binder = fun name -> Jir.Binder.generated ~namespace:[ "flatten" ] ~name

let rec lower_expr_list = fun state exprs ->
  List.fold_left exprs ~init:([], state)
    ~fn:(fun (reversed, state) expr ->
      let (expr, state) = lower_expr state expr in
      (expr :: reversed, state)) |> fun (reversed, state) -> (List.rev reversed, state)

and lower_array_element = fun state element ->
  match element with
  | Jir.Expr.Item expr ->
      let (expr, state) = lower_expr state expr in
      (Jir.Expr.Item expr, state)
  | Jir.Expr.Spread expr ->
      let (expr, state) = lower_expr state expr in
      (Jir.Expr.Spread expr, state)

and lower_array_elements = fun state elements ->
  List.fold_left elements ~init:([], state)
    ~fn:(fun (reversed, state) element ->
      let (element, state) = lower_array_element state element in
      (element :: reversed, state)) |> fun (reversed, state) -> (List.rev reversed, state)

and lower_object_field = fun state (field: Jir.Expr.object_field) ->
  let (value, state) = lower_expr state field.value in
  (Jir.Expr.{ field with value }, state)

and lower_object_fields = fun state fields ->
  List.fold_left fields ~init:([], state)
    ~fn:(fun (reversed, state) field ->
      let (field, state) = lower_object_field state field in
      (field :: reversed, state)) |> fun (reversed, state) -> (List.rev reversed, state)

and lower_expr = fun state expr ->
  match expr with
  | Jir.Expr.Literal _
  | Jir.Expr.Global _
  | Jir.Expr.Identifier _
  | Jir.Expr.Imported _
  | Jir.Expr.Runtime_helper _ ->
      (expr, state)
  | Jir.Expr.Unary unary ->
      let (operand, state) = lower_expr state unary.operand in
      (Jir.Expr.Unary Jir.Expr.{ unary with operand }, state)
  | Jir.Expr.Binary binary ->
      let (left, state) = lower_expr state binary.left in
      let (right, state) = lower_expr state binary.right in
      (Jir.Expr.Binary Jir.Expr.{ binary with left; right }, state)
  | Jir.Expr.Array elements ->
      let (elements, state) = lower_array_elements state elements in
      (Jir.Expr.Array elements, state)
  | Jir.Expr.Object fields ->
      let (fields, state) = lower_object_fields state fields in
      (Jir.Expr.Object fields, state)
  | Jir.Expr.Function function_ ->
      let (body, state) = lower_block state function_.body in
      (Jir.Expr.Function Jir.Expr.{ params = function_.params; body }, state)
  | Jir.Expr.Member member ->
      let (object_, state) = lower_expr state member.object_ in
      (Jir.Expr.Member Jir.Expr.{ object_; property = member.property }, state)
  | Jir.Expr.Index index ->
      let (object_, state) = lower_expr state index.object_ in
      let (index, state) = lower_expr state index.index in
      (Jir.Expr.Index Jir.Expr.{ object_; index }, state)
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
      let base = format Format.[ str "__raml_init_"; str declaration.binder.name ] in
      let (trial_state, target_name) = fresh_name state base in
      let target_binder = generated_binder target_name in
      match lower_initializer_function_body trial_state ~target:target_binder function_.body with
      | Some (body, state) -> (
        [
          Jir.Statement.Declaration Jir.Declaration.{
            kind = Jir.Declaration.Let;
            binder = target_binder;
            init = None
          };
          Jir.Statement.Block body;
          Jir.Statement.Declaration Jir.Declaration.{
            declaration
            with init = Some (Jir.Expr.Identifier (Jir.Binder.entity_id target_binder))
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
      Some (
        [
          Jir.Statement.Expression (Jir.Expr.Assignment Jir.Expr.{
            target = Jir.Binder.entity_id target;
            value
          })
        ],
        state
      )
  | Jir.Statement.If if_ ->
      let (condition, state) = lower_expr state if_.condition in
      let* (then_, state) = lower_initializer_function_body state ~target if_.then_ in
      let* (else_, state) = lower_initializer_function_body state ~target if_.else_ in
      Some ([ Jir.Statement.If Jir.Statement.{ condition; then_; else_ } ], state)
  | Jir.Statement.Declaration _
  | Jir.Statement.Block _
  | Jir.Statement.Expression _ ->
      None

let program = fun ~context:_ (program: Jir.Program.t) ->
  let state = collect_program_names program in
  let (body, _) = lower_block state program.body in
  { program with body }
