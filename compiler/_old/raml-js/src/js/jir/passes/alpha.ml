open Std
module Core = Raml_core.Core_ir
module Jir = Types
module Syntax = Syntax

module Binding_map = Collections.Map.Make (struct
  type t = Core.Binding_id.t

  let compare = Core.Binding_id.compare
end)

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

type env = {
  bindings: string Binding_map.t;
  visible: String_set.t;
}

let empty = { bindings = Binding_map.empty; visible = String_set.empty }

let is_visible = fun env name ->
  String_set.mem name env.visible

let lookup_binding_name = fun env binding_id ->
  Binding_map.get env.bindings ~key:binding_id
  |> Option.unwrap_or ~default:(Core.Binding_id.name binding_id)

let fresh_name = fun env name ->
  let base = Syntax.sanitize_binding_identifier name in
  if not (is_visible env base) then
    base
  else
    let rec loop index =
      let candidate = format Format.[ str base; str "$"; int index ] in
      if is_visible env candidate then
        loop (index + 1)
      else
        candidate
    in
    loop 1

let bind_binder = fun env (binder: Jir.Binder.t) ->
  let lowered = fresh_name env binder.name in
  let binder = Jir.Binder.rename binder lowered in
  (
    {
      bindings = Binding_map.insert env.bindings ~key:binder.binding_id ~value:lowered;
      visible = String_set.add lowered env.visible
    },
    binder
  )

let bind_binders = fun env binders ->
  let (env, lowered_rev) =
    List.fold_left binders ~init:(env, [])
      ~fn:(fun (env, lowered_rev) binder ->
        let (env, binder) = bind_binder env binder in
        (env, binder :: lowered_rev))
  in
  (env, List.rev lowered_rev)

let rename_bound_binder = fun env (binder: Jir.Binder.t) ->
  Jir.Binder.rename binder (lookup_binding_name env binder.binding_id)

let lower_import = fun env (import: Jir.Imports.requirement) ->
  let (env, local) = bind_binder env import.local in
  ({ import with local }, env)

let rec lower_array_element = fun env element ->
  match element with
  | Jir.Expr.Item expr -> Jir.Expr.Item (lower_expr env expr)
  | Jir.Expr.Spread expr -> Jir.Expr.Spread (lower_expr env expr)

and lower_object_field = fun env (field: Jir.Expr.object_field) ->
  Jir.Expr.{ field with value = lower_expr env field.value }

and lower_expr = fun env expr ->
  match expr with
  | Jir.Expr.Literal _
  | Jir.Expr.Global _
  | Jir.Expr.Identifier _ ->
      expr
  | Jir.Expr.Imported requirement ->
      Jir.Expr.Imported { requirement with local = rename_bound_binder env requirement.local }
  | Jir.Expr.Runtime_helper helper ->
      Jir.Expr.Runtime_helper { helper with local = rename_bound_binder env helper.local }
  | Jir.Expr.Unary unary ->
      Jir.Expr.Unary Jir.Expr.{ unary with operand = lower_expr env unary.operand }
  | Jir.Expr.Binary binary ->
      Jir.Expr.Binary Jir.Expr.{
        binary
        with left = lower_expr env binary.left;
        right = lower_expr env binary.right
      }
  | Jir.Expr.Array elements ->
      Jir.Expr.Array (List.map elements ~fn:(lower_array_element env))
  | Jir.Expr.Object fields ->
      Jir.Expr.Object (List.map fields ~fn:(lower_object_field env))
  | Jir.Expr.Function function_ ->
      let (env, params) = bind_binders env function_.params in
      let body = lower_scoped_block env function_.body in
      Jir.Expr.Function Jir.Expr.{ params; body }
  | Jir.Expr.Member member ->
      Jir.Expr.Member Jir.Expr.{
        object_ = lower_expr env member.object_;
        property = member.property
      }
  | Jir.Expr.Index index ->
      Jir.Expr.Index Jir.Expr.{
        object_ = lower_expr env index.object_;
        index = lower_expr env index.index
      }
  | Jir.Expr.Call call ->
      Jir.Expr.Call Jir.Expr.{
        callee = lower_expr env call.callee;
        arguments = List.map call.arguments ~fn:(lower_expr env)
      }
  | Jir.Expr.Conditional conditional ->
      Jir.Expr.Conditional Jir.Expr.{
        condition = lower_expr env conditional.condition;
        then_ = lower_expr env conditional.then_;
        else_ = lower_expr env conditional.else_
      }
  | Jir.Expr.Assignment assignment ->
      Jir.Expr.Assignment Jir.Expr.{
        target = assignment.target;
        value = lower_expr env assignment.value
      }

and lower_statement = fun env statement ->
  match statement with
  | Jir.Statement.Declaration declaration ->
      let init = Option.map declaration.init ~fn:(lower_expr env) in
      let (env, binder) = bind_binder env declaration.binder in
      (Jir.Statement.Declaration Jir.Declaration.{ declaration with binder; init }, env)
  | Jir.Statement.Block statements ->
      (Jir.Statement.Block (lower_scoped_block env statements), env)
  | Jir.Statement.Expression expr ->
      (Jir.Statement.Expression (lower_expr env expr), env)
  | Jir.Statement.Return expr ->
      (Jir.Statement.Return (lower_expr env expr), env)
  | Jir.Statement.If if_ ->
      let condition = lower_expr env if_.condition in
      let then_ = lower_scoped_block env if_.then_ in
      let else_ = lower_scoped_block env if_.else_ in
      (Jir.Statement.If Jir.Statement.{ condition; then_; else_ }, env)

and lower_block = fun env statements ->
  match statements with
  | [] -> ([], env)
  | statement :: rest ->
      let (statement, env) = lower_statement env statement in
      let (rest, env) = lower_block env rest in
      (statement :: rest, env)

and lower_scoped_block = fun env statements ->
  let body, _env = lower_block env statements in
  body

let program = fun ~context:_ (program: Jir.Program.t) ->
  let (imports, env) =
    List.fold_left program.imports ~init:([], empty)
      ~fn:(fun (imports_rev, env) import ->
        let (import, env) = lower_import env import in
        (import :: imports_rev, env))
  in
  let (body, _env) = lower_block env program.body in
  { program with imports = List.rev imports; body }
