open Std
module Jir = Types

type env = {
  bindings: (string * string) list;
  visible: string list;
}

let empty = { bindings = []; visible = [] }

let is_visible = fun env name ->
  List.exists (String.equal name) env.visible

let lookup_name = fun env name ->
  match
    List.find_opt
      (fun (source, _) ->
        String.equal source name)
      env.bindings
  with
  | Some (_, lowered) -> lowered
  | None -> name

let fresh_name = fun env name ->
  if not (is_visible env name) then
    name
  else
    let rec loop index =
      let candidate = format Format.[ str name; str "$"; int index ] in
      if is_visible env candidate then
        loop (index + 1)
      else
        candidate
    in
    loop 1

let bind_name = fun env name ->
  let lowered = fresh_name env name in
  ({ bindings = (name, lowered) :: env.bindings; visible = lowered :: env.visible }, lowered)

let bind_names = fun env names ->
  let (env, lowered_rev) =
    List.fold_left
      (fun (env, lowered_rev) name ->
        let (env, lowered) = bind_name env name in
        (env, lowered :: lowered_rev))
      (env, [])
      names
  in
  (env, List.rev lowered_rev)

let seed_import = fun env import -> { env with visible = Jir.Imports.local import :: env.visible }

let rec lower_expr = fun env expr ->
  match expr with
  | Jir.Expr.Literal _
  | Jir.Expr.Imported _
  | Jir.Expr.Runtime_helper _ ->
      expr
  | Jir.Expr.Identifier name ->
      Jir.Expr.Identifier (lookup_name env name)
  | Jir.Expr.Function function_ ->
      let (env, params) = bind_names env function_.params in
      let body = lower_scoped_block env function_.body in
      Jir.Expr.Function Jir.Expr.{ params; body }
  | Jir.Expr.Member member ->
      Jir.Expr.Member Jir.Expr.{
        object_ = lower_expr env member.object_;
        property = member.property
      }
  | Jir.Expr.Call call ->
      Jir.Expr.Call Jir.Expr.{
        callee = lower_expr env call.callee;
        arguments = List.map (lower_expr env) call.arguments
      }
  | Jir.Expr.Conditional conditional ->
      Jir.Expr.Conditional Jir.Expr.{
        condition = lower_expr env conditional.condition;
        then_ = lower_expr env conditional.then_;
        else_ = lower_expr env conditional.else_
      }
  | Jir.Expr.Assignment assignment ->
      Jir.Expr.Assignment Jir.Expr.{
        target = lookup_name env assignment.target;
        value = lower_expr env assignment.value
      }

and lower_statement = fun env statement ->
  match statement with
  | Jir.Statement.Declaration declaration ->
      let init = Option.map (lower_expr env) declaration.init in
      let (env, name) = bind_name env declaration.name in
      (Jir.Statement.Declaration Jir.Declaration.{ declaration with name; init }, env)
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

and lower_scoped_block = fun env statements -> lower_block env statements |> fst

let lower_export = fun env (export: Jir.Export.t) ->
  Jir.Export.{ export with local = lookup_name env export.local }

let program = fun (program: Jir.Program.t) ->
  let env = List.fold_left seed_import empty program.imports in
  let (body, env) = lower_block env program.body in
  let exports = List.map (lower_export env) program.exports in
  { program with body; exports }
