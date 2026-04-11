open Std
module Core = RamlCore.CoreIR
module Jir = Types

module Binding_map = Map.Make (struct
  type t = Core.Binding_id.t
  let compare = Core.Binding_id.compare
end)

module String_set = Set.Make (struct
  type t = string
  let compare = String.compare
end)

type env = {
  bindings: string Binding_map.t;
  visible: String_set.t;
}

let empty = { bindings = Binding_map.empty; visible = String_set.empty }

let is_visible = fun env name ->
  String_set.mem name env.visible

let lookup_binding_name = fun env binding_id ->
  Binding_map.find_opt binding_id env.bindings |> Option.unwrap_or ~default:(Core.Binding_id.name binding_id)

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

let bind_binder = fun env (binder: Jir.Binder.t) ->
  let lowered = fresh_name env binder.name in
  let binder = Jir.Binder.rename binder lowered in
  ({
    bindings = Binding_map.add binder.binding_id lowered env.bindings;
    visible = String_set.add lowered env.visible;
  }, binder)

let bind_binders = fun env binders ->
  let (env, lowered_rev) =
    List.fold_left
      (fun (env, lowered_rev) binder ->
        let (env, binder) = bind_binder env binder in
        (env, binder :: lowered_rev))
      (env, [])
      binders
  in
  (env, List.rev lowered_rev)

let rename_bound_binder = fun env (binder: Jir.Binder.t) ->
  Jir.Binder.rename binder (lookup_binding_name env binder.binding_id)

let lower_import = fun env (import: Jir.Imports.requirement) ->
  let (env, local) = bind_binder env import.local in
  ({ import with local }, env)

let rec lower_expr = fun env expr ->
  match expr with
  | Jir.Expr.Literal _
  | Jir.Expr.Identifier _ ->
      expr
  | Jir.Expr.Imported requirement ->
      Jir.Expr.Imported { requirement with local = rename_bound_binder env requirement.local }
  | Jir.Expr.Runtime_helper helper ->
      Jir.Expr.Runtime_helper { helper with local = rename_bound_binder env helper.local }
  | Jir.Expr.Function function_ ->
      let (env, params) = bind_binders env function_.params in
      let body = lower_scoped_block env function_.body in
      Jir.Expr.Function Jir.Expr.{ params; body }
  | Jir.Expr.Member member ->
      Jir.Expr.Member Jir.Expr.{
        object_ = lower_expr env member.object_;
        property = member.property;
      }
  | Jir.Expr.Call call ->
      Jir.Expr.Call Jir.Expr.{
        callee = lower_expr env call.callee;
        arguments = List.map (lower_expr env) call.arguments;
      }
  | Jir.Expr.Conditional conditional ->
      Jir.Expr.Conditional Jir.Expr.{
        condition = lower_expr env conditional.condition;
        then_ = lower_expr env conditional.then_;
        else_ = lower_expr env conditional.else_;
      }
  | Jir.Expr.Assignment assignment ->
      Jir.Expr.Assignment Jir.Expr.{
        target = assignment.target;
        value = lower_expr env assignment.value;
      }

and lower_statement = fun env statement ->
  match statement with
  | Jir.Statement.Declaration declaration ->
      let init = Option.map (lower_expr env) declaration.init in
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

and lower_scoped_block = fun env statements -> lower_block env statements |> fst

let program = fun (program: Jir.Program.t) ->
  let (imports, env) =
    List.fold_left
      (fun (imports_rev, env) import ->
        let (import, env) = lower_import env import in
        (import :: imports_rev, env))
      ([], empty)
      program.imports
  in
  let (body, _env) = lower_block env program.body in
  { program with imports = List.rev imports; body }
