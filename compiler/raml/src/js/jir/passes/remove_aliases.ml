open Std
module Jir = Types

type env = {
  aliases: (string * string) list;
  assigned: string list;
  exported: string list;
}

let remember_name = fun names name ->
  if List.exists (String.equal name) names then
    names
  else
    name :: names

let rec collect_expr_assigned_names = fun names expr ->
  match expr with
  | Jir.Expr.Literal _
  | Jir.Expr.Identifier _
  | Jir.Expr.Imported _
  | Jir.Expr.Runtime_helper _ ->
      names
  | Jir.Expr.Function function_ ->
      collect_statement_assigned_names names function_.body
  | Jir.Expr.Member member ->
      collect_expr_assigned_names names member.object_
  | Jir.Expr.Call call ->
      let names = collect_expr_assigned_names names call.callee in
      collect_expr_list_assigned_names names call.arguments
  | Jir.Expr.Conditional conditional ->
      let names = collect_expr_assigned_names names conditional.condition in
      let names = collect_expr_assigned_names names conditional.then_ in
      collect_expr_assigned_names names conditional.else_
  | Jir.Expr.Assignment assignment ->
      collect_expr_assigned_names (remember_name names assignment.target) assignment.value

and collect_expr_list_assigned_names = fun names exprs ->
  match exprs with
  | [] -> names
  | expr :: rest ->
      let names = collect_expr_assigned_names names expr in
      collect_expr_list_assigned_names names rest

and collect_statement_assigned_names = fun names statements ->
  match statements with
  | [] -> names
  | statement :: rest ->
      let names = collect_one_statement_assigned_names names statement in
      collect_statement_assigned_names names rest

and collect_one_statement_assigned_names = fun names statement ->
  match statement with
  | Jir.Statement.Declaration declaration ->
      Option.map (collect_expr_assigned_names names) declaration.init |> Option.unwrap_or ~default:names
  | Jir.Statement.Block statements ->
      collect_statement_assigned_names names statements
  | Jir.Statement.Expression expr
  | Jir.Statement.Return expr ->
      collect_expr_assigned_names names expr
  | Jir.Statement.If if_ ->
      let names = collect_expr_assigned_names names if_.condition in
      let names = collect_statement_assigned_names names if_.then_ in
      collect_statement_assigned_names names if_.else_

let collect_program_assigned_names = fun (program: Jir.Program.t) ->
  collect_statement_assigned_names [] program.body

let is_name = fun names name ->
  List.exists (String.equal name) names

let resolve_alias = fun env name ->
  let rec loop seen name =
    if is_name seen name then
      name
    else
      match
        List.find_opt
          (fun (alias, _) ->
            String.equal alias name)
          env.aliases
      with
      | Some (_, target) -> loop (name :: seen) target
      | None -> name
  in
  loop [] name

let bind_alias = fun env alias target -> { env with aliases = (alias, target) :: env.aliases }

let rec lower_expr = fun env expr ->
  match expr with
  | Jir.Expr.Literal _
  | Jir.Expr.Imported _
  | Jir.Expr.Runtime_helper _ -> expr
  | Jir.Expr.Identifier name -> Jir.Expr.Identifier (resolve_alias env name)
  | Jir.Expr.Function function_ -> Jir.Expr.Function Jir.Expr.{
    function_
    with body = lower_scoped_block env function_.body
  }
  | Jir.Expr.Member member -> Jir.Expr.Member Jir.Expr.{
    member
    with object_ = lower_expr env member.object_
  }
  | Jir.Expr.Call call -> Jir.Expr.Call Jir.Expr.{
    callee = lower_expr env call.callee;
    arguments = List.map (lower_expr env) call.arguments
  }
  | Jir.Expr.Conditional conditional -> Jir.Expr.Conditional Jir.Expr.{
    condition = lower_expr env conditional.condition;
    then_ = lower_expr env conditional.then_;
    else_ = lower_expr env conditional.else_
  }
  | Jir.Expr.Assignment assignment -> Jir.Expr.Assignment Jir.Expr.{
    assignment
    with value = lower_expr env assignment.value
  }

and lower_statement = fun env statement ->
  match statement with
  | Jir.Statement.Declaration declaration -> lower_declaration env declaration
  | Jir.Statement.Block statements -> (
    [ Jir.Statement.Block (lower_scoped_block env statements) ],
    env
  )
  | Jir.Statement.Expression expr -> ([ Jir.Statement.Expression (lower_expr env expr) ], env)
  | Jir.Statement.Return expr -> ([ Jir.Statement.Return (lower_expr env expr) ], env)
  | Jir.Statement.If if_ -> (
    [
      Jir.Statement.If Jir.Statement.{
        condition = lower_expr env if_.condition;
        then_ = lower_scoped_block env if_.then_;
        else_ = lower_scoped_block env if_.else_
      }
    ],
    env
  )

and lower_declaration = fun env (declaration: Jir.Declaration.t) ->
  let init = Option.map (lower_expr env) declaration.init in
  match (declaration.kind, init) with
  | (Jir.Declaration.Const, Some (Jir.Expr.Identifier target)) when not
    (String.equal declaration.name target)
  && not (is_name env.exported declaration.name)
  && not (is_name env.assigned target) -> ([], bind_alias env declaration.name target)
  | _ -> ([ Jir.Statement.Declaration Jir.Declaration.{ declaration with init } ], env)

and lower_block = fun env statements ->
  match statements with
  | [] -> ([], env)
  | statement :: rest ->
      let (statement, env) = lower_statement env statement in
      let (rest, env) = lower_block env rest in
      (statement @ rest, env)

and lower_scoped_block = fun env statements -> lower_block env statements |> fst

let program = fun (program: Jir.Program.t) ->
  let env = {
    aliases = [];
    assigned = collect_program_assigned_names program;
    exported = List.map (fun (export: Jir.Export.t) -> export.local) program.exports
  } in
  let (body, _) = lower_block env program.body in
  { program with body }
