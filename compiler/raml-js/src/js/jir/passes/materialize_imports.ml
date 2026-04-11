open Std
module Jir = Types

let rec lower_expr = fun expr ->
  match expr with
  | Jir.Expr.Literal _
  | Jir.Expr.Identifier _ -> expr
  | Jir.Expr.Imported requirement -> Jir.Expr.Identifier (Jir.Binder.entity_id (Jir.Imports.local requirement))
  | Jir.Expr.Runtime_helper helper -> Jir.Expr.Identifier (Jir.Binder.entity_id helper.local)
  | Jir.Expr.Function function_ -> Jir.Expr.Function Jir.Expr.{
    params = function_.params;
    body = List.map lower_statement function_.body;
  }
  | Jir.Expr.Member member -> Jir.Expr.Member Jir.Expr.{
    object_ = lower_expr member.object_;
    property = member.property;
  }
  | Jir.Expr.Call call -> Jir.Expr.Call Jir.Expr.{
    callee = lower_expr call.callee;
    arguments = List.map lower_expr call.arguments;
  }
  | Jir.Expr.Conditional conditional -> Jir.Expr.Conditional Jir.Expr.{
    condition = lower_expr conditional.condition;
    then_ = lower_expr conditional.then_;
    else_ = lower_expr conditional.else_;
  }
  | Jir.Expr.Assignment assignment -> Jir.Expr.Assignment Jir.Expr.{
    target = assignment.target;
    value = lower_expr assignment.value;
  }

and lower_statement = fun statement ->
  match statement with
  | Jir.Statement.Declaration declaration -> Jir.Statement.Declaration Jir.Declaration.{
    declaration
    with init = Option.map lower_expr declaration.init;
  }
  | Jir.Statement.Block statements -> Jir.Statement.Block (List.map lower_statement statements)
  | Jir.Statement.Expression expr -> Jir.Statement.Expression (lower_expr expr)
  | Jir.Statement.Return expr -> Jir.Statement.Return (lower_expr expr)
  | Jir.Statement.If if_ -> Jir.Statement.If Jir.Statement.{
    condition = lower_expr if_.condition;
    then_ = List.map lower_statement if_.then_;
    else_ = List.map lower_statement if_.else_;
  }

let program = fun (program: Jir.Program.t) ->
  { program with body = List.map lower_statement program.body }
