open Std
module Jir = Types

let rec lower_expr = fun expr ->
  match expr with
  | Jir.Expr.Literal _
  | Jir.Expr.Global _
  | Jir.Expr.Identifier _ -> expr
  | Jir.Expr.Imported requirement -> Jir.Expr.Identifier (Jir.Binder.entity_id
    (Jir.Imports.local requirement))
  | Jir.Expr.Runtime_helper helper -> Jir.Expr.Identifier (Jir.Binder.entity_id helper.local)
  | Jir.Expr.Unary unary -> Jir.Expr.Unary Jir.Expr.{ unary with operand = lower_expr unary.operand }
  | Jir.Expr.Binary binary -> Jir.Expr.Binary Jir.Expr.{
    binary
    with left = lower_expr binary.left;
    right = lower_expr binary.right
  }
  | Jir.Expr.Array elements -> Jir.Expr.Array (List.map elements ~fn:lower_array_element)
  | Jir.Expr.Object fields -> Jir.Expr.Object (List.map fields ~fn:lower_object_field)
  | Jir.Expr.Function function_ -> Jir.Expr.Function Jir.Expr.{
    params = function_.params;
    body = List.map function_.body ~fn:lower_statement
  }
  | Jir.Expr.Member member -> Jir.Expr.Member Jir.Expr.{
    object_ = lower_expr member.object_;
    property = member.property
  }
  | Jir.Expr.Index index -> Jir.Expr.Index Jir.Expr.{
    object_ = lower_expr index.object_;
    index = lower_expr index.index
  }
  | Jir.Expr.Call call -> Jir.Expr.Call Jir.Expr.{
    callee = lower_expr call.callee;
    arguments = List.map call.arguments ~fn:lower_expr
  }
  | Jir.Expr.Conditional conditional -> Jir.Expr.Conditional Jir.Expr.{
    condition = lower_expr conditional.condition;
    then_ = lower_expr conditional.then_;
    else_ = lower_expr conditional.else_
  }
  | Jir.Expr.Assignment assignment -> Jir.Expr.Assignment Jir.Expr.{
    target = assignment.target;
    value = lower_expr assignment.value
  }

and lower_array_element = fun element ->
  match element with
  | Jir.Expr.Item expr -> Jir.Expr.Item (lower_expr expr)
  | Jir.Expr.Spread expr -> Jir.Expr.Spread (lower_expr expr)

and lower_object_field = fun (field: Jir.Expr.object_field) ->
  Jir.Expr.{ field with value = lower_expr field.value }

and lower_statement = fun statement ->
  match statement with
  | Jir.Statement.Declaration declaration -> Jir.Statement.Declaration Jir.Declaration.{
    declaration
    with init = Option.map declaration.init ~fn:lower_expr
  }
  | Jir.Statement.Block statements -> Jir.Statement.Block (List.map statements ~fn:lower_statement)
  | Jir.Statement.Expression expr -> Jir.Statement.Expression (lower_expr expr)
  | Jir.Statement.Return expr -> Jir.Statement.Return (lower_expr expr)
  | Jir.Statement.If if_ -> Jir.Statement.If Jir.Statement.{
    condition = lower_expr if_.condition;
    then_ = List.map if_.then_ ~fn:lower_statement;
    else_ = List.map if_.else_ ~fn:lower_statement
  }

let program = fun ~context:_ (program: Jir.Program.t) ->
  { program with body = List.map program.body ~fn:lower_statement }
