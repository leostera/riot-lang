open Std
module Jir = Types

let global = fun name -> Jir.Expr.Global Jir.Expr.{ name }

let member = fun object_ property -> Jir.Expr.Member Jir.Expr.{ object_; property }

let index = fun object_ index -> Jir.Expr.Index Jir.Expr.{ object_; index }

let call = fun callee arguments -> Jir.Expr.Call Jir.Expr.{ callee; arguments }

let unary = fun operator operand -> Jir.Expr.Unary Jir.Expr.{ operator; operand }

let binary = fun operator left right -> Jir.Expr.Binary Jir.Expr.{ operator; left; right }

let array = fun elements -> Jir.Expr.Array (List.map elements ~fn:(fun expr -> Jir.Expr.Item expr))

let string_constructor = fun value -> call (global "String") [ value ]

let console_log = fun arguments -> call (member (global "console") "log") arguments

let console_error = fun arguments -> call (member (global "console") "error") arguments

let stdout_write = fun value ->
  call (member (member (global "process") "stdout") "write") [ string_constructor value ]

let stderr_write = fun value ->
  call (member (member (global "process") "stderr") "write") [ string_constructor value ]

let math_sqrt = fun value -> call (member (global "Math") "sqrt") [ value ]
