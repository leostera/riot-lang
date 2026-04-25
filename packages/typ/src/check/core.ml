let span_of_node = fun (node: Syn.Ast.Node.t) ->
  let start, end_ = Syn.Ast.Node.raw_range node in Syn.Ceibo.Span.make ~start ~end_

let unsupported_syntax = fun node summary -> Diagnostics.Diagnostic.UnsupportedSyntax { span = span_of_node node; kind = Syn.Ast.Node.kind node; summary }

let unsupported_type = fun node summary -> Diagnostics.Diagnostic.UnsupportedType { span = span_of_node node; summary }

let check_source_file = fun ~typing_context (parse_result: Syn.Parser.parse_result) ->
  let _ = parse_result in { File.empty with typing_context }

let check_expression = fun (_expression: Syn.Ast.Expr.t) -> []

let check_pattern = fun (_pattern: Syn.Ast.Pattern.t) -> []

let check_let_binding = fun (_binding: Syn.Ast.LetBinding.t) -> []

let check_core_type = fun (_type_expr: Syn.Ast.TypeExpr.t) -> []
