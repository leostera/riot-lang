val check: Syn.Ast.Expr.t -> Diagnostics.Diagnostic.t list

val check_parameter: Syn.Ast.Parameter.t -> Diagnostics.Diagnostic.t list

val check_let_binding: Syn.Ast.LetBinding.t -> Diagnostics.Diagnostic.t list
