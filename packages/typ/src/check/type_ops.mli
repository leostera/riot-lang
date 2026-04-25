val unsupported_syntax: Syn.Ast.Node.t -> string -> Diagnostics.Diagnostic.t

val unsupported_type: Syn.Ast.Node.t -> string -> Diagnostics.Diagnostic.t

val check_core_type: Syn.Ast.TypeExpr.t -> Diagnostics.Diagnostic.t list

val check_type_definition: Syn.Ast.Node.t -> Diagnostics.Diagnostic.t list

val check_type_declaration: Syn.Ast.TypeDeclaration.t -> Diagnostics.Diagnostic.t list

val check_value_declaration: Syn.Ast.ValueDeclaration.t -> Diagnostics.Diagnostic.t list

val check_external_declaration: Syn.Ast.ExternalDeclaration.t -> Diagnostics.Diagnostic.t list

val check_exception_rhs: Syn.Ast.Node.t -> Diagnostics.Diagnostic.t list
