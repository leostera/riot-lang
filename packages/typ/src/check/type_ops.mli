val unsupported_syntax: Syn.Cst.syntax_node -> string -> Diagnostics.Diagnostic.t

val unsupported_type: Syn.Cst.syntax_node -> string -> Diagnostics.Diagnostic.t

val check_core_type: Syn.Cst.core_type -> Diagnostics.Diagnostic.t list

val check_type_definition: Syn.Cst.TypeDefinition.t -> Diagnostics.Diagnostic.t list

val check_type_declaration: Syn.Cst.TypeDeclaration.t -> Diagnostics.Diagnostic.t list

val check_value_declaration: Syn.Cst.value_declaration -> Diagnostics.Diagnostic.t list

val check_external_declaration: Syn.Cst.external_declaration -> Diagnostics.Diagnostic.t list

val check_exception_rhs: Syn.Cst.exception_rhs -> Diagnostics.Diagnostic.t list
