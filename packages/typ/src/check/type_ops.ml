let unsupported_syntax = Core.unsupported_syntax

let unsupported_type = Core.unsupported_type

let check_core_type = Core.check_core_type

let check_type_definition = fun (_node: Syn.Ast.Node.t) -> []

let check_type_declaration = fun (_declaration: Syn.Ast.TypeDeclaration.t) -> []

let check_value_declaration = fun (_declaration: Syn.Ast.ValueDeclaration.t) -> []

let check_external_declaration = fun (_declaration: Syn.Ast.ExternalDeclaration.t) -> []

let check_exception_rhs = fun (_node: Syn.Ast.Node.t) -> []
