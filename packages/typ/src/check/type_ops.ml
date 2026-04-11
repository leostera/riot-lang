let unsupported_syntax = Core.unsupported_syntax

let unsupported_type = Core.unsupported_type

let check_core_type = Core.check_core_type

let check_type_definition = fun _ -> []

let check_type_declaration = fun declaration ->
  [ unsupported_syntax (Syn.Cst.TypeDeclaration.syntax_node declaration) "type declaration" ]

let check_value_declaration = fun (declaration : Syn.Cst.ValueDeclaration.t) ->
  Core.check_core_type (Syn.Cst.ValueDeclaration.type_ declaration)

let check_external_declaration = fun (declaration : Syn.Cst.external_declaration) ->
  Core.check_core_type declaration.type_

let check_exception_rhs = fun _ -> []
