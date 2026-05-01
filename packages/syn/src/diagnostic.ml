open Std

(** Structured parse error kinds *)
type found_token = {
  kind: string;
  (* e.g. "trivia", "keyword", "operator" *)
  text: string;
  (* actual text from source *)
}

type kind =
  (* Specific parsing errors with helpful hints *)
  | MalformedTypeVariable of {
      found: found_token;
    }
  | MissingLetBindingPattern of {
      found: found_token;
    }
  | MissingLetBindingEquals of {
      found: found_token;
    }
  | MissingLetBindingExpr of {
      found: found_token;
    }
  | UnexpectedStructureItem of {
      found: found_token;
    }
  | UnexpectedSignatureItem of {
      found: found_token;
    }
  | InvalidPattern of {
      found: found_token;
    }
  | InvalidExpression of {
      found: found_token;
    }
  | InvalidConstant of {
      found: found_token;
    }
  | InvalidTypeExpression of {
      found: found_token;
    }
  | MissingLetKeyword of {
      found: found_token;
    }
  | MissingTypeKeyword of {
      found: found_token;
    }
  | MissingTypeName of {
      found: found_token;
    }
  | MissingTypeDeclEquals of {
      found: found_token;
    }
  | UnclosedDelimiter of {
      opener: string;
      found: found_token;
    }
  | UnclosedTypeParams of {
      found: found_token;
    }
  | EmptyCharLiteral
  | MultiCharLiteral of { text: string }
  | UnclosedCharLiteral of { text: string }
  | MissingBinaryOperand of {
      operator: string;
      side: string;
      found: found_token;
    }
  | ConsecutiveBinaryOperators of {
      operators: string;
      found: found_token;
    }
  | InvalidTypeParameter of {
      text: string;
      found: found_token;
    }
  | UppercaseTypeVariable of {
      text: string;
      found: found_token;
    }
  | UppercaseTypeName of {
      text: string;
      found: found_token;
    }
  | BracketedTypeParameters of {
      type_name: string;
      found: found_token;
    }
  | ListDoubleSemicolon of {
      found: found_token;
    }
  | IfMissingThen of {
      found: found_token;
    }
  | MatchMissingScrutinee of {
      found: found_token;
    }
  | MatchMissingWith of {
      found: found_token;
    }
  | MatchMissingPattern of {
      found: found_token;
    }
  | MatchGuardMissingExpr of {
      found: found_token;
    }
  | TuplePatternExtraComma of {
      found: found_token;
    }
  | ConstructorPatternNeedsParens of {
      constructor: string;
      found: found_token;
    }
  | ConsPatternMissingHead of {
      found: found_token;
    }
  | ConsPatternMissingTail of {
      found: found_token;
    }
  | OrPatternMissing of {
      found: found_token;
    }
  | OrPatternDouble of {
      found: found_token;
    }
  | MutableFieldMissingName of {
      found: found_token;
    }
  | RecordFieldMissingColon of {
      field_name: string;
      found: found_token;
    }
  | RecordFieldMissingType of {
      field_name: string;
      found: found_token;
    }
  | PolyTypeMissingVarName of {
      found: found_token;
    }
  | PolyTypeMissingDot of {
      found: found_token;
    }
  | UnexpectedClosingDelimiter of {
      delimiter: string;
      found: found_token;
    }
  | MissingModuleDeclEquals of {
      found: found_token;
    }
  | MissingExternalColon of {
      found: found_token;
    }
  | MissingExceptionName of {
      found: found_token;
    }
  | MissingModulePath of {
      found: found_token;
    }
  | MissingModuleTypeName of {
      found: found_token;
    }
  | MissingModuleTypeExpr of {
      found: found_token;
    }
  | MissingModuleExpr of {
      found: found_token;
    }
  | MissingWithKeyword of {
      found: found_token;
    }
  | InvalidModuleName of {
      found: found_token;
    }

type t = {
  kind: kind;
  span: Span.t;
}

let make = fun ~kind ~span -> { kind; span }

let malformed_type_variable = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MalformedTypeVariable { found }) ~span

let missing_let_binding_pattern = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingLetBindingPattern { found }) ~span

let missing_let_binding_equals = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingLetBindingEquals { found }) ~span

let missing_let_binding_expr = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingLetBindingExpr { found }) ~span

let unexpected_structure_item = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(UnexpectedStructureItem { found }) ~span

let unexpected_signature_item = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(UnexpectedSignatureItem { found }) ~span

let invalid_pattern = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(InvalidPattern { found }) ~span

let invalid_expression = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(InvalidExpression { found }) ~span

let invalid_constant = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(InvalidConstant { found }) ~span

let invalid_type_expression = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(InvalidTypeExpression { found }) ~span

let missing_let_keyword = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingLetKeyword { found }) ~span

let missing_type_keyword = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingTypeKeyword { found }) ~span

let missing_type_name = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingTypeName { found }) ~span

let missing_type_decl_equals = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingTypeDeclEquals { found }) ~span

let unclosed_delimiter = fun ~opener ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(UnclosedDelimiter { opener; found }) ~span

let empty_char_literal = fun ~span -> make ~kind:EmptyCharLiteral ~span

let multi_char_literal = fun ~text ~span -> make ~kind:(MultiCharLiteral { text }) ~span

let unclosed_char_literal = fun ~text ~span -> make ~kind:(UnclosedCharLiteral { text }) ~span

let unclosed_type_params = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(UnclosedTypeParams { found }) ~span

let missing_binary_operand = fun ~operator ~side ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingBinaryOperand { operator; side; found }) ~span

let consecutive_binary_operators = fun ~operators ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(ConsecutiveBinaryOperators { operators; found }) ~span

let invalid_type_parameter = fun ~text ~found:token ~text_found ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text = text_found } in
  make ~kind:(InvalidTypeParameter { text; found }) ~span

let uppercase_type_variable = fun ~text ~found:token ~text_found ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text = text_found } in
  make ~kind:(UppercaseTypeVariable { text; found }) ~span

let uppercase_type_name = fun ~text ~found:token ~text_found ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text = text_found } in
  make ~kind:(UppercaseTypeName { text; found }) ~span

let bracketed_type_parameters = fun ~type_name ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(BracketedTypeParameters { type_name; found }) ~span

let list_double_semicolon = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(ListDoubleSemicolon { found }) ~span

let if_missing_then = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(IfMissingThen { found }) ~span

let match_missing_scrutinee = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MatchMissingScrutinee { found }) ~span

let match_missing_with = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MatchMissingWith { found }) ~span

let match_missing_pattern = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MatchMissingPattern { found }) ~span

let match_guard_missing_expr = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MatchGuardMissingExpr { found }) ~span

let tuple_pattern_extra_comma = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(TuplePatternExtraComma { found }) ~span

let constructor_pattern_needs_parens = fun ~constructor ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(ConstructorPatternNeedsParens { constructor; found }) ~span

let cons_pattern_missing_head = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(ConsPatternMissingHead { found }) ~span

let cons_pattern_missing_tail = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(ConsPatternMissingTail { found }) ~span

let or_pattern_missing = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(OrPatternMissing { found }) ~span

let or_pattern_double = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(OrPatternDouble { found }) ~span

let mutable_field_missing_name = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MutableFieldMissingName { found }) ~span

let record_field_missing_colon = fun ~field_name ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(RecordFieldMissingColon { field_name; found }) ~span

let record_field_missing_type = fun ~field_name ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(RecordFieldMissingType { field_name; found }) ~span

let poly_type_missing_var_name = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(PolyTypeMissingVarName { found }) ~span

let poly_type_missing_dot = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(PolyTypeMissingDot { found }) ~span

let unexpected_closing_delimiter = fun ~delimiter ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(UnexpectedClosingDelimiter { delimiter; found }) ~span

let missing_module_decl_equals = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingModuleDeclEquals { found }) ~span

let missing_external_colon = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingExternalColon { found }) ~span

let missing_exception_name = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingExceptionName { found }) ~span

let missing_module_path = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingModulePath { found }) ~span

let missing_module_type_name = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingModuleTypeName { found }) ~span

let missing_module_type_expr = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingModuleTypeExpr { found }) ~span

let missing_module_expr = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingModuleExpr { found }) ~span

let missing_with_keyword = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingWithKeyword { found }) ~span

let invalid_module_name = fun ~found:token ~text ~span ->
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(InvalidModuleName { found }) ~span

let expected_message = fun diag ->
  match diag.kind with
  | MalformedTypeVariable _ -> "type variable identifier (e.g., 'a, 'b)"
  | MissingLetBindingPattern _ -> "pattern (e.g., variable, _, Some x, or literal)"
  | MissingLetBindingEquals _ -> "="
  | MissingLetBindingExpr _ -> "expression"
  | UnexpectedStructureItem _ -> "structure item (e.g., let, type, module)"
  | UnexpectedSignatureItem _ -> "signature item (e.g., val, type, module)"
  | InvalidPattern _ -> "identifier or pattern"
  | InvalidExpression _ -> "expression"
  | InvalidConstant _ -> "constant (integer, float, string, or char)"
  | InvalidTypeExpression _ -> "type definition"
  | MissingLetKeyword _ -> "let keyword"
  | MissingTypeKeyword _ -> "type keyword"
  | MissingTypeName _ -> "type name"
  | MissingTypeDeclEquals _ -> "="
  | UnclosedDelimiter { opener; _ } -> (
      match opener with
      | "(" -> ")"
      | "begin" -> "end"
      | "sig" -> "end"
      | "struct" -> "end"
      | "{" -> "}"
      | "[" -> "]"
      | "[|" -> "|]"
      | _ -> "closing delimiter"
    )
  | UnclosedTypeParams _ -> ")"
  | EmptyCharLiteral -> "non-empty character literal"
  | MultiCharLiteral _ -> "single character"
  | UnclosedCharLiteral _ -> "' (closing quote)"
  | MissingBinaryOperand { operator; side; _ } -> side ^ " operand for " ^ operator
  | ConsecutiveBinaryOperators { operators; _ } -> "expression between operators in " ^ operators
  | InvalidTypeParameter _ -> "valid type parameter"
  | UppercaseTypeVariable _ -> "lowercase type variable"
  | UppercaseTypeName _ -> "lowercase type name"
  | BracketedTypeParameters _ -> "type parameters with ('a, 'b) syntax"
  | ListDoubleSemicolon _ -> "single semicolon between list elements"
  | IfMissingThen _ -> "then keyword"
  | MatchMissingScrutinee _ -> "expression to match on"
  | MatchMissingWith _ -> "with keyword"
  | MatchMissingPattern _ -> "pattern before ->"
  | MatchGuardMissingExpr _ -> "boolean expression after when"
  | TuplePatternExtraComma _ -> "pattern element (not comma)"
  | ConstructorPatternNeedsParens { constructor; _ } ->
      "parentheses around " ^ constructor ^ " arguments"
  | ConsPatternMissingHead _ -> "pattern before ::"
  | ConsPatternMissingTail _ -> "pattern after ::"
  | OrPatternMissing _ -> "pattern (not bare |)"
  | OrPatternDouble _ -> "pattern between | operators"
  | MutableFieldMissingName _ -> "field name after mutable"
  | RecordFieldMissingColon { field_name; _ } -> "colon after field name '" ^ field_name ^ "'"
  | RecordFieldMissingType { field_name; _ } -> "type definition for field '" ^ field_name ^ "'"
  | PolyTypeMissingVarName _ -> "type variable name after '"
  | PolyTypeMissingDot _ -> ". (dot) after type variables in polymorphic type"
  | UnexpectedClosingDelimiter { delimiter; _ } -> "matching opening delimiter before " ^ delimiter
  | MissingModuleDeclEquals _ -> "="
  | MissingExternalColon _ -> ":"
  | MissingExceptionName _ -> "exception constructor name"
  | MissingModulePath _ -> "module name or module path"
  | MissingModuleTypeName _ -> "module type name"
  | MissingModuleTypeExpr _ -> "module type expression"
  | MissingModuleExpr _ -> "module expression"
  | MissingWithKeyword _ -> "with keyword"
  | InvalidModuleName _ -> "module name starting with uppercase letter"

let fix_message = fun diag ->
  match diag.kind with
  | MalformedTypeVariable _ ->
      Some "remove the space or comment between the quote and the variable name"
  | MissingLetBindingPattern _ -> Some "add a variable name, \"_\" (underscore) to ignore it."
  | MissingLetBindingEquals _ -> Some "add = between the pattern and expression"
  | MissingLetBindingExpr _ -> Some "add an expression after the ="
  | MissingTypeName _ -> Some "add a type name after the type keyword"
  | MissingTypeDeclEquals _ -> Some "add = between the type name and type definition"
  | UnclosedDelimiter { opener; _ } ->
      Some (
        match opener with
        | "(" -> "add ) to close the parenthesis"
        | "begin" -> "add end to close the begin block"
        | "sig" -> "add end to close the signature"
        | "struct" -> "add end to close the struct block"
        | "{" -> "add } to close the brace"
        | "[" -> "add ] to close the bracket"
        | "[|" -> "add |] to close the array"
        | _ -> "add closing delimiter"
      )
  | UnclosedTypeParams _ -> Some "add ) to close the type parameter list"
  | InvalidPattern { found } -> (
      match found.kind with
      | "keyword" ->
          Some ("use a different name like "
          ^ found.text
          ^ "_ or a raw identifier like \\#"
          ^ found.text)
      | _ -> Some "add a variable name, \"_\" (underscore) to ignore it."
    )
  | EmptyCharLiteral -> Some "add a character between the quotes, e.g. 'a' or '\\000'"
  | MultiCharLiteral _ -> Some "use only one character in the literal"
  | UnclosedCharLiteral _ -> Some "add a closing ' (quote) after the character"
  | MissingBinaryOperand { side; _ } -> (
      match side with
      | "right" -> Some "add an expression after the operator"
      | "left" -> Some "add an expression before the operator"
      | _ -> None
    )
  | ConsecutiveBinaryOperators _ ->
      Some "add an expression between the operators or remove one operator"
  | InvalidTypeParameter { text; _ } ->
      if text = "__" then
        Some "use _ instead of __"
      else
        Some "use a valid type parameter like 'a or _"
  | UppercaseTypeVariable { text; _ } ->
      Some ("change " ^ text ^ " to " ^ String.lowercase_ascii text)
  | UppercaseTypeName { text; _ } ->
      let lower = String.lowercase_ascii text in
      Some ("change " ^ text ^ " to " ^ lower)
  | BracketedTypeParameters { type_name; _ } ->
      Some ("put generics on the left of the type name with parenthesis, like \
          this: ('a, 'b) "
      ^ type_name)
  | ListDoubleSemicolon _ ->
      Some "remove one semicolon - list elements are separated by single \
         semicolons"
  | IfMissingThen _ -> Some "add 'then' keyword after the condition"
  | MatchMissingScrutinee _ -> Some "add an expression after 'match' to specify what to match on"
  | MatchMissingWith _ -> Some "add 'with' keyword after the expression"
  | MatchMissingPattern _ -> Some "add a pattern before the '->' arrow"
  | MatchGuardMissingExpr _ -> Some "add a boolean expression after 'when'"
  | TuplePatternExtraComma _ -> Some "use () for unit or (_, _) for a tuple pattern"
  | ConstructorPatternNeedsParens { constructor; _ } ->
      Some ("wrap arguments in parentheses: " ^ constructor ^ " (...)")
  | ConsPatternMissingHead _ -> Some "add a pattern before the :: operator"
  | ConsPatternMissingTail _ -> Some "add a pattern after the :: operator"
  | OrPatternMissing _ -> Some "add patterns on both sides of the | operator"
  | OrPatternDouble _ -> Some "add a pattern between the | operators or remove one |"
  | MutableFieldMissingName _ -> Some "add a field name after the 'mutable' keyword"
  | RecordFieldMissingColon { field_name; _ } ->
      Some ("add a colon after field name '" ^ field_name ^ "'")
  | RecordFieldMissingType { field_name; _ } ->
      Some ("add a type definition after the colon for field '" ^ field_name ^ "'")
  | PolyTypeMissingVarName _ -> Some "add a type variable name (e.g., 'a, 'b) after the quote"
  | PolyTypeMissingDot _ -> Some "add a dot (.) after all type variables before the type definition"
  | UnexpectedClosingDelimiter { delimiter; _ } -> Some ("remove the extra " ^ delimiter)
  | MissingModuleDeclEquals _ -> Some "add = between the module name and module expression"
  | MissingExternalColon _ -> Some "add : between the external name and its type"
  | MissingExceptionName _ -> Some "add an exception constructor name after exception"
  | MissingModulePath _ -> Some "add a module name or module path"
  | MissingModuleTypeName _ -> Some "add a module type name after module type"
  | MissingModuleTypeExpr _ -> Some "add a module type expression after ="
  | MissingModuleExpr _ -> Some "add a module expression after module"
  | MissingWithKeyword _ -> Some "add with before the module type constraints"
  | InvalidModuleName { found } ->
      if found.text = "" then
        Some "capitalize the module name so it starts with an uppercase letter"
      else
        Some ("capitalize " ^ found.text ^ " as " ^ String.capitalize_ascii found.text)
  | _ -> None

let error_id = fun diag ->
  match diag.kind with
  | MalformedTypeVariable _ -> Error.E0001_MalformedTypeVariable
  | MissingLetBindingPattern _ -> Error.E0002_MissingLetBindingPattern
  | MissingLetBindingEquals _ -> Error.E0003_MissingLetBindingEquals
  | MissingLetBindingExpr _ -> Error.E0004_MissingLetBindingExpr
  | UnexpectedStructureItem _ -> Error.E0005_UnexpectedStructureItem
  | UnexpectedSignatureItem _ -> Error.E0006_UnexpectedSignatureItem
  | InvalidPattern _ -> Error.E0007_InvalidPattern
  | InvalidExpression _ -> Error.E0008_InvalidExpression
  | InvalidConstant _ -> Error.E0009_InvalidConstant
  | InvalidTypeExpression _ -> Error.E0010_InvalidTypeExpression
  | MissingLetKeyword _ -> Error.E0011_MissingLetKeyword
  | MissingTypeKeyword _ -> Error.E0012_MissingTypeKeyword
  | MissingTypeDeclEquals _ -> Error.E0013_MissingTypeDeclEquals
  | UnclosedDelimiter _ -> Error.E0014_UnclosedDelimiter
  | MissingTypeName _ -> Error.E0015_MissingTypeName
  | EmptyCharLiteral -> Error.E0016_EmptyCharLiteral
  | MultiCharLiteral _ -> Error.E0017_MultiCharLiteral
  | UnclosedCharLiteral _ -> Error.E0018_UnclosedCharLiteral
  | UnclosedTypeParams _ -> Error.E0019_UnclosedTypeParams
  | MissingBinaryOperand _ -> Error.E0020_MissingBinaryOperand
  | ConsecutiveBinaryOperators _ -> Error.E0021_ConsecutiveBinaryOperators
  | InvalidTypeParameter _ -> Error.E0022_InvalidTypeParameter
  | UppercaseTypeVariable _ -> Error.E0023_UppercaseTypeVariable
  | UppercaseTypeName _ -> Error.E0024_UppercaseTypeName
  | BracketedTypeParameters _ -> Error.E0025_BracketedTypeParameters
  | ListDoubleSemicolon _ -> Error.E0026_ListDoubleSemicolon
  | IfMissingThen _ -> Error.E0027_IfMissingThen
  | MatchMissingScrutinee _ -> Error.E0028_MatchMissingScrutinee
  | MatchMissingWith _ -> Error.E0029_MatchMissingWith
  | MatchMissingPattern _ -> Error.E0030_MatchMissingPattern
  | MatchGuardMissingExpr _ -> Error.E0031_MatchGuardMissingExpr
  | TuplePatternExtraComma _ -> Error.E0032_TuplePatternExtraComma
  | ConstructorPatternNeedsParens _ -> Error.E0033_ConstructorPatternNeedsParens
  | ConsPatternMissingHead _ -> Error.E0034_ConsPatternMissingHead
  | ConsPatternMissingTail _ -> Error.E0035_ConsPatternMissingTail
  | OrPatternMissing _ -> Error.E0036_OrPatternMissing
  | OrPatternDouble _ -> Error.E0037_OrPatternDouble
  | MutableFieldMissingName _ -> Error.E0038_MutableFieldMissingName
  | RecordFieldMissingColon _ -> Error.E0039_RecordFieldMissingColon
  | RecordFieldMissingType _ -> Error.E0040_RecordFieldMissingType
  | PolyTypeMissingVarName _ -> Error.E0041_PolyTypeMissingVarName
  | PolyTypeMissingDot _ -> Error.E0042_PolyTypeMissingDot
  | UnexpectedClosingDelimiter _ -> Error.E0043_UnexpectedClosingDelimiter
  | MissingModuleDeclEquals _ -> Error.E0044_MissingModuleDeclEquals
  | MissingExternalColon _ -> Error.E0045_MissingExternalColon
  | MissingExceptionName _ -> Error.E0046_MissingExceptionName
  | MissingModulePath _ -> Error.E0047_MissingModulePath
  | MissingModuleTypeName _ -> Error.E0048_MissingModuleTypeName
  | MissingModuleTypeExpr _ -> Error.E0049_MissingModuleTypeExpr
  | MissingModuleExpr _ -> Error.E0050_MissingModuleExpr
  | MissingWithKeyword _ -> Error.E0051_MissingWithKeyword
  | InvalidModuleName _ -> Error.E0052_InvalidModuleName

let hint_message = fun diag ->
  diag
  |> error_id
  |> Error.explain

let id = fun err ->
  err
  |> error_id
  |> Error.id_to_string

let to_string = fun err ->
  let id = id err in
  let fix = fix_message err in
  let hint = hint_message err in
  let message =
    match fix with
    | Some fix -> "fix: " ^ fix ^ "\n\nhint: " ^ hint
    | None -> "hint: " ^ hint
  in
  "Parse error \"" ^ id ^ "\" at " ^ Span.to_string err.span ^ ": " ^ message

let found_token = fun diag ->
  match diag.kind with
  | MalformedTypeVariable { found } -> found
  | MissingLetBindingPattern { found } -> found
  | MissingLetBindingEquals { found } -> found
  | MissingLetBindingExpr { found } -> found
  | UnexpectedStructureItem { found } -> found
  | UnexpectedSignatureItem { found } -> found
  | InvalidPattern { found } -> found
  | InvalidExpression { found } -> found
  | InvalidConstant { found } -> found
  | InvalidTypeExpression { found } -> found
  | MissingLetKeyword { found } -> found
  | MissingTypeKeyword { found } -> found
  | MissingTypeName { found } -> found
  | MissingTypeDeclEquals { found } -> found
  | UnclosedDelimiter { found; _ } -> found
  | UnclosedTypeParams { found } -> found
  | EmptyCharLiteral -> { kind = "char literal"; text = "''" }
  | MultiCharLiteral { text } -> { kind = "char literal"; text }
  | UnclosedCharLiteral { text } -> { kind = "char literal"; text }
  | MissingBinaryOperand { found; _ } -> found
  | ConsecutiveBinaryOperators { found; _ } -> found
  | InvalidTypeParameter { found; _ } -> found
  | UppercaseTypeVariable { found; _ } -> found
  | UppercaseTypeName { found; _ } -> found
  | BracketedTypeParameters { found } -> found
  | ListDoubleSemicolon { found } -> found
  | IfMissingThen { found } -> found
  | MatchMissingScrutinee { found } -> found
  | MatchMissingWith { found } -> found
  | MatchMissingPattern { found } -> found
  | MatchGuardMissingExpr { found } -> found
  | TuplePatternExtraComma { found } -> found
  | ConstructorPatternNeedsParens { found; _ } -> found
  | ConsPatternMissingHead { found } -> found
  | ConsPatternMissingTail { found } -> found
  | OrPatternMissing { found } -> found
  | OrPatternDouble { found } -> found
  | MutableFieldMissingName { found } -> found
  | RecordFieldMissingColon { found; _ } -> found
  | RecordFieldMissingType { found; _ } -> found
  | PolyTypeMissingVarName { found } -> found
  | PolyTypeMissingDot { found } -> found
  | UnexpectedClosingDelimiter { found; _ } -> found
  | MissingModuleDeclEquals { found } -> found
  | MissingExternalColon { found } -> found
  | MissingExceptionName { found } -> found
  | MissingModulePath { found } -> found
  | MissingModuleTypeName { found } -> found
  | MissingModuleTypeExpr { found } -> found
  | MissingModuleExpr { found } -> found
  | MissingWithKeyword { found } -> found
  | InvalidModuleName { found } -> found

let main_message = fun diag ->
  match diag.kind with
  | EmptyCharLiteral -> "empty character literal"
  | MultiCharLiteral { text } -> "character literal '" ^ text ^ "' contains multiple characters"
  | UnclosedCharLiteral { text } -> "missing ' (quote) after " ^ text
  | UnexpectedClosingDelimiter { delimiter; _ } -> "unexpected extra closing " ^ delimiter
  | MissingModuleDeclEquals _ -> "expected = between module name and module expression"
  | MissingExternalColon _ -> "expected : between external name and type"
  | MissingExceptionName _ -> "expected exception constructor name"
  | MissingModulePath _ -> "expected module name or module path"
  | MissingModuleTypeName _ -> "expected module type name"
  | MissingModuleTypeExpr _ -> "expected module type expression"
  | MissingModuleExpr _ -> "expected module expression"
  | MissingWithKeyword _ -> "expected with before the module type constraints"
  | InvalidModuleName { found } -> "invalid module name " ^ found.text
  | _ ->
      let expected = expected_message diag in
      let found_kind = (found_token diag).kind in
      "expected " ^ expected ^ ", found " ^ found_kind

(** Convert diagnostic to JSON for machine consumption *)
let to_json = fun err ->
  let open Data.Json in
  let found = found_token err in
  let err_id = error_id err in
  let fix_fields =
    match fix_message err with
    | Some fix -> [ ("fix", String fix); ]
    | None -> []
  in
  Object [
    (
      "kind",
      Object (([
        ("id", String (Error.id_to_string err_id));
        ("name", String (Error.name err_id));
        ("expected", String (expected_message err));
        ("found", Object [ ("kind", String found.kind); ("text", String found.text); ]);
      ]
      @ fix_fields)
      @ [ ("hint", String (hint_message err)); ])
    );
    ("span", Object [ ("start", Int err.span.start); ("end", Int err.span.end_); ]);
  ]

(* parse_result removed - now in Parser module *)

let from_json = fun json ->
  let open Data.Json in
  let field name fields =
    List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name)
    |> Option.map ~fn:(fun (_, value) -> value)
  in
  match json with
  | Object fields -> (
      let kind_json = field "kind" fields in
      let span_json = field "span" fields in
      match (kind_json, span_json) with
      | (Some (Object kind_fields), Some (Object span_fields)) -> (
          let id_opt =
            Option.and_then
              (field "id" kind_fields)
              ~fn:(fun __tmp1 ->
                match __tmp1 with
                | String s -> Some s
                | _ -> None)
          in
          let found_obj = field "found" kind_fields in
          let found =
            match found_obj with
            | Some (Object found_fields) ->
                let kind =
                  Option.and_then
                    (field "kind" found_fields)
                    ~fn:(fun __tmp1 ->
                      match __tmp1 with
                      | String s -> Some s
                      | _ -> None)
                  |> Option.unwrap_or ~default:""
                in
                let text =
                  Option.and_then
                    (field "text" found_fields)
                    ~fn:(fun __tmp1 ->
                      match __tmp1 with
                      | String s -> Some s
                      | _ -> None)
                  |> Option.unwrap_or ~default:""
                in
                { kind; text }
            | _ -> { kind = ""; text = "" }
          in
          let start =
            Option.and_then
              (field "start" span_fields)
              ~fn:(fun __tmp1 ->
                match __tmp1 with
                | Int n -> Some n
                | _ -> None)
            |> Option.unwrap_or ~default:0
          in
          let end_ =
            Option.and_then
              (field "end" span_fields)
              ~fn:(fun __tmp1 ->
                match __tmp1 with
                | Int n -> Some n
                | _ -> None)
            |> Option.unwrap_or ~default:0
          in
          let span = Span.make ~start ~end_ in
          match id_opt with
          | Some id ->
              let expected =
                Option.and_then
                  (field "expected" kind_fields)
                  ~fn:(fun __tmp1 ->
                    match __tmp1 with
                    | String s -> Some s
                    | _ -> None)
                |> Option.unwrap_or ~default:""
              in
              let fix =
                Option.and_then
                  (field "fix" kind_fields)
                  ~fn:(fun __tmp1 ->
                    match __tmp1 with
                    | String s -> Some s
                    | _ -> None)
                |> Option.unwrap_or ~default:""
              in
              let strip_prefix_str str ~prefix =
                let prefix_len = String.length prefix in
                let str_len = String.length str in
                if str_len >= prefix_len && String.starts_with ~prefix str then
                  String.sub str ~offset:prefix_len ~len:(str_len - prefix_len)
                else
                  ""
              in
              let parse_missing_binary_operand expected =
                let right_prefix = "right operand for " in
                let left_prefix = "left operand for " in
                if String.starts_with ~prefix:right_prefix expected then
                  ("right", strip_prefix_str expected ~prefix:right_prefix)
                else if String.starts_with ~prefix:left_prefix expected then
                  ("left", strip_prefix_str expected ~prefix:left_prefix)
                else
                  ("", "")
              in
              let parse_consecutive_binary_operators expected =
                let operators_prefix = "expression between operators in " in
                strip_prefix_str expected ~prefix:operators_prefix
              in
              let parse_unclosed_delimiter expected fix =
                match expected with
                | "closing delimiter" -> ""
                | ")" -> "("
                | "]" -> "["
                | "}" -> "{"
                | "end" ->
                    if String.starts_with ~prefix:"add end to close the begin block" fix then
                      "begin"
                    else if String.starts_with ~prefix:"add end to close the struct block" fix then
                      "struct"
                    else if String.starts_with ~prefix:"add end to close the signature" fix then
                      "sig"
                    else
                      ""
                | _ -> ""
              in
              let parse_bracketed_type_name fix =
                let prefix =
                  "put generics on the left of the type name with parenthesis, like this: "
                in
                let tail = strip_prefix_str fix ~prefix in
                let type_prefix = "('a, 'b) " in
                if tail = "" then
                  ""
                else if String.starts_with ~prefix:type_prefix tail then
                  strip_prefix_str tail ~prefix:type_prefix
                else
                  ""
              in
              let parse_quoted_field expected ~prefix =
                if not (String.starts_with ~prefix expected) then
                  ""
                else
                  let rest = strip_prefix_str expected ~prefix in
                  match String.index_of rest ~char:'\'' with
                  | Some idx -> String.sub rest ~offset:0 ~len:idx
                  | None -> rest
              in
              let kind =
                match Error.id_of_string id with
                | Some E0001_MalformedTypeVariable -> MalformedTypeVariable { found }
                | Some E0002_MissingLetBindingPattern -> MissingLetBindingPattern { found }
                | Some E0003_MissingLetBindingEquals -> MissingLetBindingEquals { found }
                | Some E0004_MissingLetBindingExpr -> MissingLetBindingExpr { found }
                | Some E0005_UnexpectedStructureItem -> UnexpectedStructureItem { found }
                | Some E0006_UnexpectedSignatureItem -> UnexpectedSignatureItem { found }
                | Some E0007_InvalidPattern -> InvalidPattern { found }
                | Some E0008_InvalidExpression -> InvalidExpression { found }
                | Some E0009_InvalidConstant -> InvalidConstant { found }
                | Some E0010_InvalidTypeExpression -> InvalidTypeExpression { found }
                | Some E0011_MissingLetKeyword -> MissingLetKeyword { found }
                | Some E0012_MissingTypeKeyword -> MissingTypeKeyword { found }
                | Some E0013_MissingTypeDeclEquals -> MissingTypeDeclEquals { found }
                | Some E0014_UnclosedDelimiter ->
                    UnclosedDelimiter { opener = parse_unclosed_delimiter expected fix; found }
                | Some E0015_MissingTypeName -> MissingTypeName { found }
                | Some E0016_EmptyCharLiteral -> EmptyCharLiteral
                | Some E0017_MultiCharLiteral -> MultiCharLiteral { text = found.text }
                | Some E0018_UnclosedCharLiteral -> UnclosedCharLiteral { text = found.text }
                | Some E0019_UnclosedTypeParams -> UnclosedTypeParams { found }
                | Some E0020_MissingBinaryOperand ->
                    let (side, operator) = parse_missing_binary_operand expected in
                    MissingBinaryOperand { operator; side; found }
                | Some E0021_ConsecutiveBinaryOperators ->
                    ConsecutiveBinaryOperators {
                      operators = parse_consecutive_binary_operators expected;
                      found;
                    }
                | Some E0022_InvalidTypeParameter ->
                    InvalidTypeParameter { text = found.text; found }
                | Some E0023_UppercaseTypeVariable ->
                    UppercaseTypeVariable { text = found.text; found }
                | Some E0024_UppercaseTypeName -> UppercaseTypeName { text = found.text; found }
                | Some E0025_BracketedTypeParameters ->
                    BracketedTypeParameters { type_name = parse_bracketed_type_name fix; found }
                | Some E0026_ListDoubleSemicolon -> ListDoubleSemicolon { found }
                | Some E0027_IfMissingThen -> IfMissingThen { found }
                | Some E0028_MatchMissingScrutinee -> MatchMissingScrutinee { found }
                | Some E0029_MatchMissingWith -> MatchMissingWith { found }
                | Some E0030_MatchMissingPattern -> MatchMissingPattern { found }
                | Some E0031_MatchGuardMissingExpr -> MatchGuardMissingExpr { found }
                | Some E0032_TuplePatternExtraComma -> TuplePatternExtraComma { found }
                | Some E0033_ConstructorPatternNeedsParens ->
                    ConstructorPatternNeedsParens { constructor = ""; found }
                | Some E0034_ConsPatternMissingHead -> ConsPatternMissingHead { found }
                | Some E0035_ConsPatternMissingTail -> ConsPatternMissingTail { found }
                | Some E0036_OrPatternMissing -> OrPatternMissing { found }
                | Some E0037_OrPatternDouble -> OrPatternDouble { found }
                | Some E0038_MutableFieldMissingName -> MutableFieldMissingName { found }
                | Some E0039_RecordFieldMissingColon ->
                    RecordFieldMissingColon {
                      field_name = parse_quoted_field expected ~prefix:"colon after field name '";
                      found;
                    }
                | Some E0040_RecordFieldMissingType ->
                    RecordFieldMissingType {
                      field_name = parse_quoted_field expected ~prefix:"type definition for field '";
                      found;
                    }
                | Some E0041_PolyTypeMissingVarName -> PolyTypeMissingVarName { found }
                | Some E0042_PolyTypeMissingDot -> PolyTypeMissingDot { found }
                | Some E0043_UnexpectedClosingDelimiter ->
                    UnexpectedClosingDelimiter { delimiter = found.text; found }
                | Some E0044_MissingModuleDeclEquals -> MissingModuleDeclEquals { found }
                | Some E0045_MissingExternalColon -> MissingExternalColon { found }
                | Some E0046_MissingExceptionName -> MissingExceptionName { found }
                | Some E0047_MissingModulePath -> MissingModulePath { found }
                | Some E0048_MissingModuleTypeName -> MissingModuleTypeName { found }
                | Some E0049_MissingModuleTypeExpr -> MissingModuleTypeExpr { found }
                | Some E0050_MissingModuleExpr -> MissingModuleExpr { found }
                | Some E0051_MissingWithKeyword -> MissingWithKeyword { found }
                | Some E0052_InvalidModuleName -> InvalidModuleName { found }
                | _ -> InvalidExpression { found }
              in
              Ok { kind; span }
          | None -> Error "Missing 'id' field in diagnostic kind"
        )
      | _ -> Error "Invalid diagnostic JSON structure"
    )
  | _ -> Error "Expected JSON object for diagnostic"
