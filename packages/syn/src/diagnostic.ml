open Std

type found_token = {
  kind : string; (* e.g. "trivia", "keyword", "operator" *)
  text : string; (* actual text from source *)
}
(** Structured parse error kinds *)

type kind =
  (* Specific parsing errors with helpful hints *)
  | MalformedTypeVariable of { found : found_token }
  | MissingLetBindingPattern of { found : found_token }
  | MissingLetBindingEquals of { found : found_token }
  | MissingLetBindingExpr of { found : found_token }
  | UnexpectedStructureItem of { found : found_token }
  | UnexpectedSignatureItem of { found : found_token }
  | InvalidPattern of { found : found_token }
  | InvalidExpression of { found : found_token }
  | InvalidConstant of { found : found_token }
  | InvalidTypeExpression of { found : found_token }
  | MissingLetKeyword of { found : found_token }
  | MissingTypeKeyword of { found : found_token }
  | MissingTypeName of { found : found_token }
  | MissingTypeDeclEquals of { found : found_token }
  | UnclosedDelimiter of { opener : string; found : found_token }
  | UnclosedTypeParams of { found : found_token }
  | EmptyCharLiteral
  | MultiCharLiteral of { text : string }
  | UnclosedCharLiteral of { text : string }
  | MissingBinaryOperand of { operator : string; side : string; found : found_token }
  | ConsecutiveBinaryOperators of { operators : string; found : found_token }
  | InvalidTypeParameter of { text : string; found : found_token }
  | UppercaseTypeVariable of { text : string; found : found_token }
  | UppercaseTypeName of { text : string; found : found_token }
  | BracketedTypeParameters of { type_name : string; found : found_token }
  | ListDoubleSemicolon of { found : found_token }
  | IfMissingThen of { found : found_token }

type t = { kind : kind; span : Ceibo.Span.t }
(** Parse error information *)

let make ~kind ~span = { kind; span }

let malformed_type_variable ~found:token ~text ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MalformedTypeVariable { found }) ~span

let missing_let_binding_pattern ~found:token ~text ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingLetBindingPattern { found }) ~span

let missing_let_binding_equals ~found:token ~text ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingLetBindingEquals { found }) ~span

let missing_let_binding_expr ~found:token ~text ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingLetBindingExpr { found }) ~span

let unexpected_structure_item ~found:token ~text ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(UnexpectedStructureItem { found }) ~span

let unexpected_signature_item ~found:token ~text ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(UnexpectedSignatureItem { found }) ~span

let invalid_pattern ~found:token ~text ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(InvalidPattern { found }) ~span

let invalid_expression ~found:token ~text ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(InvalidExpression { found }) ~span

let invalid_constant ~found:token ~text ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(InvalidConstant { found }) ~span

let invalid_type_expression ~found:token ~text ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(InvalidTypeExpression { found }) ~span

let missing_let_keyword ~found:token ~text ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingLetKeyword { found }) ~span

let missing_type_keyword ~found:token ~text ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingTypeKeyword { found }) ~span

let missing_type_name ~found:token ~text ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingTypeName { found }) ~span

let missing_type_decl_equals ~found:token ~text ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingTypeDeclEquals { found }) ~span

let unclosed_delimiter ~opener ~found:token ~text ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(UnclosedDelimiter { opener; found }) ~span

let empty_char_literal ~span = make ~kind:EmptyCharLiteral ~span

let multi_char_literal ~text ~span =
  make ~kind:(MultiCharLiteral { text }) ~span

let unclosed_char_literal ~text ~span =
  make ~kind:(UnclosedCharLiteral { text }) ~span

let unclosed_type_params ~found:token ~text ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(UnclosedTypeParams { found }) ~span

let missing_binary_operand ~operator ~side ~found:token ~text ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(MissingBinaryOperand { operator; side; found }) ~span

let consecutive_binary_operators ~operators ~found:token ~text ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(ConsecutiveBinaryOperators { operators; found }) ~span

let invalid_type_parameter ~text ~found:token ~text_found ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text = text_found } in
  make ~kind:(InvalidTypeParameter { text; found }) ~span

let uppercase_type_variable ~text ~found:token ~text_found ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text = text_found } in
  make ~kind:(UppercaseTypeVariable { text; found }) ~span

let uppercase_type_name ~text ~found:token ~text_found ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text = text_found } in
  make ~kind:(UppercaseTypeName { text; found }) ~span

let bracketed_type_parameters ~type_name ~found:token ~text ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(BracketedTypeParameters { type_name; found }) ~span

let list_double_semicolon ~found:token ~text ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(ListDoubleSemicolon { found }) ~span

let if_missing_then ~found:token ~text ~span =
  let kind_str = Token.to_string token in
  let found = { kind = kind_str; text } in
  make ~kind:(IfMissingThen { found }) ~span

let expected_message diag =
  match diag.kind with
  | MalformedTypeVariable _ -> "type variable identifier (e.g., 'a, 'b)"
  | MissingLetBindingPattern _ ->
      "pattern (e.g., variable, _, Some x, or literal)"
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
      | "{" -> "}"
      | "[" -> "]"
      | "[|" -> "|]"
      | _ -> "closing delimiter")
  | UnclosedTypeParams _ -> ")"
  | EmptyCharLiteral -> "non-empty character literal"
  | MultiCharLiteral _ -> "single character"
  | UnclosedCharLiteral _ -> "' (closing quote)"
  | MissingBinaryOperand { operator; side; _ } ->
      side ^ " operand for " ^ operator
  | ConsecutiveBinaryOperators { operators; _ } ->
      "expression between operators in " ^ operators
  | InvalidTypeParameter _ ->
      "valid type parameter"
  | UppercaseTypeVariable _ ->
      "lowercase type variable"
  | UppercaseTypeName _ ->
      "lowercase type name"
  | BracketedTypeParameters _ ->
      "type parameters with ('a, 'b) syntax"
  | ListDoubleSemicolon _ ->
      "single semicolon between list elements"
  | IfMissingThen _ ->
      "then keyword"

let fix_message diag =
  match diag.kind with
  | MalformedTypeVariable _ ->
      Some "remove the space or comment between the quote and the variable name"
  | MissingLetBindingPattern _ ->
      Some "add a variable name, \"_\" (underscore) to ignore it."
  | MissingLetBindingEquals _ -> Some "add = between the pattern and expression"
  | MissingLetBindingExpr _ -> Some "add an expression after the ="
  | MissingTypeName _ -> Some "add a type name after the type keyword"
  | MissingTypeDeclEquals _ ->
      Some "add = between the type name and type definition"
  | UnclosedDelimiter { opener; _ } ->
      Some
        (match opener with
        | "(" -> "add ) to close the parenthesis"
        | "begin" -> "add end to close the begin block"
        | "{" -> "add } to close the brace"
        | "[" -> "add ] to close the bracket"
        | "[|" -> "add |] to close the array"
        | _ -> "add closing delimiter")
  | UnclosedTypeParams _ -> Some "add ) to close the type parameter list"
  | InvalidPattern { found } -> (
      match found.kind with
      | "keyword" -> Some (format "use a different name like %s_" found.text)
      | _ -> Some "add a variable name, \"_\" (underscore) to ignore it.")
  | EmptyCharLiteral -> Some "add a character between the quotes, e.g. 'a'"
  | MultiCharLiteral _ -> Some "use only one character in the literal"
  | UnclosedCharLiteral _ -> Some "add a closing ' (quote) after the character"
  | MissingBinaryOperand { side; _ } -> (
      match side with
      | "right" -> Some "add an expression after the operator"
      | "left" -> Some "add an expression before the operator"
      | _ -> None)
  | ConsecutiveBinaryOperators _ ->
      Some "add an expression between the operators or remove one operator"
  | InvalidTypeParameter { text; _ } ->
      if text = "__" then Some "use _ instead of __"
      else Some "use a valid type parameter like 'a or _"
  | UppercaseTypeVariable { text; _ } ->
      Some (format "change %s to %s" text (String.lowercase_ascii text))
  | UppercaseTypeName { text; _ } ->
      let lower = String.lowercase_ascii text in
      Some (format "change %s to %s" text lower)
  | BracketedTypeParameters { type_name; _ } ->
      Some (format "put generics on the left of the type name with parenthesis, like this: ('a, 'b) %s" type_name)
  | ListDoubleSemicolon _ ->
      Some "remove one semicolon - list elements are separated by single semicolons"
  | IfMissingThen _ ->
      Some "add 'then' keyword after the condition"
  | _ -> None

let error_id diag =
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

let hint_message diag = diag |> error_id |> Error.explain
let id err = err |> error_id |> Error.id_to_string

let to_string err =
  let id = id err in
  let fix = fix_message err in
  let hint = hint_message err in
  let message =
    match fix with
    | Some fix -> format "fix: %s\n\nhint: %s" fix hint
    | None -> format "hint: %s" hint
  in
  format "Parse error %S at %s: %s" id (Ceibo.Span.to_string err.span) message

let found_token diag =
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

let main_message diag =
  match diag.kind with
  | EmptyCharLiteral -> "empty character literal"
  | MultiCharLiteral { text } ->
      format "character literal '%s' contains multiple characters" text
  | UnclosedCharLiteral { text } -> format "missing ' (quote) after %s" text
  | _ ->
      let expected = expected_message diag in
      let found_kind = (found_token diag).kind in
      format "expected %s, found %s" expected found_kind

(** Convert diagnostic to JSON for machine consumption *)
let to_json err =
  let open Data.Json in
  let found = found_token err in
  let err_id = error_id err in
  let fix_fields =
    match fix_message err with
    | Some fix -> [ ("fix", String fix) ]
    | None -> []
  in
  Object
    [
      ( "kind",
        Object
          ([
             ("id", String (Error.id_to_string err_id));
             ("name", String (Error.name err_id));
             ("expected", String (expected_message err));
             ( "found",
               Object
                 [ ("kind", String found.kind); ("text", String found.text) ] );
           ]
          @ fix_fields
          @ [ ("hint", String (hint_message err)) ]) );
      ( "span",
        Object [ ("start", Int err.span.start); ("end", Int err.span.end_) ] );
    ]

(* parse_result removed - now in Parser module *)
