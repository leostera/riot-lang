open Std

(**
   Structured parser diagnostics for recoverable OCaml syntax errors.

   This module defines structured representations of parse errors.

   Unlike simple error strings, diagnostics are structured data that can be:
   - Rendered as human-readable messages
   - Serialized to JSON for tooling
   - Filtered/sorted by severity or category
   - Used by IDEs for inline error display

   Parser recovery model:
   The parser **never fails**. When it encounters malformed code, it:
   1. Creates ERROR/MISSING nodes in the syntax tree
   2. Records a diagnostic describing the problem
   3. Continues parsing to find more errors

   This enables:
   - Reporting multiple errors in one pass
   - Producing usable trees from incomplete code
   - Better IDE support (errors don't block analysis)
*)
type found_token = {
  (** Token kind description like "trivia", "keyword" *)
  kind: string;
  (** Actual text from source *)
  text: string;
}
(** A diagnostic with structured error information and source location. *)
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
  (**
     Token found in signature that doesn't start a signature item.

     Example: `42` in .mli file - not a valid signature item

     Expected: "signature item (e.g., val, type, module)" Hint: Expected a
     signature item like 'val', 'type', or 'module'.
  *)
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
(** A structured parser diagnostic with its source span. *)
type t = {
  kind: kind;
  span: Span.t;
}

(**
   `make ~kind ~span` creates a diagnostic.

   Example:
   ```ocaml
   let diag = Diagnostic.make
     ~kind:(MissingToken { expected = ")" })
     ~span:(Span.make ~start:10 ~end_:10)
   ```
*)
val make: kind:kind -> span:Span.t -> t

(**
   Create a "malformed type variable" diagnostic.

   Example: ```ocaml Diagnostic.malformed_type_variable ~found:token ~text:" "
   ~span:error_span ```
*)
val malformed_type_variable: found:Token.t -> text:string -> span:Span.t -> t

(**
   Create a "missing let binding pattern" diagnostic.

   Example: ```ocaml Diagnostic.missing_let_binding_pattern ~found:equals_token
   ~text:"=" ~span:error_span ```
*)
val missing_let_binding_pattern: found:Token.t -> text:string -> span:Span.t -> t

(**
   Create a "missing let binding equals" diagnostic.

   Example: ```ocaml Diagnostic.missing_let_binding_equals ~found:int_token
   ~text:"42" ~span:error_span ```
*)
val missing_let_binding_equals: found:Token.t -> text:string -> span:Span.t -> t

(**
   Create a "missing let binding expression" diagnostic.

   Example: ```ocaml Diagnostic.missing_let_binding_expr ~found:eof_token
   ~text:"" ~span:error_span ```
*)
val missing_let_binding_expr: found:Token.t -> text:string -> span:Span.t -> t

(**
   Create an "unexpected structure item" diagnostic.

   Example: ```ocaml Diagnostic.unexpected_structure_item ~found:token
   ~text:"42" ~span:error_span ```
*)
val unexpected_structure_item: found:Token.t -> text:string -> span:Span.t -> t

(** Create an "unexpected closing delimiter" diagnostic. *)
val unexpected_closing_delimiter:
  delimiter:string ->
  found:Token.t ->
  text:string ->
  span:Span.t ->
  t

(** Create a "missing module declaration equals" diagnostic. *)
val missing_module_decl_equals: found:Token.t -> text:string -> span:Span.t -> t

val missing_external_colon: found:Token.t -> text:string -> span:Span.t -> t

val missing_exception_name: found:Token.t -> text:string -> span:Span.t -> t

val missing_module_path: found:Token.t -> text:string -> span:Span.t -> t

val missing_module_type_name: found:Token.t -> text:string -> span:Span.t -> t

val missing_module_type_expr: found:Token.t -> text:string -> span:Span.t -> t

val missing_module_expr: found:Token.t -> text:string -> span:Span.t -> t

val missing_with_keyword: found:Token.t -> text:string -> span:Span.t -> t

val invalid_module_name: found:Token.t -> text:string -> span:Span.t -> t

(**
   Create an "unexpected signature item" diagnostic.

   Example: ```ocaml Diagnostic.unexpected_signature_item ~found:token
   ~text:"42" ~span:error_span ```
*)
val unexpected_signature_item: found:Token.t -> text:string -> span:Span.t -> t

val invalid_pattern: found:Token.t -> text:string -> span:Span.t -> t

val invalid_expression: found:Token.t -> text:string -> span:Span.t -> t

val invalid_constant: found:Token.t -> text:string -> span:Span.t -> t

val invalid_type_expression: found:Token.t -> text:string -> span:Span.t -> t

val missing_let_keyword: found:Token.t -> text:string -> span:Span.t -> t

val missing_type_keyword: found:Token.t -> text:string -> span:Span.t -> t

val missing_type_name: found:Token.t -> text:string -> span:Span.t -> t

val missing_type_decl_equals: found:Token.t -> text:string -> span:Span.t -> t

val unclosed_delimiter: opener:string -> found:Token.t -> text:string -> span:Span.t -> t

val unclosed_type_params: found:Token.t -> text:string -> span:Span.t -> t

val empty_char_literal: span:Span.t -> t

val multi_char_literal: text:string -> span:Span.t -> t

val unclosed_char_literal: text:string -> span:Span.t -> t

val missing_binary_operand:
  operator:string ->
  side:string ->
  found:Token.t ->
  text:string ->
  span:Span.t ->
  t

val consecutive_binary_operators:
  operators:string ->
  found:Token.t ->
  text:string ->
  span:Span.t ->
  t

val invalid_type_parameter: text:string -> found:Token.t -> text_found:string -> span:Span.t -> t

val uppercase_type_variable: text:string -> found:Token.t -> text_found:string -> span:Span.t -> t

val uppercase_type_name: text:string -> found:Token.t -> text_found:string -> span:Span.t -> t

val bracketed_type_parameters: type_name:string -> found:Token.t -> text:string -> span:Span.t -> t

val list_double_semicolon: found:Token.t -> text:string -> span:Span.t -> t

val if_missing_then: found:Token.t -> text:string -> span:Span.t -> t

val match_missing_scrutinee: found:Token.t -> text:string -> span:Span.t -> t

val match_missing_with: found:Token.t -> text:string -> span:Span.t -> t

val match_missing_pattern: found:Token.t -> text:string -> span:Span.t -> t

val match_guard_missing_expr: found:Token.t -> text:string -> span:Span.t -> t

val tuple_pattern_extra_comma: found:Token.t -> text:string -> span:Span.t -> t

val constructor_pattern_needs_parens:
  constructor:string ->
  found:Token.t ->
  text:string ->
  span:Span.t ->
  t

val cons_pattern_missing_head: found:Token.t -> text:string -> span:Span.t -> t

val cons_pattern_missing_tail: found:Token.t -> text:string -> span:Span.t -> t

val or_pattern_missing: found:Token.t -> text:string -> span:Span.t -> t

val or_pattern_double: found:Token.t -> text:string -> span:Span.t -> t

val mutable_field_missing_name: found:Token.t -> text:string -> span:Span.t -> t

val record_field_missing_colon:
  field_name:string ->
  found:Token.t ->
  text:string ->
  span:Span.t ->
  t

val record_field_missing_type: field_name:string -> found:Token.t -> text:string -> span:Span.t -> t

val poly_type_missing_var_name: found:Token.t -> text:string -> span:Span.t -> t

val poly_type_missing_dot: found:Token.t -> text:string -> span:Span.t -> t

(** `error_id diag` returns the error ID for this diagnostic. *)
val error_id: t -> Error.id

(** `id diag` returns the unique error identifier string for this diagnostic. *)
val id: t -> string

(** `kind_to_expected kind` returns what was expected at this position. *)
val expected_message: t -> string

(** `kind_to_fix kind` returns a quick fix suggestion (None if none). *)
val fix_message: t -> string option

(** `kind_to_hint diag` returns a detailed explanation hint. *)
val hint_message: t -> string

(** `main_message diag` returns the main error message for display. *)
val main_message: t -> string

val found_token: t -> found_token

(**
   `to_string diag` converts a diagnostic to a human-readable error message.

   Example output: ``` Missing token ')' at position 10 Unexpected token '+'
   (expected identifier) at 5..6 Invalid syntax in let binding at 12..18 ```

   Example: ```ocaml let msg = Diagnostic.to_string diag in Printf.eprintf
   "Error: %s\n" msg ```
*)
val to_string: t -> string

(**
   `to_json diag` converts a diagnostic to structured JSON.

   This is useful for:
   - IDE integration (language servers)
   - Build tools that consume machine-readable errors
   - Logging and error aggregation systems

   Example output:
   ```json
   {
     "kind": "MissingToken",
     "expected": ")",
     "span": { "start": 10, "end": 10 }
   }
   ```

   Example:
   ```ocaml
   let json = Diagnostic.to_json diag in
   let json_str = Data.Json.to_string json in
   print_endline json_str
   ```
*)
val to_json: t -> Data.Json.t

(**
   Deserialize a diagnostic from JSON.

   This is the inverse of [to_json], used for reading diagnostic test fixtures.

   Example: ```ocaml match Data.Json.of_string json_str with | Ok json -> (
   match Diagnostic.from_json json with | Ok diag -> (* use diagnostic *) |
   Error msg -> (* handle parse error *) ) | Error _ -> (* handle JSON parse
   error *) ```
*)
val from_json: Std.Data.Json.t -> (t, string) result
