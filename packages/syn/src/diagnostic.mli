open Std

(** Parse Diagnostics - Structured Error Information

    This module defines structured representations of parse errors.

    Unlike simple error strings, diagnostics are structured data that can be:
    - Rendered as human-readable messages
    - Serialized to JSON for tooling
    - Filtered/sorted by severity or category
    - Used by IDEs for inline error display

    # Philosophy

    The parser **never fails**. When it encounters malformed code, it: 1.
    Creates ERROR/MISSING nodes in the syntax tree 2. Records a diagnostic
    describing the problem 3. Continues parsing to find more errors

    This enables:
    - Reporting multiple errors in one pass
    - Producing usable trees from incomplete code
    - Better IDE support (errors don't block analysis) *)

(** # Types *)

type found_token = {
  kind : string;  (** Token kind description like "trivia", "keyword" *)
  text : string;  (** Actual text from source *)
}

type kind =
  (* Specific parsing errors with helpful hints *)
  | MalformedTypeVariable of { found : found_token }
  | MissingLetBindingPattern of { found : found_token }
  | MissingLetBindingEquals of { found : found_token }
  | MissingLetBindingExpr of { found : found_token }
  | UnexpectedStructureItem of { found : found_token }
  | UnexpectedSignatureItem of { found : found_token }
      (** Token found in signature that doesn't start a signature item.

          Example: `42` in .mli file - not a valid signature item

          Expected: "signature item (e.g., val, type, module)" Hint: Expected a
          signature item like 'val', 'type', or 'module'. *)
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

type t = { kind : kind; span : Ceibo.Span.t }
(** A diagnostic with structured error information and source location. *)

(** # Construction *)

val make : kind:kind -> span:Ceibo.Span.t -> t
(** `make ~kind ~span` creates a diagnostic.
    
    Example:
    ```ocaml
    let diag = Diagnostic.make
      ~kind:(MissingToken { expected = ")" })
      ~span:(Ceibo.Span.make ~start:10 ~end_:10)
    ```
*)

(** ## Diagnostic Constructors

    These helpers create specific diagnostic types with helpful hints. *)

val malformed_type_variable :
  found:Token.t -> text:string -> span:Ceibo.Span.t -> t
(** Create a "malformed type variable" diagnostic.

    Example: ```ocaml Diagnostic.malformed_type_variable ~found:token ~text:" "
    ~span:error_span ``` *)

val missing_let_binding_pattern :
  found:Token.t -> text:string -> span:Ceibo.Span.t -> t
(** Create a "missing let binding pattern" diagnostic.

    Example: ```ocaml Diagnostic.missing_let_binding_pattern ~found:equals_token
    ~text:"=" ~span:error_span ``` *)

val missing_let_binding_equals :
  found:Token.t -> text:string -> span:Ceibo.Span.t -> t
(** Create a "missing let binding equals" diagnostic.

    Example: ```ocaml Diagnostic.missing_let_binding_equals ~found:int_token
    ~text:"42" ~span:error_span ``` *)

val missing_let_binding_expr :
  found:Token.t -> text:string -> span:Ceibo.Span.t -> t
(** Create a "missing let binding expression" diagnostic.

    Example: ```ocaml Diagnostic.missing_let_binding_expr ~found:eof_token
    ~text:"" ~span:error_span ``` *)

val unexpected_structure_item :
  found:Token.t -> text:string -> span:Ceibo.Span.t -> t
(** Create an "unexpected structure item" diagnostic.

    Example: ```ocaml Diagnostic.unexpected_structure_item ~found:token
    ~text:"42" ~span:error_span ``` *)

val unexpected_signature_item :
  found:Token.t -> text:string -> span:Ceibo.Span.t -> t
(** Create an "unexpected signature item" diagnostic.

    Example: ```ocaml Diagnostic.unexpected_signature_item ~found:token
    ~text:"42" ~span:error_span ``` *)

val invalid_pattern : found:Token.t -> text:string -> span:Ceibo.Span.t -> t
val invalid_expression : found:Token.t -> text:string -> span:Ceibo.Span.t -> t
val invalid_constant : found:Token.t -> text:string -> span:Ceibo.Span.t -> t

val invalid_type_expression :
  found:Token.t -> text:string -> span:Ceibo.Span.t -> t

val missing_let_keyword : found:Token.t -> text:string -> span:Ceibo.Span.t -> t

val missing_type_keyword :
  found:Token.t -> text:string -> span:Ceibo.Span.t -> t

val missing_type_name : found:Token.t -> text:string -> span:Ceibo.Span.t -> t

val missing_type_decl_equals :
  found:Token.t -> text:string -> span:Ceibo.Span.t -> t

val unclosed_delimiter :
  opener:string -> found:Token.t -> text:string -> span:Ceibo.Span.t -> t

val unclosed_type_params :
  found:Token.t -> text:string -> span:Ceibo.Span.t -> t

val empty_char_literal : span:Ceibo.Span.t -> t
val multi_char_literal : text:string -> span:Ceibo.Span.t -> t
val unclosed_char_literal : text:string -> span:Ceibo.Span.t -> t

val missing_binary_operand :
  operator:string ->
  side:string ->
  found:Token.t ->
  text:string ->
  span:Ceibo.Span.t ->
  t

val consecutive_binary_operators :
  operators:string -> found:Token.t -> text:string -> span:Ceibo.Span.t -> t

val invalid_type_parameter :
  text:string -> found:Token.t -> text_found:string -> span:Ceibo.Span.t -> t

val uppercase_type_variable :
  text:string -> found:Token.t -> text_found:string -> span:Ceibo.Span.t -> t

val uppercase_type_name :
  text:string -> found:Token.t -> text_found:string -> span:Ceibo.Span.t -> t

val bracketed_type_parameters :
  type_name:string -> found:Token.t -> text:string -> span:Ceibo.Span.t -> t

val list_double_semicolon :
  found:Token.t -> text:string -> span:Ceibo.Span.t -> t

(** # Serialization *)

val error_id : t -> Error.id
(** `error_id diag` returns the error ID for this diagnostic. *)

val id : t -> string
(** `id diag` returns the unique error identifier string for this diagnostic. *)

val expected_message : t -> string
(** `kind_to_expected kind` returns what was expected at this position. *)

val fix_message : t -> string option
(** `kind_to_fix kind` returns a quick fix suggestion (None if none). *)

val hint_message : t -> string
(** `kind_to_hint diag` returns a detailed explanation hint. *)

val main_message : t -> string
(** `main_message diag` returns the main error message for display. *)

val found_token : t -> found_token

val to_string : t -> string
(** `to_string diag` converts a diagnostic to a human-readable error message.

    Example output: ``` Missing token ')' at position 10 Unexpected token '+'
    (expected identifier) at 5..6 Invalid syntax in let binding at 12..18 ```

    Example: ```ocaml let msg = Diagnostic.to_string diag in Printf.eprintf
    "Error: %s\n" msg ``` *)

val to_json : t -> Data.Json.t
(** `to_json diag` converts a diagnostic to structured JSON.
    
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
