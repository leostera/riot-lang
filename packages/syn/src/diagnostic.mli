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

type kind =
  | MissingToken of { expected : string }
      (** Expected a specific token but found something else or EOF.

          Example: Missing `)` in `let x = (1 + 2` *)
  | UnexpectedToken of { expected : string option; found : string }
      (** Found a token that doesn't fit the current context.

          Example: `let x + 1` - `+` is unexpected after identifier *)
  | UnexpectedEof of { expected : string }
      (** Reached end of file while expecting more input.

          Example: `let x =` with nothing after `=` *)
  | InvalidSyntax of { context : string }
      (** Syntactically invalid construct.

          Example: Malformed pattern, invalid expression form *)
  | UnclosedDelimiter of { delimiter : string; opened_at : int }
      (** Opening delimiter without matching closing delimiter.

          Example: `begin ... (* missing end *)` *)
  | MismatchedDelimiter of { expected : string; found : string }
      (** Closing delimiter doesn't match opening delimiter.

          Example: `begin ... end)` - `end` expected but `)` found *)

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

(** ## Convenience Constructors

    These helpers create diagnostics for common error types without needing to
    construct the `kind` variant manually. *)

val make_missing_token : expected:string -> span:Ceibo.Span.t -> t
(** Create a "missing token" diagnostic.

    Example: ```ocaml Diagnostic.make_missing_token ~expected:")"
    ~span:error_span ``` *)

val make_unexpected_token :
  expected:string option -> found:string -> span:Ceibo.Span.t -> t
(** Create an "unexpected token" diagnostic.

    Example: ```ocaml Diagnostic.make_unexpected_token ~expected:(Some
    "identifier") ~found:"+" ~span:token_span ``` *)

val make_unexpected_eof : expected:string -> span:Ceibo.Span.t -> t
(** Create an "unexpected end of file" diagnostic.

    Example: ```ocaml Diagnostic.make_unexpected_eof ~expected:"expression"
    ~span:eof_span ``` *)

val make_invalid_syntax : context:string -> span:Ceibo.Span.t -> t
(** Create an "invalid syntax" diagnostic.

    Example: ```ocaml Diagnostic.make_invalid_syntax ~context:"let binding"
    ~span:binding_span ``` *)

val make_unclosed_delimiter :
  delimiter:string -> opened_at:int -> span:Ceibo.Span.t -> t
(** Create an "unclosed delimiter" diagnostic.

    Example: ```ocaml Diagnostic.make_unclosed_delimiter ~delimiter:"("
    ~opened_at:start_pos ~span:current_span ``` *)

val make_mismatched_delimiter :
  expected:string -> found:string -> span:Ceibo.Span.t -> t
(** Create a "mismatched delimiter" diagnostic.

    Example: ```ocaml Diagnostic.make_mismatched_delimiter ~expected:"end"
    ~found:")" ~span:delimiter_span ``` *)

(** # Serialization *)

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
