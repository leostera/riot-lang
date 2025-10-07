open Std

(** Structured parse error kinds *)
type kind =
  | MissingToken of { expected : string }
  | UnexpectedToken of { expected : string option; found : string }
  | UnexpectedEof of { expected : string }
  | InvalidSyntax of { context : string }
  | UnclosedDelimiter of { delimiter : string; opened_at : int }
  | MismatchedDelimiter of { expected : string; found : string }

(** Parse error information *)
type t = {
  kind : kind;
  span : Ceibo.Span.t;
}

val make : kind:kind -> span:Ceibo.Span.t -> t

(** Convenience constructors for common error types *)

val make_missing_token : expected:string -> span:Ceibo.Span.t -> t
val make_unexpected_token : expected:string option -> found:string -> span:Ceibo.Span.t -> t
val make_unexpected_eof : expected:string -> span:Ceibo.Span.t -> t
val make_invalid_syntax : context:string -> span:Ceibo.Span.t -> t
val make_unclosed_delimiter : delimiter:string -> opened_at:int -> span:Ceibo.Span.t -> t
val make_mismatched_delimiter : expected:string -> found:string -> span:Ceibo.Span.t -> t

(** Convert error to human-readable string *)
val to_string : t -> string

(** Convert error to JSON for machine consumption *)
val to_json : t -> Data.Json.t
