(** Common types and utilities for HTTP/1.1 parsing *)
open Std

type 'a parse_result =
  | Done of { value: 'a; remaining: string }
  (** Successfully parsed + remaining input *)
  | Need_more
  (** Need more data to continue parsing *)
  | Error of error

and error =
  | InvalidCrlf
  | RequestLineTooLong of { max_length: int }
  | MissingMethod
  | MissingPath
  | InvalidHttpVersion
  | InvalidRequestTarget of Std.Net.Uri.error
  | MissingVersion
  | MissingStatusCode
  | InvalidStatusCode
  | InvalidHeaderFormat of header_format_error
  | HeaderTooLong of { max_length: int }
  | TooManyHeaders of { max_count: int }
  | InvalidContentLength
  | ConflictingContentLength
  | UnsupportedTransferEncoding
  | TransferEncodingWithContentLength
  | InvalidChunkSizeLineEnding
  | InvalidChunkSize

and header_format_error =
  | MissingColon
  | MissingValueSeparator

(** Render a parse error for logs, diagnostics, or tests. *)
val error_to_string: error -> string

val find_substring: needle:string -> string -> int option

val split_at: string -> int -> string * string
