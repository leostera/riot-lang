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
  | StatusLineTooLong of { max_length: int }
  | MissingMethod
  | MissingPath
  | InvalidHttpVersion
  | InvalidRequestTarget of Std.Net.Uri.error
  | MissingVersion
  | MissingStatusCode
  | InvalidStatusCode
  | InvalidHeaderFormat of header_format_error
  | HeaderTooLong of { max_length: int }
  | HeaderBlockTooLong of { max_length: int }
  | TooManyHeaders of { max_count: int }
  | InvalidContentLength
  | ConflictingContentLength
  | UnsupportedTransferEncoding
  | TransferEncodingWithContentLength
  | InvalidChunkSizeLineEnding
  | InvalidChunkDataLineEnding
  | InvalidChunkSize
  | InvalidChunkExtensionCharacter of { code: int; index: int }
  | ChunkTooLarge of { size: int; max_size: int }
  | ChunkedBodyTooLarge of { size: int; max_size: int }

and header_format_error =
  | MissingColon
  | MissingValueSeparator
  | EmptyName
  | WhitespaceBeforeColon
  | ObsoleteLineFolding
  | InvalidNameCharacter of { code: int; index: int }
  | InvalidValueCharacter of { code: int; index: int }

(** Render a parse error for logs, diagnostics, or tests. *)
val error_to_string: error -> string

val validate_header_name: string -> (unit, header_format_error) Result.t

val validate_header_value: string -> (unit, header_format_error) Result.t

val find_substring: needle:string -> string -> int option

val split_at: string -> int -> string * string
