(** Common types and utilities for HTTP/1.1 parsing *)
open Std

type 'a parse_result =
  | Done of { value: 'a; remaining: string }
  | Need_more
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

let uri_error_to_string = function
  | Std.Net.Uri.InvalidScheme -> "invalid scheme"
  | Std.Net.Uri.InvalidAuthority -> "invalid authority"
  | Std.Net.Uri.InvalidPath -> "invalid path"
  | Std.Net.Uri.InvalidQuery -> "invalid query"
  | Std.Net.Uri.InvalidFragment -> "invalid fragment"
  | Std.Net.Uri.InvalidFormat -> "invalid format"
  | Std.Net.Uri.TooLong -> "too long"

let header_format_error_to_string = function
  | MissingColon -> "missing colon"
  | MissingValueSeparator -> "missing value separator"

let error_to_string = function
  | InvalidCrlf -> "Invalid CRLF"
  | RequestLineTooLong { max_length } ->
      "Request line too long (max " ^ Int.to_string max_length ^ " bytes)"
  | MissingMethod -> "Missing method"
  | MissingPath -> "Missing path"
  | InvalidHttpVersion -> "Invalid HTTP version"
  | InvalidRequestTarget error -> "Invalid request target: " ^ uri_error_to_string error
  | MissingVersion -> "Missing version"
  | MissingStatusCode -> "Missing status code"
  | InvalidStatusCode -> "Invalid status code"
  | InvalidHeaderFormat error ->
      "Invalid header format (" ^ header_format_error_to_string error ^ ")"
  | HeaderTooLong { max_length } -> "Header too long (max " ^ Int.to_string max_length ^ " bytes)"
  | TooManyHeaders { max_count } -> "Too many headers (max " ^ Int.to_string max_count ^ ")"
  | InvalidContentLength -> "Invalid Content-Length"
  | ConflictingContentLength -> "Conflicting Content-Length"
  | UnsupportedTransferEncoding -> "Unsupported Transfer-Encoding"
  | TransferEncodingWithContentLength -> "Invalid body framing: Transfer-Encoding with Content-Length"
  | InvalidChunkSizeLineEnding -> "Invalid chunk size line ending"
  | InvalidChunkSize -> "Invalid chunk size"

(** Helper: Find substring in string *)
let find_substring = fun ~needle haystack ->
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec search pos =
    if pos + needle_len > haystack_len then
      None
    else if String.sub haystack ~offset:pos ~len:needle_len = needle then
      Some pos
    else
      search (pos + 1)
  in
  search 0

(** Helper: Split string at position *)
let split_at = fun str pos ->
  let left = String.sub str ~offset:0 ~len:pos in
  let right = String.sub str ~offset:pos ~len:(String.length str - pos) in
  (left, right)
