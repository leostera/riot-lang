(** Common types and utilities for HTTP/1.1 parsing *)
open Std

type 'a parse_result =
  | Done of { value: 'a; remaining: string }
  | Need_more
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
  | ChunkSizeLineTooLong of { max_length: int }
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
  | EmptyName -> "empty header name"
  | WhitespaceBeforeColon -> "whitespace before colon"
  | ObsoleteLineFolding -> "obsolete line folding"
  | InvalidNameCharacter { code; index } ->
      "invalid header name character code "
      ^ Int.to_string code
      ^ " at index "
      ^ Int.to_string index
  | InvalidValueCharacter { code; index } ->
      "invalid header value character code "
      ^ Int.to_string code
      ^ " at index "
      ^ Int.to_string index

let error_to_string = function
  | InvalidCrlf -> "Invalid CRLF"
  | RequestLineTooLong { max_length } ->
      "Request line too long (max " ^ Int.to_string max_length ^ " bytes)"
  | StatusLineTooLong { max_length } ->
      "Status line too long (max " ^ Int.to_string max_length ^ " bytes)"
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
  | HeaderBlockTooLong { max_length } ->
      "Header block too long (max " ^ Int.to_string max_length ^ " bytes)"
  | TooManyHeaders { max_count } -> "Too many headers (max " ^ Int.to_string max_count ^ ")"
  | InvalidContentLength -> "Invalid Content-Length"
  | ConflictingContentLength -> "Conflicting Content-Length"
  | UnsupportedTransferEncoding -> "Unsupported Transfer-Encoding"
  | TransferEncodingWithContentLength -> "Invalid body framing: Transfer-Encoding with Content-Length"
  | InvalidChunkSizeLineEnding -> "Invalid chunk size line ending"
  | InvalidChunkDataLineEnding -> "Invalid chunk data line ending"
  | ChunkSizeLineTooLong { max_length } ->
      "Chunk size line too long (max " ^ Int.to_string max_length ^ " bytes)"
  | InvalidChunkSize -> "Invalid chunk size"
  | InvalidChunkExtensionCharacter { code; index } ->
      "Invalid chunk extension character code "
      ^ Int.to_string code
      ^ " at index "
      ^ Int.to_string index
  | ChunkTooLarge { size; max_size } ->
      "Chunk size " ^ Int.to_string size ^ " exceeds maximum " ^ Int.to_string max_size
  | ChunkedBodyTooLarge { size; max_size } ->
      "Chunked body size " ^ Int.to_string size ^ " exceeds maximum " ^ Int.to_string max_size

let is_tchar = fun c ->
  let code = Char.to_int c in
  (code >= Char.to_int '0' && code <= Char.to_int '9')
  || (code >= Char.to_int 'A' && code <= Char.to_int 'Z')
  || (code >= Char.to_int 'a' && code <= Char.to_int 'z')
  || c = '!'
  || c = '#'
  || c = '$'
  || c = '%'
  || c = '&'
  || c = '\''
  || c = '*'
  || c = '+'
  || c = '-'
  || c = '.'
  || c = '^'
  || c = '_'
  || c = '`'
  || c = '|'
  || c = '~'

let validate_header_name = fun name ->
  if String.length name = 0 then
    Result.Error EmptyName
  else
    let rec loop index =
      if index >= String.length name then
        Result.Ok ()
      else
        let c = String.get_unchecked name ~at:index in
        if c = ' ' || c = '\t' then
          Result.Error WhitespaceBeforeColon
        else if is_tchar c then
          loop (index + 1)
        else
          Result.Error (InvalidNameCharacter { code = Char.to_int c; index })
    in
    loop 0

let validate_header_value = fun value ->
  let rec loop index =
    if index >= String.length value then
      Result.Ok ()
    else
      let c = String.get_unchecked value ~at:index in
      let code = Char.to_int c in
      if c = '\t' || (code >= 0x20 && code <= 0x7e) || code >= 0x80 then
        loop (index + 1)
      else
        Result.Error (InvalidValueCharacter { code; index })
  in
  loop 0

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
