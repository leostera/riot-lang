open Std
open Std.Collections

type format_error =
  | Cannot_parse of Syn.Diagnostic.t Vector.t
  | Cannot_render of string

val format_error_to_string: format_error -> string

type write_error =
  | Format_failed of format_error
  | Write_failed of IO.error

val parse_source: filename:Path.t -> string -> Syn.Parser.parse_result

val format: Syn.Parser.parse_result -> (string, format_error) result

val format_source: filename:Path.t -> string -> (string, format_error) result

val stream_format:
  Syn.Parser.parse_result ->
  writer:IO.Writer.t ->
  width:int ->
  (unit, write_error) result

val stream_format_to_string: Syn.Parser.parse_result -> width:int -> (string, format_error) result

val write: writer:IO.Writer.t -> Syn.Parser.parse_result -> (unit, write_error) result
