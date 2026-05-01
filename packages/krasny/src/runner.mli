open Std

type run_mode =
  | Check
  | Verify
  | Format
type file_status =
  | Already_formatted
  | Needs_formatting
  | Would_reformat
  | Unsafe_to_format
  | Formatted
  | Failed
type file_result = {
  file: Path.t;
  status: file_status;
  needs_formatting: bool;
  error: string option;
  diagnostics: Syn.Diagnostic.t list option;
  duration: Time.Duration.t;
}
type summary = {
  total_files: int;
  already_formatted: int;
  needs_formatting: int;
  would_reformat: int;
  unsafe_to_format: int;
  formatted_files: int;
  failed_files: int;
  duration: Time.Duration.t;
}
type run_result = {
  files: file_result list;
  summary: summary;
}

val syntax_hash: Syn.Parser.parse_result -> string

val collect_ocaml_files: ?should_ignore:(Path.t -> bool) -> roots:Path.t list -> unit -> Path.t list

val check_file: Path.t -> file_result

val verify_file: Path.t -> file_result

val run_checks_streaming:
  ?concurrency:int ->
  ?should_ignore:(Path.t -> bool) ->
  roots:Path.t list ->
  on_result:(file_result -> unit) ->
  unit ->
  run_result

val run_verify_streaming:
  ?concurrency:int ->
  ?should_ignore:(Path.t -> bool) ->
  roots:Path.t list ->
  on_result:(file_result -> unit) ->
  unit ->
  run_result

val run_format_streaming:
  ?concurrency:int ->
  ?should_ignore:(Path.t -> bool) ->
  roots:Path.t list ->
  on_result:(file_result -> unit) ->
  unit ->
  run_result

val run_checks: ?concurrency:int -> ?should_ignore:(Path.t -> bool) -> Path.t list -> run_result

val run_verify: ?concurrency:int -> ?should_ignore:(Path.t -> bool) -> Path.t list -> run_result

val run_format: ?concurrency:int -> ?should_ignore:(Path.t -> bool) -> Path.t list -> run_result
