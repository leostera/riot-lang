open Std

type file_result = {
  file : Path.t;
  needs_formatting : bool;
  error : string option;
  duration : Time.Duration.t;
}

type summary = {
  total_files : int;
  already_formatted : int;
  needs_formatting : int;
  failed_files : int;
  duration : Time.Duration.t;
}

type run_result = { files : file_result list; summary : summary }

val collect_ocaml_files :
  ?should_ignore:(Path.t -> bool) ->
  roots:Path.t list ->
  unit ->
  Path.t list

val check_file : Path.t -> file_result

val run_checks_streaming :
  ?concurrency:int ->
  ?should_ignore:(Path.t -> bool) ->
  roots:Path.t list ->
  on_result:(file_result -> unit) ->
  unit ->
  run_result

val run_checks : ?concurrency:int -> ?should_ignore:(Path.t -> bool) -> Path.t list -> run_result
