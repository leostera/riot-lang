open Std

type file_result = {
  file : Path.t;
  needs_formatting : bool;
  error : string option;
}

type summary = {
  total_files : int;
  already_formatted : int;
  needs_formatting : int;
  failed_files : int;
  duration : Time.Duration.t;
}

type run_result = { files : file_result list; summary : summary }

val collect_ocaml_files : roots:Path.t list -> Path.t list

val check_file : Path.t -> file_result

val run_checks : ?concurrency:int -> Path.t list -> run_result
