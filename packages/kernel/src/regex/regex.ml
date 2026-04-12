open Prelude

type compile_error = {
  message: string;
  offset: int option;
}

type match_ = {
  start: int;
  stop: int;
}

type t = Regex_stubs.compiled

let compile = fun pattern ->
  match Regex_stubs.compile pattern with
  | Ok regex -> Ok regex
  | Error (message, offset) -> Error { message; offset }

let is_match = Regex_stubs.is_match

let find = fun regex haystack ->
  match Regex_stubs.find regex haystack with
  | None -> None
  | Some (start, stop) -> Some { start; stop }
