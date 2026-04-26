open Std

type t = { rows: int; cols: int }

let get = fun () ->
  match Platform.get_size (Platform.stdout_fd ()) with
  | Ok (cols, rows) -> Ok { rows; cols }
  | Error error -> Error (`System_error (IO.error_message error))

let to_string = fun { rows; cols } ->
  "{ rows = " ^ Int.to_string rows ^ "; cols = " ^ Int.to_string cols ^ " }"
