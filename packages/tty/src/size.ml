open Std

type t = { rows : int; cols : int }

let get () =
  try
    let ic = Unix.open_process_in "stty size 2>/dev/null" in
    let line = Stdlib.input_line ic in
    let _ = Unix.close_process_in ic in
    match String.split_on_char ' ' line with
    | [ rows; cols ] -> (
        match
          (Stdlib.int_of_string_opt rows, Stdlib.int_of_string_opt cols)
        with
        | Some rows, Some cols -> Ok { rows; cols }
        | _ -> Error (`System_error "Failed to parse terminal size"))
    | _ -> Error (`System_error "Failed to parse terminal size")
  with _ -> Error (`System_error "Failed to get terminal size")

let to_string { rows; cols } = format "{ rows = %d; cols = %d }" rows cols
