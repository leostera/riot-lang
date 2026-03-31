open Std

type t = {
  rows: int;
  cols: int;
}

let get = fun () ->
  (* Use Kernel.Terminal.get_size which handles the external call *)
  match Kernel.Terminal.get_size Kernel.IO.stdout with
  | Ok (cols, rows) ->
      Log.debug
      ("[SIZE.GET] get_size(stdout) returned cols=" ^ Int.to_string cols ^ " rows=" ^ Int.to_string rows);
      Ok {rows; cols}
  | Error _ as e ->
      Log.debug "[SIZE.GET] Failed to get terminal size from stdout";
      e

let to_string = fun ({ rows; cols }) -> "{ rows = "
^ Int.to_string rows
^ "; cols = "
^ Int.to_string cols
^ " }"
