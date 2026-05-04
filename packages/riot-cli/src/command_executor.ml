open Std

let array_of_list = fun lst -> Array.from_list lst

let execute = fun ~command_binary ~args ->
  let command_path = Path.to_string command_binary in
  (* Check if binary exists *)
  match Fs.exists command_binary with
  | Ok false ->
      Log.error ("Command binary not found: " ^ command_path);
      Error (Failure ("Command binary not found: " ^ command_path))
  | Error _ ->
      Log.error "Failed to check if command exists";
      Error (Failure "Failed to check if command exists")
  | Ok true ->
      (* Execute the binary, replacing current process *)
      let args_list = command_path :: args in
      let argv = array_of_list args_list in
      (
        match Process.execv ~program:command_path ~args:argv with
        | Ok () -> Error (Failure "execv returned unexpectedly")
        | Error error ->
            Error (Failure ("Failed to exec command: "
            ^ IO.error_message (IO.from_system_error error)))
      )
