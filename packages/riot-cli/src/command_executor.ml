open Std

let array_of_list = fun lst ->
  match lst with
  | [] -> [||]
  | hd :: _ ->
      let len = List.length lst in
      let arr = Kernel.Collections.Array.make len hd in
      List.iteri
        (fun i x ->
          Kernel.Collections.Array.set arr i x)
        lst;
      arr

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
      System.execv command_path argv;
      (* execv never returns on success *)
      Error (Failure "execv returned unexpectedly")
