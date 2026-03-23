let daemon_exists x =
  match (Fs.exists pid_file, Fs.exists port_file) with
  | Ok true, Ok true -> Some ()
  | _ -> None
