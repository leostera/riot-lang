(** Filesystem utilities *)

type error = SystemError of string

let create_dir path =
  let path_str = Path.to_string path in
  try
    if not (Sys.file_exists path_str) then Unix.mkdir path_str 0o755;
    Ok ()
  with e -> Error (SystemError (Printexc.to_string e))
