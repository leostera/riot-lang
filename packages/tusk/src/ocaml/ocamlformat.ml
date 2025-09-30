open Std
open Model

type format_result =
  | Formatted of { code : string; changed : bool }
  | Error of string

let find_ocamlformat_config path =
  let rec search dir =
    match Path.of_string ".ocamlformat" with
    | Error _ -> None
    | Ok ocamlformat_name -> (
        let ocamlformat_file = Path.join dir ocamlformat_name in
        if File.exists ~path:(Path.to_string ocamlformat_file) then
          Some ocamlformat_file
        else
          match Path.parent dir with
          | None -> None
          | Some parent -> if Path.equal parent dir then None else search parent
        )
  in
  search
    (if File.is_directory ~path:(Path.to_string path) then path
     else match Path.parent path with None -> path | Some parent -> parent)

let format_file ~toolchain ~file_path ~check_only =
  let file_str = Path.to_string file_path in

  if not (File.exists ~path:file_str) then
    Error (Printf.sprintf "File not found: %s" file_str)
  else if
    not
      (String.ends_with ~suffix:".ml" file_str
      || String.ends_with ~suffix:".mli" file_str)
  then Error (Printf.sprintf "Not an OCaml file: %s" file_str)
  else
    let check_flag = if check_only then "--check" else "--inplace" in
    let ocamlformat_bin = Path.to_string (Toolchains.ocamlformat_path toolchain) in
    let cmd = Printf.sprintf "%s %s %s" ocamlformat_bin check_flag file_str in

    match Command.run_command cmd with
    | Ok output -> (
        if check_only then
          (* In check mode, exit code 0 means no changes needed *)
          Formatted { code = output; changed = false }
        else
          (* In inplace mode, we need to read the file to get formatted content *)
          match File.read ~path:file_str with
          | Ok content -> Formatted { code = content; changed = true }
          | Error err ->
              let err_msg =
                match err with
                | `File_not_found -> "File not found"
                | `Permission_denied -> "Permission denied"
                | `Is_a_directory -> "Is a directory"
                | `Not_a_directory -> "Not a directory"
                | `Already_exists -> "Already exists"
                | `No_space -> "No space"
                | `Unknown msg -> msg
              in
              Error (Printf.sprintf "Failed to read formatted file: %s" err_msg)
        )
    | Error (Command.SpawnFailed msg) -> Error msg
    | Error (Command.CommandNotFound msg) -> Error msg

let format_code ~toolchain ~code ~file_path =
  (* Create a temporary file with proper extension based on hint *)
  let extension =
    match file_path with
    | Some path ->
        let path_str = Path.to_string path in
        if String.ends_with ~suffix:".mli" path_str then ".mli" else ".ml"
    | None -> ".ml"
  in

  let temp_file =
    Printf.sprintf "/tmp/tusk_format_%d%s" (Unix.getpid ()) extension
  in

  match File.write ~path:temp_file ~content:code with
  | Error err ->
      let err_msg =
        match err with
        | `File_not_found -> "File not found"
        | `Permission_denied -> "Permission denied"
        | `Is_a_directory -> "Is a directory"
        | `Not_a_directory -> "Not a directory"
        | `Already_exists -> "Already exists"
        | `No_space -> "No space"
        | `Unknown msg -> msg
      in
      Error (Printf.sprintf "Failed to write temp file: %s" err_msg)
  | Ok () ->
      let result =
        let ocamlformat_bin = Path.to_string (Toolchains.ocamlformat_path toolchain) in
        let cmd =
          Printf.sprintf "%s --enable-outside-detected-project %s"
            ocamlformat_bin temp_file
        in
        match Command.run_command cmd with
        | Ok output ->
            let changed =
              not (String.equal (String.trim output) (String.trim code))
            in
            Formatted { code = output; changed }
        | Error (Command.SpawnFailed msg) -> Error msg
        | Error (Command.CommandNotFound msg) -> Error msg
      in
      (* Clean up temp file *)
      let _ = Command.run_command (Printf.sprintf "rm -f %s" temp_file) in
      result
