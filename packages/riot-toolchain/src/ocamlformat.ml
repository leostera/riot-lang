open Std

type t = Path.t

let make = fun path -> path

let path = fun t -> t

type format_result =
  | Formatted of { code: string; changed: bool }
  | Error of string

let find_ocamlformat_config = fun path ->
  let rec search dir =
    match Path.of_string ".ocamlformat" with
    | Error _ -> None
    | Ok ocamlformat_name -> (
        let ocamlformat_file = Path.join dir ocamlformat_name in
        if Fs.exists ocamlformat_file |> Result.unwrap_or ~default:false then
          Some ocamlformat_file
        else
          match Path.parent dir with
          | None -> None
          | Some parent ->
              if Path.equal parent dir then
                None
              else
                search parent
      )
  in
  search
    (
      if Fs.is_dir path |> Result.unwrap_or ~default:false then
        path
      else
        match Path.parent path with
        | None -> path
        | Some parent -> parent
    )

let format_file = fun t ~file_path ~check_only ->
  let file_str = Path.to_string file_path in
  if not
      (Fs.exists file_path |> Result.unwrap_or ~default:false) then
    Error ("File not found: " ^ file_str)
  else if not
      (file_str |> String.ends_with ~suffix:".ml" || file_str |> String.ends_with ~suffix:".mli") then
    Error ("Not an OCaml file: " ^ file_str)
  else
    let check_flag =
      if check_only then
        "--check"
      else
        "--inplace"
    in
    let ocamlformat_bin = Path.to_string t in
    let cmd_str = ocamlformat_bin ^ " " ^ check_flag ^ " " ^ file_str in
    let cmd = Command.make ~args:[ "-c"; cmd_str ] "sh" in
    match Command.output cmd with
    | Ok output when output.Command.status = 0 -> (
        if check_only then
          Formatted { code = output.Command.stdout; changed = false }
        else
          (* In inplace mode, we need to read the file to get formatted content *)
          match Fs.read file_path with
          | Ok content -> Formatted { code = content; changed = true }
          | Error err -> Error ("Failed to read formatted file: " ^ IO.error_message err)
      )
    | Ok output ->
        let error_msg =
          if String.length output.Command.stderr > 0 then
            "ocamlformat failed with status "
            ^ Int.to_string output.Command.status
            ^ ": "
            ^ output.Command.stderr
          else
            "ocamlformat failed with status " ^ Int.to_string output.Command.status
        in
        Error error_msg
    | Error (Command.SystemError msg) ->
        Error msg

let format_code = fun t ~code ~file_path ->
  (* Create a temporary file with proper extension based on hint *)
  let extension =
    match file_path with
    | Some path ->
        let path_str = Path.to_string path in
        if String.ends_with ~suffix:".mli" path_str then
          ".mli"
        else
          ".ml"
    | None -> ".ml"
  in
  let temp_file = "/tmp/riot_format_" ^ Int32.to_string (Process.id ()) ^ extension in
  match Path.of_string temp_file with
  | Error _ -> Error "Failed to create temp file path"
  | Ok temp_file_path -> (
      match Fs.write code temp_file_path with
      | Error err -> Error ("Failed to write temp file: " ^ IO.error_message err)
      | Ok () ->
          let result =
            let ocamlformat_bin = Path.to_string t in
            let cmd_str = ocamlformat_bin ^ " --enable-outside-detected-project " ^ temp_file in
            let cmd = Command.make ~args:[ "-c"; cmd_str ] "sh" in
            match Command.output cmd with
            | Ok output when output.Command.status = 0 ->
                let changed = not
                  (String.equal (String.trim output.Command.stdout) (String.trim code)) in
                Formatted { code = output.Command.stdout; changed }
            | Ok output ->
                Error ("ocamlformat failed with status " ^ Int.to_string output.Command.status)
            | Error (Command.SystemError msg) ->
                Error msg
          in
          (* Clean up temp file *)
          let _ = Fs.remove_file temp_file_path in
          result
    )
