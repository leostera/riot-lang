open Global

type env = Dev | Test | Prod

let detect_env () =
  match Kernel.Env.getenv "RIOT_ENV" with
  | Some "test" -> Test
  | Some "prod" -> Prod
  | Some "production" -> Prod
  | _ -> Dev

let env_to_string = function
  | Dev -> "dev"
  | Test -> "test"
  | Prod -> "prod"

let config_path env =
  "./config/" ^ env_to_string env ^ ".toml"

let load_file path =
  match Path.of_string path with
  | Error _ -> Error ("Invalid path: " ^ path)
  | Ok path_t ->
      if not (Path.exists path_t) then
        Error ("File not found: " ^ path)
      else
        match Fs.read path_t with
        | Error err -> Error ("Failed to read file: " ^ Kernel.IO.error_message err)
        | Ok contents ->
            match Data.Toml.parse contents with
            | Error toml_err -> Error ("TOML parse error: " ^ Data.Toml.error_to_string toml_err)
            | Ok value -> Ok value

let load_for_env env =
  let path = config_path env in
  load_file path

let rec extract_app_section app_name toml =
  match String.split_on_char '.' app_name with
  | [] -> Error "Empty app name"
  | [single] -> 
      (* Single key - original behavior *)
      (match Data.Toml.get_table toml with
      | None -> Error "Root is not a table"
      | Some fields ->
          match Collections.List.assoc_opt single fields with
          | None -> Error ("No [" ^ single ^ "] section found")
          | Some section -> Ok section)
  | first :: rest ->
      (* Dotted path - navigate recursively *)
      (match Data.Toml.get_table toml with
      | None -> Error "Root is not a table"
      | Some fields ->
          match Collections.List.assoc_opt first fields with
          | None -> Error ("No [" ^ first ^ "] section found")
          | Some next_table -> 
              extract_app_section (String.concat "." rest) next_table)
