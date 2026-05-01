open Global
open Collections

type env =
  | Dev
  | Test
  | Prod

let detect_env = fun () ->
  match Env.get Env.String ~var:"RIOT_ENV" with
  | Some "test" -> Test
  | Some "prod" -> Prod
  | Some "production" -> Prod
  | _ -> Dev

let env_to_string = fun __tmp1 ->
  match __tmp1 with
  | Dev -> "dev"
  | Test -> "test"
  | Prod -> "prod"

let config_path = fun env -> "./config/" ^ env_to_string env ^ ".toml"

let load_file = fun path ->
  match Path.from_string path with
  | Error _ -> Error ("Invalid path: " ^ path)
  | Ok path_t ->
      if not (Path.exists path_t) then
        Error ("File not found: " ^ path)
      else
        match Fs.read path_t with
        | Error err -> Error ("Failed to read file: " ^ IO.error_message err)
        | Ok contents ->
            match Data.Toml.parse contents with
            | Error toml_err -> Error ("TOML parse error: " ^ Data.Toml.error_to_string toml_err)
            | Ok value -> Ok value

let load_for_env = fun env ->
  let path = config_path env in
  load_file path

let find_field = fun fields name ->
  List.find
    fields
    ~fn:(fun (field_name, _value) -> String.equal field_name name)

let rec extract_app_section = fun app_name toml ->
  match String.split ~by:"." app_name with
  | [] -> Error "Empty app name"
  | [ single ] -> (
      match Data.Toml.get_table toml with
      | None -> Error "Root is not a table"
      | Some fields ->
          match find_field fields single with
          | None -> Error ("No [" ^ single ^ "] section found")
          | Some (_, section) -> Ok section
    )
  | first :: rest -> (
      match Data.Toml.get_table toml with
      | None -> Error "Root is not a table"
      | Some fields ->
          match find_field fields first with
          | None -> Error ("No [" ^ first ^ "] section found")
          | Some (_, next_table) -> extract_app_section (String.concat "." rest) next_table
    )
