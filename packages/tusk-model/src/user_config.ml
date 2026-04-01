open Std
open Std.Data

type registry = {
  api_token: string option;
}

type t = {
  registries: (string * registry) list;
}

type error =
  | ReadFailed of { path: Path.t; error: string }
  | ParseFailed of { path: Path.t; error: string }
  | InvalidConfig of { error: string }
  | InvalidRegistryConfig of { registry_name: string; error: string }

let empty = { registries = [] }

let message = function
  | ReadFailed { path; error } -> "failed to read config '" ^ Path.to_string path ^ "': " ^ error
  | ParseFailed { path; error } -> "failed to parse config '" ^ Path.to_string path ^ "': " ^ error
  | InvalidConfig { error } -> "invalid config: " ^ error
  | InvalidRegistryConfig { registry_name; error } -> "invalid registry config for '"
  ^ registry_name
  ^ "': "
  ^ error

let registry_of_toml = fun ~registry_name value ->
  match value with
  | Toml.Table fields ->
      let api_token =
        match List.assoc_opt "api_token" fields with
        | None -> Ok None
        | Some (Toml.String token) -> Ok (Some token)
        | Some _ -> Error (InvalidRegistryConfig {
          registry_name;
          error = "field 'api_token' must be a string"
        })
      in
      Result.map (fun api_token -> { api_token }) api_token
  | _ -> Error (InvalidRegistryConfig { registry_name; error = "registry entry must be a table" })

let normalize_registry_name = fun name ->
  let len = String.length name in
  if len >= 2 && Char.equal (String.get name 0) '"' && Char.equal (String.get name (len - 1)) '"' then
    String.sub name 1 (len - 2)
  else
    name

let rec collect_registries = fun ~path acc fields ->
  let has_registry_fields =
    List.exists
      (fun (name, _value) ->
        String.equal name "api_token")
      fields
  in
  if has_registry_fields then
    let registry_name = String.concat "." (List.rev path) |> normalize_registry_name in
    match registry_of_toml ~registry_name (Toml.Table fields) with
    | Ok registry -> Ok ((registry_name, registry) :: acc)
    | Error _ as err -> err
  else
    let rec loop acc = function
      | [] ->
          Ok acc
      | (name, Toml.Table nested_fields) :: rest -> (
          match collect_registries ~path:((name :: path)) acc nested_fields with
          | Ok acc -> loop acc rest
          | Error _ as err -> err
        )
      | _ :: rest ->
          loop acc rest
    in
    loop acc fields

let of_toml = fun value ->
  match value with
  | Toml.Table fields -> (
      match List.assoc_opt "registry" fields with
      | None -> Ok empty
      | Some (Toml.Table registry_fields) -> collect_registries ~path:[] [] registry_fields
      |> Result.map (fun registries -> { registries = List.rev registries })
      | Some _ -> Error (InvalidConfig { error = "top-level 'registry' entry must be a table" })
    )
  | _ -> Ok empty

let load = fun path ->
  match Fs.read_to_string path with
  | Error io_error -> Error (ReadFailed { path; error = IO.error_message io_error })
  | Ok source -> (
      match Toml.parse source with
      | Error parse_error -> Error (ParseFailed { path; error = Toml.error_to_string parse_error })
      | Ok toml -> (
          match of_toml toml with
          | Ok config -> Ok config
          | Error (InvalidConfig { error }) -> Error (ParseFailed { path; error })
          | Error _ as err -> err
        )
    )

let api_token = fun config ~registry_name ->
  match
    List.find_opt
      (fun (name, _registry) ->
        String.equal name registry_name)
      config.registries
  with
  | None -> None
  | Some (_name, registry) -> registry.api_token
