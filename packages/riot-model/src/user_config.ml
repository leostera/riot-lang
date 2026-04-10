open Std
open Std.Data

type registry = {
  api_url: Net.Uri.t;
  cdn_url: Net.Uri.t;
  api_token: string option;
}

type t = {
  registries: (string * registry) list;
}

type error =
  | ReadFailed of { path: Path.t; error: string }
  | ParseFailed of { path: Path.t; error: string }
  | WriteFailed of { path: Path.t; error: string }
  | InvalidConfig of { error: string }
  | InvalidRegistryConfig of { registry_name: string; error: string }

let ( let* ) = Result.and_then

let empty = { registries = [] }

let default = {
  registries = [
    (
      "pkgs.ml",
      {
        api_url = Net.Uri.of_string "https://api.pkgs.ml" |> Result.unwrap;
        cdn_url = Net.Uri.of_string "https://cdn.pkgs.ml" |> Result.unwrap;
        api_token = None
      }
    )
  ]
}

let message = function
  | ReadFailed { path; error } -> "failed to read config '" ^ Path.to_string path ^ "': " ^ error
  | ParseFailed { path; error } -> "failed to parse config '" ^ Path.to_string path ^ "': " ^ error
  | WriteFailed { path; error } -> "failed to write config '" ^ Path.to_string path ^ "': " ^ error
  | InvalidConfig { error } -> "invalid config: " ^ error
  | InvalidRegistryConfig { registry_name; error } -> "invalid registry config for '"
  ^ registry_name
  ^ "': "
  ^ error

let uri_error_message = function
  | Net.Uri.InvalidScheme -> "invalid scheme"
  | Net.Uri.InvalidAuthority -> "invalid authority"
  | Net.Uri.InvalidPath -> "invalid path"
  | Net.Uri.InvalidQuery -> "invalid query"
  | Net.Uri.InvalidFragment -> "invalid fragment"
  | Net.Uri.InvalidFormat -> "invalid format"
  | Net.Uri.TooLong -> "uri too long"

let default_api_url = fun ~registry_name ->
  Net.Uri.of_string ("https://api." ^ registry_name)
  |> Result.map_error
    (fun error ->
      InvalidRegistryConfig {
        registry_name;
        error = "invalid default api_url: " ^ uri_error_message error
      })

let default_cdn_url = fun ~registry_name ->
  Net.Uri.of_string ("https://cdn." ^ registry_name)
  |> Result.map_error
    (fun error ->
      InvalidRegistryConfig {
        registry_name;
        error = "invalid default cdn_url: " ^ uri_error_message error
      })

let default_registry = fun ~registry_name ->
  let* api_url = default_api_url ~registry_name in
  let* cdn_url = default_cdn_url ~registry_name in
  Ok { api_url; cdn_url; api_token = None }

let registry_of_toml = fun ~registry_name value ->
  match value with
  | Toml.Table fields ->
      let* defaults = default_registry ~registry_name in
      let* api_url =
        match List.assoc_opt "api_url" fields with
        | None -> Ok defaults.api_url
        | Some (Toml.String url) -> Net.Uri.of_string url
        |> Result.map_error
          (fun error ->
            InvalidRegistryConfig {
              registry_name;
              error = "field 'api_url' must be a valid URI: " ^ uri_error_message error
            })
        | Some _ -> Error (InvalidRegistryConfig {
          registry_name;
          error = "field 'api_url' must be a string"
        })
      in
      let* cdn_url =
        match List.assoc_opt "cdn_url" fields with
        | None -> Ok defaults.cdn_url
        | Some (Toml.String url) -> Net.Uri.of_string url
        |> Result.map_error
          (fun error ->
            InvalidRegistryConfig {
              registry_name;
              error = "field 'cdn_url' must be a valid URI: " ^ uri_error_message error
            })
        | Some _ -> Error (InvalidRegistryConfig {
          registry_name;
          error = "field 'cdn_url' must be a string"
        })
      in
      let api_token =
        match List.assoc_opt "api_token" fields with
        | None -> Ok None
        | Some (Toml.String token) -> Ok (Some token)
        | Some _ -> Error (InvalidRegistryConfig {
          registry_name;
          error = "field 'api_token' must be a string"
        })
      in
      Result.map (fun api_token -> { api_url; cdn_url; api_token }) api_token
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
  let has_nested_tables =
    List.exists
      (fun (_name, value) ->
        match value with
        | Toml.Table _ -> true
        | _ -> false)
      fields
  in
  if List.length path > 0 && (has_registry_fields || not has_nested_tables) then
    let registry_name = String.concat "." (List.rev path) |> normalize_registry_name in
    match registry_of_toml ~registry_name (Toml.Table fields) with
    | Ok registry -> Ok ((registry_name, registry) :: acc)
    | Error _ as err -> err
  else
    let rec loop acc = function
      | [] ->
          Ok acc
      | (name, Toml.Table nested_fields) :: rest -> (
          match collect_registries ~path:(name :: path) acc nested_fields with
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

let render_string = fun value -> Toml.to_string (Toml.String value)

let to_string = fun config ->
  let render_registry (registry_name, registry) =
    let header = "[registry." ^ render_string registry_name ^ "]" in
    let fields = [
      "api_url = " ^ render_string (Net.Uri.to_string registry.api_url);
      "cdn_url = " ^ render_string (Net.Uri.to_string registry.cdn_url);
    ] in
    match registry.api_token with
    | Some token -> String.concat "\n" (header :: fields @ [ "api_token = " ^ render_string token ])
    | None -> String.concat "\n" (header :: fields)
  in
  config.registries |> List.map render_registry |> String.concat "\n\n"

let save = fun config path ->
  Fs.write (to_string config) path
  |> Result.map_error (fun io_error -> WriteFailed { path; error = IO.error_message io_error })

let upsert_registry = fun config ~registry_name ~update ->
  let rec loop acc = function
    | [] -> (
        match default_registry ~registry_name with
        | Ok registry -> List.rev ((registry_name, update registry) :: acc)
        | Error _ -> List.rev acc
      )
    | (name, registry) :: rest when String.equal name registry_name ->
        List.rev_append acc ((name, update registry) :: rest)
    | entry :: rest ->
        loop (entry :: acc) rest
  in
  { registries = loop [] config.registries }

let api_token = fun config ~registry_name ->
  match
    List.find_opt
      (fun (name, _registry) ->
        String.equal name registry_name)
      config.registries
  with
  | None -> None
  | Some (_name, registry) -> registry.api_token

let set_api_token = fun config ~registry_name token ->
  upsert_registry
    config
    ~registry_name
    ~update:(fun registry -> { registry with api_token = Some token })

let clear_api_token = fun config ~registry_name ->
  upsert_registry config ~registry_name ~update:(fun registry -> { registry with api_token = None })
