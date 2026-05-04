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

type registry_field =
  | Api_url
  | Cdn_url
  | Api_token

type config_error =
  | RegistryMustBeTable

type registry_error =
  | InvalidDefaultUri of {
      field: registry_field;
      error: Net.Uri.error;
    }
  | InvalidUri of {
      field: registry_field;
      error: Net.Uri.error;
    }
  | FieldMustBeString of registry_field
  | RegistryEntryMustBeTable

type error =
  | ReadFailed of {
      path: Path.t;
      error: IO.error;
    }
  | ParseFailed of {
      path: Path.t;
      error: Toml.error;
    }
  | WriteFailed of {
      path: Path.t;
      error: IO.error;
    }
  | InvalidConfig of config_error
  | InvalidRegistryConfig of {
      registry_name: string;
      error: registry_error;
    }

let ( let* ) result fn = Result.and_then result ~fn

let empty = { registries = [] }

let default = {
  registries =
    [
      (
        "pkgs.ml",
        {
          api_url =
            Net.Uri.from_string "https://api.pkgs.ml"
            |> Result.unwrap;
          cdn_url =
            Net.Uri.from_string "https://cdn.pkgs.ml"
            |> Result.unwrap;
          api_token = None;
        }
      );
    ];
}

let registry_field_name = fun __tmp1 ->
  match __tmp1 with
  | Api_url -> "api_url"
  | Cdn_url -> "cdn_url"
  | Api_token -> "api_token"

let config_error_message = fun RegistryMustBeTable -> "top-level 'registry' entry must be a table"

let uri_error_message = fun __tmp1 ->
  match __tmp1 with
  | Net.Uri.InvalidScheme -> "invalid scheme"
  | Net.Uri.InvalidAuthority -> "invalid authority"
  | Net.Uri.InvalidPath -> "invalid path"
  | Net.Uri.InvalidQuery -> "invalid query"
  | Net.Uri.InvalidFragment -> "invalid fragment"
  | Net.Uri.InvalidFormat -> "invalid format"
  | Net.Uri.TooLong -> "uri too long"

let registry_error_message = fun __tmp1 ->
  match __tmp1 with
  | InvalidDefaultUri { field; error } ->
      "invalid default " ^ registry_field_name field ^ ": " ^ uri_error_message error
  | InvalidUri { field; error } ->
      "field '" ^ registry_field_name field ^ "' must be a valid URI: " ^ uri_error_message error
  | FieldMustBeString field -> "field '" ^ registry_field_name field ^ "' must be a string"
  | RegistryEntryMustBeTable -> "registry entry must be a table"

let message = fun __tmp1 ->
  match __tmp1 with
  | ReadFailed { path; error } ->
      "failed to read config '" ^ Path.to_string path ^ "': " ^ IO.error_message error
  | ParseFailed { path; error } ->
      "failed to parse config '" ^ Path.to_string path ^ "': " ^ Toml.error_to_string error
  | WriteFailed { path; error } ->
      "failed to write config '" ^ Path.to_string path ^ "': " ^ IO.error_message error
  | InvalidConfig error -> "invalid config: " ^ config_error_message error
  | InvalidRegistryConfig { registry_name; error } ->
      "invalid registry config for '" ^ registry_name ^ "': " ^ registry_error_message error

let default_api_url = fun ~registry_name ->
  Net.Uri.from_string ("https://api." ^ registry_name)
  |> Result.map_err
    ~fn:(fun error ->
      InvalidRegistryConfig {
        registry_name;
        error = InvalidDefaultUri { field = Api_url; error };
      })

let default_cdn_url = fun ~registry_name ->
  Net.Uri.from_string ("https://cdn." ^ registry_name)
  |> Result.map_err
    ~fn:(fun error ->
      InvalidRegistryConfig {
        registry_name;
        error = InvalidDefaultUri { field = Cdn_url; error };
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
        match Fields.get "api_url" fields with
        | None -> Ok defaults.api_url
        | Some (Toml.String url) ->
            Net.Uri.from_string url
            |> Result.map_err
              ~fn:(fun error ->
                InvalidRegistryConfig {
                  registry_name;
                  error = InvalidUri { field = Api_url; error };
                })
        | Some _ ->
            Error (InvalidRegistryConfig { registry_name; error = FieldMustBeString Api_url })
      in
      let* cdn_url =
        match Fields.get "cdn_url" fields with
        | None -> Ok defaults.cdn_url
        | Some (Toml.String url) ->
            Net.Uri.from_string url
            |> Result.map_err
              ~fn:(fun error ->
                InvalidRegistryConfig {
                  registry_name;
                  error = InvalidUri { field = Cdn_url; error };
                })
        | Some _ ->
            Error (InvalidRegistryConfig { registry_name; error = FieldMustBeString Cdn_url })
      in
      let api_token =
        match Fields.get "api_token" fields with
        | None -> Ok None
        | Some (Toml.String token) -> Ok (Some token)
        | Some _ ->
            Error (InvalidRegistryConfig { registry_name; error = FieldMustBeString Api_token })
      in
      Result.map api_token ~fn:(fun api_token -> { api_url; cdn_url; api_token })
  | _ -> Error (InvalidRegistryConfig { registry_name; error = RegistryEntryMustBeTable })

let normalize_registry_name = fun name ->
  let len = String.length name in
  if
    len >= 2
    && Char.equal (String.get_unchecked name ~at:0) '"'
    && Char.equal (String.get_unchecked name ~at:(len - 1)) '"'
  then
    String.sub name ~offset:1 ~len:(len - 2)
  else
    name

let rec collect_registries = fun ~path acc fields ->
  let has_registry_fields =
    List.any fields ~fn:(fun (name, _value) -> String.equal name "api_token")
  in
  let has_nested_tables =
    List.any
      fields
      ~fn:(fun (_name, value) ->
        match value with
        | Toml.Table _ -> true
        | _ -> false)
  in
  if List.length path > 0 && (has_registry_fields || not has_nested_tables) then
    let registry_name =
      String.concat "." (List.reverse path)
      |> normalize_registry_name
    in
    match registry_of_toml ~registry_name (Toml.Table fields) with
    | Ok registry -> Ok ((registry_name, registry) :: acc)
    | Error _ as err -> err
  else
    let rec loop acc = fun __tmp1 ->
      match __tmp1 with
      | [] -> Ok acc
      | (name, Toml.Table nested_fields) :: rest -> (
          match collect_registries ~path:(name :: path) acc nested_fields with
          | Ok acc -> loop acc rest
          | Error _ as err -> err
        )
      | _ :: rest -> loop acc rest
    in
    loop acc fields

let from_toml = fun value ->
  match value with
  | Toml.Table fields -> (
      match Fields.get "registry" fields with
      | None -> Ok empty
      | Some (Toml.Table registry_fields) ->
          collect_registries ~path:[] [] registry_fields
          |> Result.map ~fn:(fun registries -> { registries = List.reverse registries })
      | Some _ -> Error (InvalidConfig RegistryMustBeTable)
    )
  | _ -> Ok empty

let load = fun path ->
  match Fs.read_to_string path with
  | Error io_error -> Error (ReadFailed { path; error = io_error })
  | Ok source -> (
      match Toml.parse source with
      | Error parse_error -> Error (ParseFailed { path; error = parse_error })
      | Ok toml -> (
          match from_toml toml with
          | Ok config -> Ok config
          | Error (InvalidConfig _ as err) -> Error err
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
    ]
    in
    match registry.api_token with
    | Some token ->
        String.concat "\n" ((header :: fields) @ [ "api_token = " ^ render_string token; ])
    | None -> String.concat "\n" (header :: fields)
  in
  config.registries
  |> List.map ~fn:render_registry
  |> String.concat "\n\n"

let save = fun config path ->
  Fs.write (to_string config) path
  |> Result.map_err ~fn:(fun io_error -> WriteFailed { path; error = io_error })

let upsert_registry = fun config ~registry_name ~update ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> (
        match default_registry ~registry_name with
        | Ok registry -> List.reverse ((registry_name, update registry) :: acc)
        | Error _ -> List.reverse acc
      )
    | (name, registry) :: rest when String.equal name registry_name ->
        List.append (List.reverse acc) ((name, update registry) :: rest)
    | entry :: rest -> loop (entry :: acc) rest
  in
  { registries = loop [] config.registries }

let api_token = fun config ~registry_name ->
  match List.find config.registries ~fn:(fun (name, _registry) -> String.equal name registry_name) with
  | None -> None
  | Some (_name, registry) -> registry.api_token

let set_api_token = fun config ~registry_name token ->
  upsert_registry
    config
    ~registry_name
    ~update:(fun registry -> { registry with api_token = Some token })

let clear_api_token = fun config ~registry_name ->
  upsert_registry
    config
    ~registry_name
    ~update:(fun registry -> { registry with api_token = None })
