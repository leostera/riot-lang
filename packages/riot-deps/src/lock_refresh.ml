open Std
open Std.Result.Syntax

let compare_by_path = fun left right ->
  String.compare (Path.to_string left) (Path.to_string right)

let rec canonicalize_toml_value = function
  | Std.Data.Toml.String _ as value -> value
  | Std.Data.Toml.Int _ as value -> value
  | Std.Data.Toml.Bool _ as value -> value
  | Std.Data.Toml.Array items -> Std.Data.Toml.Array (List.map items ~fn:canonicalize_toml_value)
  | Std.Data.Toml.Table fields ->
      Std.Data.Toml.Table (
        fields
        |> List.map ~fn:(fun (key, value) -> (key, canonicalize_toml_value value))
        |> List.sort
          ~compare:(fun (left, _) (right, _) ->
            String.compare left right)
      )

let manifest_id = fun ~workspace_root manifest_path ->
  match Path.strip_prefix manifest_path ~prefix:workspace_root with
  | Ok relative -> Path.to_string relative
  | Error _ -> Path.to_string manifest_path

let dependency_section_value = fun ~manifest_path section_name toml ->
  match toml with
  | Std.Data.Toml.Table fields -> (
      match
        List.find fields
          ~fn:(fun (key, _) ->
            String.equal key section_name) |> Option.map ~fn:(fun (_, value) -> value)
      with
      | Some (Std.Data.Toml.Table _ as value) -> Ok (canonicalize_toml_value value)
      | Some _ -> Error (format
        Format.[
          str "manifest dependency section '[";
          str section_name;
          str "]' in '";
          str (Path.to_string manifest_path);
          str "' must be a table";
        ])
      | None -> Ok (Std.Data.Toml.Table [])
    )
  | _ -> Error (format
    Format.[
      str "manifest '";
      str (Path.to_string manifest_path);
      str "' must decode to a TOML table";
    ])

let load_manifest_toml = fun ~workspace_manager manifest_path ->
  Riot_model.Workspace_manager.load_riot_toml workspace_manager manifest_path
  |> Result.map_err ~fn:Riot_model.Workspace_manager.manifest_load_error_message

let manifest_dependency_fingerprint = fun ~workspace_manager ~workspace_root manifest_path ->
  let* toml = load_manifest_toml ~workspace_manager manifest_path in
  let* dependencies = dependency_section_value ~manifest_path "dependencies" toml in
  let* build_dependencies = dependency_section_value ~manifest_path "build-dependencies" toml in
  let* dev_dependencies = dependency_section_value ~manifest_path "dev-dependencies" toml in
  Ok (Std.Data.Toml.Table [
    ("manifest", Std.Data.Toml.String (manifest_id ~workspace_root manifest_path));
    ("dependencies", dependencies);
    ("build-dependencies", build_dependencies);
    ("dev-dependencies", dev_dependencies);
  ])

let dependency_hash = fun ~workspace_manager ~workspace_root ~manifest_paths ->
  let manifest_paths = List.unique manifest_paths ~compare:compare_by_path in
  let rec loop acc = function
    | [] ->
        let canonical = Std.Data.Toml.Array (List.reverse acc)
        |> canonicalize_toml_value
        |> Std.Data.Toml.to_string in
        Ok (Crypto.hash_string canonical |> Crypto.Digest.hex)
    | manifest_path :: rest ->
        let* fingerprint = manifest_dependency_fingerprint ~workspace_manager ~workspace_root manifest_path in
        loop (fingerprint :: acc) rest
  in
  loop [] manifest_paths

let needs_refresh = fun ~workspace_manager ~workspace_root ~manifest_paths ~lockfile ->
  let* current_dependency_hash = dependency_hash ~workspace_manager ~workspace_root ~manifest_paths in
  match lockfile with
  | None -> Ok true
  | Some (lockfile: Riot_model.Lockfile.t) -> Ok (not
    (String.equal lockfile.dependency_hash current_dependency_hash))
