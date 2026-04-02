open Std

let ( let* ) = Result.and_then

let compare_by_path = fun left right ->
  String.compare (Path.to_string left) (Path.to_string right)

let rec canonicalize_toml_value = function
  | Std.Data.Toml.String _ as value -> value
  | Std.Data.Toml.Int _ as value -> value
  | Std.Data.Toml.Bool _ as value -> value
  | Std.Data.Toml.Array items -> Std.Data.Toml.Array (List.map canonicalize_toml_value items)
  | Std.Data.Toml.Table fields ->
      Std.Data.Toml.Table (
        fields |> List.map (fun (key, value) -> (key, canonicalize_toml_value value)) |> List.sort
          (fun (left, _) (right, _) ->
            String.compare left right)
      )

let manifest_id = fun ~workspace_root manifest_path ->
  match Path.strip_prefix manifest_path ~prefix:workspace_root with
  | Ok relative -> Path.to_string relative
  | Error _ -> Path.to_string manifest_path

let dependency_section_value = fun ~manifest_path section_name toml ->
  match toml with
  | Std.Data.Toml.Table fields -> (
      match List.assoc_opt section_name fields with
      | Some (Std.Data.Toml.Table _ as value) -> Ok (canonicalize_toml_value value)
      | Some _ -> Error ("manifest dependency section '["
      ^ section_name
      ^ "]' in '"
      ^ Path.to_string manifest_path
      ^ "' must be a table")
      | None -> Ok (Std.Data.Toml.Table [])
    )
  | _ -> Error ("manifest '" ^ Path.to_string manifest_path ^ "' must decode to a TOML table")

let load_manifest_toml = fun ~workspace_manager manifest_path ->
  match workspace_manager with
  | Some workspace_manager -> Riot_model.Workspace_manager.load_riot_toml workspace_manager manifest_path
  | None ->
      let* source = Fs.read_to_string manifest_path
      |> Result.map_error
        (fun err ->
          "failed to read manifest '" ^ Path.to_string manifest_path ^ "': " ^ IO.error_message err) in
      Std.Data.Toml.parse source
      |> Result.map_error
        (fun err ->
          "failed to parse manifest '"
          ^ Path.to_string manifest_path
          ^ "': "
          ^ Std.Data.Toml.error_to_string err)

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
  let manifest_paths = List.sort_uniq compare_by_path manifest_paths in
  let rec loop acc = function
    | [] ->
        let canonical = Std.Data.Toml.Array (List.rev acc)
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
