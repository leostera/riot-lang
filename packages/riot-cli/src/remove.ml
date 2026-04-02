open Std
open Std.Collections

type error =
  | MissingDependency
  | ConflictingTarget
  | ConflictingScope
  | CurrentDirUnavailable of string
  | RemoveFailed of Riot_deps.package_error

let out = eprintln

let command =
  let open ArgParser in
    let open Arg in command "rm"
    |> about "Remove a dependency from a manifest section and refresh riot.lock"
    |> args
      [
        positional "dependency" |> help "Dependency name to remove";
        option "package" |> short 'p' |> long "package" |> help "Edit a specific workspace package manifest";
        flag "workspace" |> long "workspace" |> help "Edit the workspace root manifest";
        flag "build" |> long "build" |> help "Remove from [build-dependencies]";
        flag "dev" |> long "dev" |> help "Remove from [dev-dependencies]";
        flag "json" |> long "json" |> help "Render events as JSON";
      ]

let message = function
  | MissingDependency -> "missing dependency name"
  | ConflictingTarget -> "cannot combine --workspace with --package"
  | ConflictingScope -> "cannot combine --build with --dev"
  | CurrentDirUnavailable error -> "failed to determine current directory: " ^ error
  | RemoveFailed error -> Riot_deps.package_error_message error

let path_error_message = function
  | Path.InvalidUtf8 { path } -> "invalid UTF-8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } -> "system call '"
  ^ syscall
  ^ "' returned invalid UTF-8 path: "
  ^ path
  | Path.SystemError error -> error

let fail = fun err ->
  out ("\027[1;31mError\027[0m: " ^ message err);
  Error (Failure (message err))

let selection_of_matches = fun matches ->
  let package = ArgParser.get_one matches "package" in
  let workspace = ArgParser.get_flag matches "workspace" in
  match package, workspace with
  | Some _, true -> Error ConflictingTarget
  | Some package, false -> Ok (Riot_deps.Package package)
  | None, true -> Ok Riot_deps.Workspace
  | None, false -> Ok Riot_deps.Current

let scope_of_matches = fun matches ->
  let build = ArgParser.get_flag matches "build" in
  let dev = ArgParser.get_flag matches "dev" in
  match build, dev with
  | true, true -> Error ConflictingScope
  | true, false -> Ok Riot_deps.Build
  | false, true -> Ok Riot_deps.Dev
  | false, false -> Ok Riot_deps.Runtime

let json_of_event = function
  | Riot_deps.RegistryPackageLookupStarted { package } -> Some (Data.Json.Object [
    ("type", Data.Json.String "RegistryPackageLookupStarted");
    ("package", Data.Json.String package)
  ])
  | Riot_deps.RegistryPackageLookupFinished { package; latest_version } -> Some (Data.Json.Object [
    ("type", Data.Json.String "RegistryPackageLookupFinished");
    ("package", Data.Json.String package);
    ("latest_version", Data.Json.String latest_version)
  ])
  | Riot_deps.SourceDependencyMaterializationStarted { source_locator; ref_ } -> Some (Data.Json.Object [
    ("type", Data.Json.String "SourceDependencyMaterializationStarted");
    ("source_locator", Data.Json.String source_locator);
    (
      "ref",
      match ref_ with
      | Some ref_ -> Data.Json.String ref_
      | None -> Data.Json.Null
    )
  ])
  | Riot_deps.SourceDependencyMaterializationFinished { source_locator; ref_; package; version } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "SourceDependencyMaterializationFinished");
        ("source_locator", Data.Json.String source_locator);
        (
          "ref",
          match ref_ with
          | Some ref_ -> Data.Json.String ref_
          | None -> Data.Json.Null
        );
        ("package", Data.Json.String package);
        (
          "version",
          match version with
          | Some version -> Data.Json.String version
          | None -> Data.Json.Null
        )
      ])
  | Riot_deps.ManifestUpdated { path; section; operation; dependency } ->
      Some (
        Data.Json.Object [
          ("type", Data.Json.String "ManifestUpdated");
          ("path", Data.Json.String (Path.to_string path));
          ("section", Data.Json.String section);
          (
            "operation",
            Data.Json.String (
              match operation with
              | `Add -> "add"
              | `Remove -> "remove"
            )
          );
          ("dependency", Data.Json.String dependency)
        ]
      )
  | Riot_deps.PackageUpdated { package; from_version; to_version } -> Some (Data.Json.Object [
    ("type", Data.Json.String "PackageUpdated");
    ("package", Data.Json.String package);
    ("from_version", Data.Json.String from_version);
    ("to_version", Data.Json.String to_version)
  ])
  | Riot_deps.Pm _ -> None

let write_pm_event_json = fun ~session_id kind ->
  Riot_model.Event.create ~session_id ~level:Riot_model.Event.Info kind
  |> Riot_model.Event.to_json
  |> Data.Json.to_string
  |> println

let write_pm_event_human = fun ~session_id ~seen_registry_updates kind ->
  Riot_model.Event.create ~session_id ~level:Riot_model.Event.Info kind
  |> Build.write_pm_event ~mode:Build.Human ~seen_registry_updates

let write_event = fun ~mode ~pm_session_id ~seen_registry_updates event ->
  match mode with
  | Build.Json -> (
      match event with
      | Riot_deps.Pm event -> write_pm_event_json ~session_id:pm_session_id event
      | _ -> Option.iter (fun json -> println (Data.Json.to_string json)) (json_of_event event)
    )
  | Build.Human -> (
      match event with
      | Riot_deps.RegistryPackageLookupStarted _ ->
          ()
      | Riot_deps.RegistryPackageLookupFinished _ ->
          ()
      | Riot_deps.SourceDependencyMaterializationStarted _ ->
          ()
      | Riot_deps.SourceDependencyMaterializationFinished _ ->
          ()
      | Riot_deps.PackageUpdated { package; from_version; to_version } ->
          out
            ("    \027[1;32mUpdated\027[0m "
            ^ package
            ^ " ("
            ^ from_version
            ^ " -> "
            ^ to_version
            ^ ")")
      | Riot_deps.ManifestUpdated { path; section; operation; dependency } ->
          let verb =
            match operation with
            | `Add -> "Added"
            | `Remove -> "Removed"
          in
          out
            ("    \027[1;32m"
            ^ verb
            ^ "\027[0m "
            ^ dependency
            ^ " ("
            ^ section
            ^ ") in "
            ^ Path.to_string path)
      | Riot_deps.Pm event ->
          write_pm_event_human ~session_id:pm_session_id ~seen_registry_updates event
    )

let run = fun ~workspace matches ->
  let mode =
    if ArgParser.get_flag matches "json" then
      Build.Json
    else
      Build.Human
  in
  let dependency =
    match ArgParser.get_one matches "dependency" with
    | Some dependency -> Ok dependency
    | None -> Error MissingDependency
  in
  match dependency, selection_of_matches matches, scope_of_matches matches, Env.current_dir () with
  | Ok dependency, Ok selection, Ok scope, Ok cwd ->
      let request : Riot_deps.remove_request = Riot_deps.{ selection; scope; dependency } in
      let pm_session_id = Riot_model.Session_id.make () in
      let seen_registry_updates = HashSet.create () in
      (
        match Riot_deps.remove
          ~on_event:(write_event ~mode ~pm_session_id ~seen_registry_updates)
          ~workspace
          ~cwd
          ~request
          () with
        | Ok () -> Ok ()
        | Error error -> fail (RemoveFailed error)
      )
  | (Error err, _, _, _)
  | (_, Error err, _, _)
  | (_, _, Error err, _) ->
      fail err
  | _, _, _, Error err ->
      fail (CurrentDirUnavailable (path_error_message err))
