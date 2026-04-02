open Std
open Std.Collections

type error =
  | UpdateFailed of Tusk_deps.package_error

let out = eprintln

let command =
  let open ArgParser in
    let open Arg in command "update"
    |> about "Re-resolve the workspace graph and rewrite tusk.lock"
    |> args [ flag "json" |> long "json" |> help "Render events as JSON"; ]

let message = function
  | UpdateFailed error -> Tusk_deps.package_error_message error

let fail = fun err ->
  out ("\027[1;31mError\027[0m: " ^ message err);
  Error (Failure (message err))

let json_of_event = function
  | Tusk_deps.RegistryPackageLookupStarted { package } -> Some (Data.Json.Object [
    ("type", Data.Json.String "RegistryPackageLookupStarted");
    ("package", Data.Json.String package)
  ])
  | Tusk_deps.RegistryPackageLookupFinished { package; latest_version } -> Some (Data.Json.Object [
    ("type", Data.Json.String "RegistryPackageLookupFinished");
    ("package", Data.Json.String package);
    ("latest_version", Data.Json.String latest_version)
  ])
  | Tusk_deps.ManifestUpdated { path; section; operation; dependency } ->
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
  | Tusk_deps.Pm _ -> None

let write_pm_event_json = fun ~session_id kind ->
  Tusk_model.Event.create ~session_id ~level:Tusk_model.Event.Info kind
  |> Tusk_model.Event.to_json
  |> Data.Json.to_string
  |> println

let write_pm_event_human = fun ~session_id ~seen_registry_updates kind ->
  Tusk_model.Event.create ~session_id ~level:Tusk_model.Event.Info kind
  |> Build.write_pm_event ~mode:Build.Human ~seen_registry_updates

let write_build_event_json = fun event ->
  match Tusk_build.Event.to_json event with
  | Some json -> println (Data.Json.to_string json)
  | None -> ()

let write_event = fun ~mode ~pm_session_id ~seen_registry_updates event ->
  match mode with
  | Build.Json -> (
      match event with
      | Tusk_deps.Pm event -> write_pm_event_json ~session_id:pm_session_id event
      | _ -> Option.iter (fun json -> println (Data.Json.to_string json)) (json_of_event event)
    )
  | Build.Human -> (
      match event with
      | Tusk_deps.RegistryPackageLookupStarted _ -> ()
      | Tusk_deps.RegistryPackageLookupFinished _ -> ()
      | Tusk_deps.ManifestUpdated _ -> ()
      | Tusk_deps.Pm event -> write_pm_event_human ~session_id:pm_session_id ~seen_registry_updates event
    )

let run = fun ~workspace matches ->
  let mode =
    if ArgParser.get_flag matches "json" then
      Build.Json
    else
      Build.Human
  in
  let pm_session_id = Tusk_model.Session_id.make () in
  let seen_registry_updates = HashSet.create () in
  match Tusk_deps.update
    ~on_event:(write_event ~mode ~pm_session_id ~seen_registry_updates)
    ~workspace
    () with
  | Ok () -> Ok ()
  | Error error -> fail (UpdateFailed error)
