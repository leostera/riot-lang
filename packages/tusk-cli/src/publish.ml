open Std
open Std.Collections
open Tusk_model

type request =
  | Workspace
  | Package of string

type error =
  | ConflictingSelection
  | PublishFailed of Tusk_pm.Publish.error

let out = eprintln

let command =
  let open ArgParser in
    let open Arg in command "publish"
    |> about "Publish packages to the registry"
    |> args
      [
        option "package" |> short 'p' |> long "package" |> help "Publish a specific workspace package";
        flag "workspace" |> long "workspace" |> help "Publish workspace packages in dependency order";
        flag "dry-run" |> long "dry-run" |> help "Run local publish checks without uploading";
      ]

let message = function
  | ConflictingSelection -> "cannot combine --package with --workspace"
  | PublishFailed error -> Tusk_pm.Publish.message error

let fail = fun err ->
  out ("\027[1;31mError\027[0m: " ^ message err);
  Error (Failure (message err))

let resolve_request = fun ~package_name ~workspace_mode ->
  match package_name, workspace_mode with
  | Some _, true -> Error ConflictingSelection
  | Some package, false -> Ok (Package package)
  | None, _ -> Ok Workspace

let publish_request = function
  | Workspace -> Tusk_pm.Publish.Workspace
  | Package package -> Tusk_pm.Publish.Package package

let format_pm_event = fun ~seen_registry_updates kind ->
  match kind with
  | Tusk_model.Event.RegistryIndexUpdating { registry } ->
      if HashSet.contains seen_registry_updates registry then
        None
      else
        (
          let _ = HashSet.insert seen_registry_updates registry in
          Some ("    \027[1;32mUpdating\027[0m " ^ registry ^ " index")
        )
  | _ -> None

let render_dry_run = fun (prepared: Tusk_pm.Publisher.prepared_publish) ->
  "    \027[1;32mWouldPublish\027[0m "
  ^ prepared.package.name
  ^ " "
  ^ prepared.locator
  ^ "@"
  ^ prepared.selector

let render_published = fun (published: Pkgs_ml.Registry.published_release) ->
  "    \027[1;32mPublished\027[0m " ^ published.package_name ^ " " ^ published.package_version

let format_publish_event = fun ~seen_registry_updates event ->
  match event with
  | Tusk_pm.Publish.Pm kind -> format_pm_event ~seen_registry_updates kind
  | Tusk_pm.Publish.DryRunPlanned prepared -> Some (render_dry_run prepared)
  | Tusk_pm.Publish.PackagePublished published -> Some (render_published published)
  | Tusk_pm.Publish.Fmt _
  | Tusk_pm.Publish.Fix _
  | Tusk_pm.Publish.CheckStarted _
  | Tusk_pm.Publish.CheckFinished _ -> None

let write_publish_event = fun ~seen_registry_updates event ->
  match format_publish_event ~seen_registry_updates event with
  | Some message -> out message
  | None -> ()

let run = fun (workspace: Workspace.t) matches ->
  match resolve_request
    ~package_name:(ArgParser.get_one matches "package")
    ~workspace_mode:(ArgParser.get_flag matches "workspace") with
  | Error err -> fail err
  | Ok request ->
      let mode =
        if ArgParser.get_flag matches "dry-run" then
          Tusk_pm.Publish.Dry_run
        else
          Tusk_pm.Publish.Publish
      in
      let seen_registry_updates = HashSet.create () in
      match Tusk_pm.Publish.run
        ~on_event:(write_publish_event ~seen_registry_updates)
        ~workspace
        ~request:(publish_request request)
        ~mode
        () with
      | Error err -> fail (PublishFailed err)
      | Ok _results -> Ok ()
