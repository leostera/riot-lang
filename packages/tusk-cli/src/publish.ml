open Std
open Tusk_model

type request =
  | Workspace
  | Package of string

type error =
  | ConflictingSelection
  | PackageNotFound of { package: string }
  | NoWorkspacePackages
  | PublishConfigLoadFailed of Tusk_model.User_config.error
  | MissingApiToken of { registry_name: string; path: Path.t }
  | RegistryInitializationFailed of { registry_name: string; error: string }
  | PublishPlanFailed of Tusk_pm.Publisher.error
  | PublishFailed of { package: string; error: Tusk_pm.Publisher.error }

let default_registry_name = "pkgs.ml"

let out = eprintln

let command =
  let open ArgParser in
    let open Arg in command "publish"
    |> about "Publish packages to the registry"
    |> args
      [
        option "package" |> short 'p' |> long "package" |> help "Publish a specific workspace package";
        flag "workspace" |> long "workspace" |> help "Publish workspace packages in dependency order";
      ]

let message = function
  | ConflictingSelection ->
      "cannot combine --package with --workspace"
  | PackageNotFound { package } ->
      "package '" ^ package ^ "' was not found in this workspace"
  | NoWorkspacePackages ->
      "no workspace packages were found to publish"
  | PublishConfigLoadFailed err ->
      Tusk_model.User_config.message err
  | MissingApiToken { registry_name; path } ->
      "missing API token for registry '"
      ^ registry_name
      ^ "' in "
      ^ Path.to_string path
      ^ " (expected [registry.\""
      ^ registry_name
      ^ "\"].api_token)"
  | RegistryInitializationFailed { registry_name; error } ->
      "failed to initialize registry '" ^ registry_name ^ "': " ^ error
  | PublishPlanFailed err ->
      Tusk_pm.Publisher.message err
  | PublishFailed { error; _ } ->
      Tusk_pm.Publisher.message error

let resolve_request = fun ~package_name ~workspace_mode ->
  match package_name, workspace_mode with
  | Some _, true ->
      Error ConflictingSelection
  | Some package, false ->
      Ok (Package package)
  | None, _ ->
      Ok Workspace

let workspace_packages = fun (workspace: Workspace.t) ->
  workspace.packages |> List.filter Package.is_workspace_member

let select_packages = fun ~workspace request ->
  let packages = workspace_packages workspace in
  match request with
  | Package package_name -> (
      match List.find_opt (fun (pkg: Package.t) -> String.equal pkg.name package_name) packages with
      | Some pkg ->
          Ok [ pkg ]
      | None ->
          Error (PackageNotFound { package = package_name })
    )
  | Workspace ->
      if packages = [] then
        Error NoWorkspacePackages
      else
        Tusk_pm.Publisher.workspace_publish_order ~packages
        |> Result.map_error (fun err -> PublishPlanFailed err)

let load_api_token = fun ~registry_name ->
  let config_path = Tusk_model.Tusk_dirs.config_path () in
  match Fs.exists config_path with
  | Error io_error ->
      Error (PublishConfigLoadFailed (Tusk_model.User_config.ReadFailed {
        path = config_path;
        error = IO.error_message io_error;
      }))
  | Ok false ->
      Error (MissingApiToken { registry_name; path = config_path })
  | Ok true -> (
      match Tusk_model.User_config.load config_path with
      | Error err ->
          Error (PublishConfigLoadFailed err)
      | Ok config -> (
          match Tusk_model.User_config.api_token config ~registry_name with
          | Some token ->
              Ok token
          | None ->
              Error (MissingApiToken { registry_name; path = config_path })
        )
    )

let registry = fun ~registry_name ->
  Pkgs_ml.Registry.create_filesystem ~registry_name ()
  |> Result.map_error (fun error -> RegistryInitializationFailed { registry_name; error })

let render_publishing = fun package_name ->
  "    \027[1;32mPublishing\027[0m " ^ package_name

let render_published = fun (published: Pkgs_ml.Registry.published_release) ->
  "    \027[1;32mPublished\027[0m "
  ^ published.package_name
  ^ " "
  ^ published.package_version

let publish_one = fun ~registry ~api_token (package: Package.t) ->
  out (render_publishing package.name);
  Tusk_pm.Publisher.publish ~registry ~package ~api_token
  |> Result.map_error (fun error -> PublishFailed { package = package.name; error })
  |> Result.map (fun published ->
    out (render_published published);
    published)

let rec publish_all = fun ~registry ~api_token packages ->
  match packages with
  | [] ->
      Ok ()
  | package :: rest -> (
      match publish_one ~registry ~api_token package with
      | Ok _ ->
          publish_all ~registry ~api_token rest
      | Error _ as err ->
          err
    )

let run = fun (workspace: Workspace.t) matches ->
  let package_name = ArgParser.get_one matches "package" in
  let workspace_mode = ArgParser.get_flag matches "workspace" in
  match resolve_request ~package_name ~workspace_mode with
  | Error err ->
      Error (Failure (message err))
  | Ok request -> (
      match select_packages ~workspace request with
      | Error err ->
          Error (Failure (message err))
      | Ok packages -> (
          match load_api_token ~registry_name:default_registry_name with
          | Error err ->
              Error (Failure (message err))
          | Ok api_token -> (
              match registry ~registry_name:default_registry_name with
              | Error err ->
                  Error (Failure (message err))
              | Ok registry -> (
                  match publish_all ~registry ~api_token packages with
                  | Ok () ->
                      Ok ()
                  | Error err ->
                      Error (Failure (message err))
                )
            )
        )
    )
