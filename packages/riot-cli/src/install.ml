open Std
open Std.Result.Syntax

module Install_runtime = Riot_install

let out = eprintln

let command =
  let open ArgParser in
  let open ArgParser.Arg in
  command "install"
  |> about "Install a local binary, registry package, or remote source to ~/.riot/bin"
  |> args
    [
      positional "name"
      |> required false
      |> help "Binary name, registry package, or remote source to install";
      option "package"
      |> short 'p'
      |> long "package"
      |> help "Install a binary from a specific workspace package";
      flag "local"
      |> long "local"
      |> help "Only install workspace binaries to the project root";
      flag "update"
      |> long "update"
      |> help "Refresh a cached remote source before installing";
    ]

type target =
  | Local of {
      package_name: Riot_model.Package_name.t option;
      binary_name: string;
      registry_fallback: Riot_deps.Registry_package_spec.t option;
    }
  | External of Install_runtime.external_spec * string

let parse_package_name = fun package_name ->
  Riot_model.Package_name.from_string package_name
  |> Result.map_err
    ~fn:(fun error ->
      Failure ("invalid package name '"
      ^ package_name
      ^ "': "
      ^ Riot_model.Package_name.error_message error))

let split_remote_binary = fun raw ->
  match String.last_index raw '@' with
  | Some idx when idx = String.length raw - 1 ->
      Error (Failure ("invalid remote target '" ^ raw ^ "': expected binary name after @"))
  | Some idx when idx > 0 && idx < String.length raw - 1 ->
      Ok (
        String.sub raw ~offset:0 ~len:idx,
        Some (String.sub raw ~offset:(idx + 1) ~len:(String.length raw - idx - 1))
      )
  | _ -> Ok (raw, None)

let default_remote_binary_name = fun (source_spec: Riot_deps.Git_dependency.spec) ->
  match Riot_deps.Git_dependency.parse_source_locator source_spec.source_locator with
  | Ok locator -> locator.repo
  | Error _ -> "main"

let parse_local_target = fun ?package_filter name ->
  match String.split name ~by:":" with
  | [ package_name; binary_name ] ->
      let* package_name = parse_package_name package_name in
      let* () =
        match package_filter with
        | Some expected_package when not
          (Riot_model.Package_name.equal expected_package package_name) ->
            Error (Failure ("conflicting package filters: got --package "
            ^ Riot_model.Package_name.to_string expected_package
            ^ " and binary target "
            ^ name))
        | _ -> Ok ()
      in
      Ok (Local { package_name = Some package_name; binary_name; registry_fallback = None })
  | _ ->
      Ok (
        Local {
          package_name = package_filter;
          binary_name = name;
          registry_fallback =
            match package_filter with
            | Some _ -> None
            | None ->
                Riot_deps.Registry_package_spec.from_string name
                |> Result.to_option;
        }
      )

let parse_target = fun ?package_filter raw ->
  if Riot_deps.Git_dependency.looks_like_remote_spec raw then
    match package_filter with
    | Some _ -> Error (Failure "--package cannot be used with remote source targets")
    | None -> (
        match split_remote_binary raw with
        | Ok (source_spec, binary_name) -> (
            match Riot_deps.Git_dependency.parse_spec source_spec with
            | Ok source_spec ->
                Ok (External (
                  Install_runtime.Source { spec = source_spec; update = false },
                  Option.unwrap_or ~default:(default_remote_binary_name source_spec) binary_name
                ))
            | Error err -> Error (Failure (Riot_deps.Git_dependency.message err))
          )
        | Error _ as err -> err
      )
  else
    parse_local_target ?package_filter raw

let write_install_error = fun err ->
  out
    ("\027[1;31mError\027[0m: " ^ Install_runtime.install_error_message err)

let write_workspace_error = fun message -> out ("\027[1;31mError\027[0m: " ^ message)

let local_install = fun ~on_event ~workspace ~package_name ~binary_name ~local_only ->
  Install_runtime.install
    ~on_event
    (
      Install_runtime.Workspace {
        workspace;
        package_name;
        binary_name;
        destination =
          if local_only then
            Install_runtime.Local
          else
            Install_runtime.Global;
      }
    )

let run_with_workspace_info = fun ~workspace ~workspace_error matches ->
  let open ArgParser in
  let workspace_root =
    match workspace with
    | Some (workspace: Riot_model.Workspace.t) -> Some workspace.root
    | None -> None
  in
  let ui = Ui.make ?workspace_root ~mode:(Ui.default_human_mode ()) () in
  let raw_target = get_one matches "name" in
  let* package_filter =
    match get_one matches "package" with
    | None -> Ok None
    | Some package_name ->
        parse_package_name package_name
        |> Result.map ~fn:Option.some
  in
  let local_only = get_flag matches "local" in
  let update = get_flag matches "update" in
  let on_event event = Ui.send ui event in
  let result =
    match match raw_target with
    | Some raw_target -> parse_target ?package_filter raw_target
    | None -> (
        match workspace with
        | Some workspace ->
            Run.resolve_implicit_local_target ?package_filter workspace
            |> Result.map
              ~fn:(fun Run.{ package_name; binary_name } ->
                Local { package_name = Some package_name; binary_name; registry_fallback = None })
            |> Result.map_err ~fn:(fun err -> Failure err)
        | None ->
            Error (Failure (Option.unwrap_or ~default:"Not in a riot workspace" workspace_error))
      ) with
    | Error (Failure message) -> Error (`Cli message)
    | Error err -> Error (`Cli (Exception.to_string err))
    | Ok (External (spec, binary_name)) ->
        if local_only then
          Error (`Cli "--local is only supported when installing a workspace binary")
        else
          let spec =
            match spec with
            | Install_runtime.Source { spec; update = _ } -> Install_runtime.Source { spec; update }
            | Install_runtime.Registry _ as spec -> spec
          in
          Install_runtime.install ~on_event (Install_runtime.External { spec; binary_name })
          |> Result.map_err ~fn:(fun err -> `Install err)
    | Ok (Local { package_name; binary_name; registry_fallback }) -> (
        match workspace with
        | Some workspace -> (
            match local_install ~on_event ~workspace ~package_name ~binary_name ~local_only with
            | Ok () as ok -> ok
            | Error (Install_runtime.BinaryNotFound _) when not local_only -> (
                match registry_fallback with
                | Some package_spec ->
                    Install_runtime.install
                      ~on_event
                      (Install_runtime.External {
                        spec = Install_runtime.Registry package_spec;
                        binary_name = "main";
                      })
                    |> Result.map_err ~fn:(fun err -> `Install err)
                | None -> Error (`Install (Install_runtime.BinaryNotFound { binary_name }))
              )
            | Error err -> Error (`Install err)
          )
        | None ->
            if local_only then
              Error (`Cli (Option.unwrap_or
                ~default:"--local requires a riot workspace"
                workspace_error))
            else
              (
                match registry_fallback with
                | Some package_spec ->
                    Install_runtime.install
                      ~on_event
                      (Install_runtime.External {
                        spec = Install_runtime.Registry package_spec;
                        binary_name = "main";
                      })
                    |> Result.map_err ~fn:(fun err -> `Install err)
                | None ->
                    Error (`Cli ("'"
                    ^ binary_name
                    ^ "' is not a valid registry package spec outside a riot workspace"))
              )
      )
  in
  match result with
  | Ok () -> Ok ()
  | Error (`Cli message) ->
      write_workspace_error message;
      Error (Failure message)
  | Error (`Install err) ->
      write_install_error err;
      Error (Failure (Install_runtime.install_error_message err))

let run = fun ~workspace matches ->
  run_with_workspace_info
    ~workspace:(Some workspace)
    ~workspace_error:None
    matches
