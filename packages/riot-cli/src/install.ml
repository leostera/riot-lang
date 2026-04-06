open Std
open Riot_build

let out = eprintln

let command =
  let open ArgParser in
  let open Arg in
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
         flag "local" |> long "local" |> help "Only install workspace binaries to the project root";
         flag "update" |> long "update" |> help "Refresh a cached remote source before installing";
       ]

type target =
  | Local of { package_name: string option; binary_name: string; registry_fallback: string option }
  | Remote_source of { source_spec: string; binary_name: string }

let display_path = fun ~workspace_root path ->
  match Path.strip_prefix path ~prefix:workspace_root with
  | Ok rel -> "./" ^ Path.to_string rel
  | Error _ -> (
      match Env.home_dir () with
      | Some home -> (
          match Path.strip_prefix path ~prefix:home with
          | Ok rel -> "~/" ^ Path.to_string rel
          | Error _ -> Path.to_string path
        )
      | None -> Path.to_string path
    )

let print_path_hint = fun () ->
  out "";
  out "To use the installed binary from anywhere, add ~/.riot/bin to your PATH:";
  out "  export PATH='$HOME/.riot/bin:$PATH'"

let split_remote_binary = fun raw ->
  match String.rindex_opt raw '@' with
  | Some idx when idx = String.length raw - 1 ->
      Error (Failure ("invalid remote target '" ^ raw ^ "': expected binary name after @"))
  | Some idx when idx > 0 && idx < String.length raw - 1 ->
      Ok (
        String.sub raw 0 idx,
        Some (String.sub raw (idx + 1) (String.length raw - idx - 1))
      )
  | _ -> Ok (raw, None)

let default_remote_binary_name = fun source_spec ->
  match Riot_deps.Git_dependency.parse_source_locator source_spec with
  | Ok locator -> locator.repo
  | Error _ -> "main"

let parse_local_target = fun ?package_filter name ->
  match String.split_on_char ':' name with
  | [ package_name; binary_name ] -> (
      match package_filter with
      | Some expected_package when not (String.equal expected_package package_name) ->
          Error
            (Failure
               ("conflicting package filters: got --package "
               ^ expected_package
               ^ " and binary target "
               ^ name))
      | _ -> Ok (Local { package_name = Some package_name; binary_name; registry_fallback = None })
    )
  | _ ->
      Ok (Local {
        package_name = package_filter;
        binary_name = name;
        registry_fallback =
          match package_filter with
          | Some _ -> None
          | None -> Some name
      })

let parse_target = fun ?package_filter raw ->
  if Riot_deps.Git_dependency.looks_like_remote_spec raw then
    match package_filter with
    | Some _ -> Error (Failure "--package cannot be used with remote source targets")
    | None -> (
        match split_remote_binary raw with
        | Ok (source_spec, binary_name) ->
            Ok (Remote_source {
              source_spec;
              binary_name = Option.unwrap_or ~default:(default_remote_binary_name source_spec) binary_name
            })
        | Error _ as err -> err
      )
  else
    parse_local_target ?package_filter raw

let write_install_event = fun ~workspace_root (event: Riot_build.install_event) ->
  match event with
  | Riot_build.Build _ -> ()
  | Riot_build.InstallingBinary { binary; _ } ->
      out ("  \027[1;34mInstalling\027[0m " ^ binary)
  | Riot_build.PromotedBinary { binary; destination; _ } ->
      out ("    \027[1;32mPromoted\027[0m " ^ binary ^ " to " ^ display_path ~workspace_root destination)
  | Riot_build.InstalledBinary { binary; duration_ms; global_destination } ->
      let duration =
        Time.Duration.from_millis duration_ms |> Time.Duration.to_secs_string ~precision:2
      in
      out ("   \027[1;32mInstalled\027[0m " ^ binary ^ " in " ^ duration ^ "s");
      (
        match global_destination with
        | Some _ -> print_path_hint ()
        | None -> ()
      )

let write_install_error = fun err ->
  out ("\027[1;31mError\027[0m: " ^ Riot_build.install_error_message err)

let write_workspace_error = fun message ->
  out ("\027[1;31mError\027[0m: " ^ message)

let local_install = fun ~on_event ~workspace ~package_name ~binary_name ~local_only ->
  Riot_build.install
    ~on_event
    {
      workspace;
      package_name;
      binary_name;
      local_only;
      promote_to_workspace_root = true;
    }

let run_with_workspace_info = fun ~workspace ~workspace_error matches ->
  let open ArgParser in
  let seen_registry_updates = Collections.HashSet.create () in
  let displayed_packages = Collections.HashSet.create () in
  let progress = Build.{ built_count = 0; cached_count = 0; failed_count = 0; skipped_count = 0 } in
  let raw_target = get_one matches "name" in
  let package_filter = get_one matches "package" in
  let local_only = get_flag matches "local" in
  let update = get_flag matches "update" in
  let workspace_root_for_output =
    match workspace with
    | Some (workspace: Riot_model.Workspace.t) -> workspace.root
    | None -> Path.v "."
  in
  let on_event (event: Riot_build.install_event) =
    match event with
    | Riot_build.Build build_event -> (
        match build_event with
        | Riot_build.Pm kind -> Build.write_pm_event ~mode:Build.Human ~seen_registry_updates kind
        | Riot_build.BuildingTarget { target; host } ->
            Build.write_building_target_event ~mode:Build.Human ~target ~host
        | Riot_build.CacheGc event -> Build.write_cache_gc_event ~mode:Build.Human event
        | Riot_build.Streaming streaming_event ->
            Build.write_streaming_event ~mode:Build.Human ~displayed_packages ~progress streaming_event
      )
    | _ -> write_install_event ~workspace_root:workspace_root_for_output event
  in
  let result =
    match
      match raw_target with
      | Some raw_target -> parse_target ?package_filter raw_target
      | None -> (
          match workspace with
          | Some workspace ->
              Run.resolve_implicit_local_target ?package_filter workspace
              |> Result.map (fun Run.{ package_name; binary_name } ->
                Local { package_name = Some package_name; binary_name; registry_fallback = None })
              |> Result.map_error (fun err -> Failure err)
          | None ->
              Error (Failure (Option.unwrap_or ~default:"Not in a riot workspace" workspace_error))
        )
    with
    | Error (Failure message) -> Error (`Cli message)
    | Error err -> Error (`Cli (Exception.to_string err))
    | Ok (Remote_source { source_spec; binary_name }) ->
        if local_only then
          Error (`Cli "--local is only supported when installing a workspace binary")
        else
          Riot_build.install_source ~on_event { source_spec; binary_name; update; local_only = false }
          |> Result.map_error (fun err -> `Install err)
    | Ok (Local { package_name; binary_name; registry_fallback }) -> (
        match workspace with
        | Some workspace -> (
            match
              local_install
                ~on_event
                ~workspace
                ~package_name
                ~binary_name
                ~local_only
            with
            | Ok () as ok -> ok
            | Error (Riot_build.BinaryNotFound _) when not local_only -> (
                match registry_fallback with
                | Some package_spec ->
                    Riot_build.install_registry
                      ~on_event
                      { package_spec; binary_name = "main"; local_only = false }
                    |> Result.map_error (fun err -> `Install err)
                | None -> Error (`Install (Riot_build.BinaryNotFound { binary_name }))
              )
            | Error err -> Error (`Install err)
          )
        | None ->
            if local_only then
              Error (`Cli (Option.unwrap_or ~default:"--local requires a riot workspace" workspace_error))
            else (
              match registry_fallback with
              | Some package_spec ->
                  Riot_build.install_registry
                    ~on_event
                    { package_spec; binary_name = "main"; local_only = false }
                  |> Result.map_error (fun err -> `Install err)
              | None ->
                  Error (`Cli (Option.unwrap_or ~default:"Not in a riot workspace" workspace_error))
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
      Error (Failure (Riot_build.install_error_message err))

let run = fun ~workspace matches ->
  run_with_workspace_info ~workspace:(Some workspace) ~workspace_error:None matches
