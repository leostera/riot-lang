open Std
open Tusk_build

let out = eprintln

let command =
  let open ArgParser in
    let open Arg in command "install"
    |> about "Install a binary to ~/.tusk/bin and project root"
    |> args
      [
        positional "package" |> help "Binary name to install";
        flag "local" |> long "local" |> help "Only install to project root, skip ~/.tusk/bin";
      ]

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
  out "To use the installed binary from anywhere, add ~/.tusk/bin to your PATH:";
  out "  export PATH='$HOME/.tusk/bin:$PATH'"

let write_install_event = fun ~workspace_root (event: Tusk_build.install_event) ->
  match event with
  | Tusk_build.Build _ ->
      ()
  | Tusk_build.InstallingBinary { binary; _ } ->
      out ("  \027[1;32mInstalling\027[0m " ^ binary)
  | Tusk_build.PromotedBinary { binary; destination; _ } ->
      out
        ("    \027[1;32mPromoted\027[0m " ^ binary ^ " to " ^ display_path ~workspace_root destination)
  | Tusk_build.PromotionWarning { binary; destination; reason=_; _ } ->
      out
        ("\027[1;33mWarning\027[0m: failed to promote "
        ^ binary
        ^ " to "
        ^ display_path ~workspace_root destination)
  | Tusk_build.InstalledBinary { binary; duration_ms; global_destination } ->
      let duration = Time.Duration.from_millis duration_ms
      |> Time.Duration.to_secs_string ~precision:2 in
      out ("   \027[1;32mInstalled\027[0m " ^ binary ^ " in " ^ duration ^ "s");
      (
        match global_destination with
        | Some _ -> print_path_hint ()
        | None -> ()
      )

let write_install_error = fun err ->
  out ("\027[1;31mError\027[0m: " ^ Tusk_build.install_error_message err)

let run = fun ~(workspace:Tusk_model.Workspace.t) matches ->
  let open ArgParser in
    let seen_registry_updates = Collections.HashSet.create () in
    let displayed_packages = Collections.HashSet.create () in
    let progress = Build.{ built_count = 0; cached_count = 0; failed_count = 0; skipped_count = 0 } in
    let binary_name = get_one matches "package" |> Option.expect ~msg:"binary name required" in
    let local_only = get_flag matches "local" in
    let on_event (event: Tusk_build.install_event) =
      match event with
      | Tusk_build.Build build_event -> (
          match build_event with
          | Tusk_build.Pm kind -> Build.write_pm_event ~mode:Build.Human ~seen_registry_updates kind
          | Tusk_build.BuildingTarget { target; host } -> Build.write_building_target_event
            ~mode:Build.Human
            ~target
            ~host
          | Tusk_build.Streaming streaming_event -> Build.write_streaming_event
            ~mode:Build.Human
            ~displayed_packages
            ~progress
            streaming_event
        )
      | _ -> write_install_event ~workspace_root:workspace.root event
    in
    match Tusk_build.install ~on_event { workspace; binary_name; local_only } with
    | Ok () -> Ok ()
    | Error err ->
        write_install_error err;
        Error (Failure (Tusk_build.install_error_message err))
