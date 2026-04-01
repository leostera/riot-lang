open Std
open Std.Collections
open Tusk_model
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

let build_binary = fun ~workspace package_name ->
  Build.build_command
    ~workspace
    ~mode:Build.Human
    ~show_finished_summary:false
    (Some package_name)
    None

let find_built_binary_path = fun ~workspace ~package_name ~binary_name ->
  let client = Client.connect_local ~workspace () |> Result.expect ~msg:"Failed to start local tusk session" in
  let result =
    match Client.find_artifact client ~package:package_name ~kind:"binary" ~name:binary_name with
    | Ok path -> Ok (Path.v path)
    | Error err -> Error err
  in
  Client.close client;
  result

let install_temp_path = fun dst ->
  let dir = Path.dirname dst in
  let name = Path.basename dst in
  let pid = System.OsProcess.current_pid () in
  let nonce = Random.bits () in
  Path.(dir / Path.v ("." ^ name ^ ".install-" ^ Int.to_string pid ^ "-" ^ Int.to_string nonce))

let cleanup_temp_file = fun path ->
  match Fs.remove_file path with
  | Ok () -> ()
  | Error _ -> ()

let install_binary_atomically = fun ~src ~dst ~permissions ->
  let temp_path = install_temp_path dst in
  match Fs.copy ~src ~dst:temp_path with
  | Error err -> Error err
  | Ok () -> (
      match Fs.set_permissions temp_path permissions with
      | Error err ->
          cleanup_temp_file temp_path;
          Error err
      | Ok () -> (
          match Fs.rename ~src:temp_path ~dst with
          | Ok () -> Ok ()
          | Error err ->
              cleanup_temp_file temp_path;
              Error err
        )
    )

let run = fun ~workspace matches ->
  let open ArgParser in
    let started_at = Time.Instant.now () in
    let binary_name = get_one matches "package" |> Option.expect ~msg:"binary name required" in
    let local_only = get_flag matches "local" in
    out ("  \027[1;32mInstalling\027[0m " ^ binary_name);
    (* First, find which package contains this binary *)
    let client = Client.connect_local ~workspace () |> Result.expect ~msg:"Failed to start local tusk session" in
    match Client.find_executable client binary_name with
    | Ok (Some (package_name, _binary)) -> (
        Client.close client;
        match build_binary ~workspace package_name with
        | Error err -> Error err
        | Ok () ->
          let workspace_root = workspace.root in
          match find_built_binary_path ~workspace ~package_name ~binary_name with
          | Error _ ->
              out ("\027[1;31mError\027[0m: binary '" ^ binary_name ^ "' was not produced by package '" ^ package_name ^ "'");
              out "Note: Only packages with binaries in [[bin]] can be installed";
              Error (Failure ("Binary not found: " ^ binary_name))
          | Ok binary_path ->
              let perms = Fs.Permissions.executable in
              (* Always promote to project root *)
              let project_binary = Path.(workspace_root / Path.v binary_name) in
              (
                match install_binary_atomically ~src:binary_path ~dst:project_binary ~permissions:perms with
                | Ok () ->
                    out
                      ("    \027[1;32mPromoted\027[0m "
                      ^ binary_name
                      ^ " to "
                      ^ display_path ~workspace_root project_binary)
                | Error _ ->
                    out
                      ("\027[1;33mWarning\027[0m: failed to promote "
                      ^ binary_name
                      ^ " to "
                      ^ display_path ~workspace_root project_binary)
              );
              (* If not --local, also install to ~/.tusk/bin *)
              (
                if not local_only then
                  let tusk_bin_dir = Path.(Tusk_model.Tusk_dirs.dot_tusk / Path.v "bin") in
                  let _ = Fs.create_dir_all tusk_bin_dir |> Result.expect ~msg:"Failed to create ~/.tusk/bin" in
                  let dest_path = Path.(tusk_bin_dir / Path.v binary_name) in
                  match install_binary_atomically ~src:binary_path ~dst:dest_path ~permissions:perms with
                  | Ok () ->
                      out
                        ("    \027[1;32mPromoted\027[0m "
                        ^ binary_name
                        ^ " to "
                        ^ display_path ~workspace_root dest_path)
                  | Error _ ->
                      out
                        ("\027[1;33mWarning\027[0m: failed to promote "
                        ^ binary_name
                        ^ " to "
                        ^ display_path ~workspace_root dest_path
                        ^ " (non-fatal)")
              );
              let duration = Time.Instant.duration_since ~earlier:started_at (Time.Instant.now ()) in
              out
                ("   \027[1;32mInstalled\027[0m "
                ^ binary_name
                ^ " in "
                ^ Time.Duration.to_secs_string ~precision:2 duration
                ^ "s");
              if not local_only then
                (
                  out "";
                  out
                    ("To use " ^ binary_name ^ " from anywhere, add ~/.tusk/bin to your PATH:");
                  out "  export PATH='$HOME/.tusk/bin:$PATH'"
                );
              Ok ()
      )
    | Ok None ->
        Client.close client;
        out ("\027[1;31mError\027[0m: binary '" ^ binary_name ^ "' not found in workspace");
        out "Note: Make sure the binary is declared in a [[bin]] section";
        Error (Failure ("Binary not found: " ^ binary_name))
    | Error msg ->
        Client.close client;
        out ("\027[1;31mError\027[0m: " ^ msg);
        Error (Failure msg)
