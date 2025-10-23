open Std
open Tusk_model
open Tusk_model
open Tusk_server

let command =
  let open ArgParser in
  let open Arg in
  command "install"
  |> about "Install a binary to ~/.tusk/bin and project root"
  |> args
       [
         positional "package" |> help "Binary name to install";
         flag "local" |> long "local"
         |> help "Only install to project root, skip ~/.tusk/bin";
       ]

let build_package package_name =
  let cwd = Env.current_dir () |> Result.expect ~msg:"Operation failed" in
  let workspace =
    Workspace_manager.scan cwd |> Result.expect ~msg:"Operation failed"
  in
  let client_result = Tusk_server.Server_manager.ensure_running ~workspace in
  if Result.is_error client_result then false
  else
    let client = client_result |> Result.expect ~msg:"Operation failed" in
    let displayed_packages = Hashtbl.create 32 in
    let result =
      Tusk_client.build_streaming client (Tusk_client.BuildPackage package_name)
        (function
        | Tusk_client.BuildStarted session_id -> ()
        | Tusk_client.BuildEvent event ->
            let should_display =
              match event.kind with
              | CacheHit { package; _ } | CacheMiss { package; _ } ->
                  if Hashtbl.mem displayed_packages package then false
                  else (
                    Hashtbl.add displayed_packages package ();
                    true)
              | PackageComplete { package; success; errors; _ } ->
                  success = false && errors <> []
              | _ -> true
            in
            if should_display then
              let formatted = Event_formatter.format event in
              if formatted <> "" then println "%s" formatted
        | Tusk_client.BuildFinished _ -> ())
    in
    Tusk_client.close client;
    match result with
    | Ok (Tusk_client.BuildFinished (Ok ())) -> true
    | Ok (Tusk_client.BuildFinished (Error _)) -> false
    | Ok (Tusk_client.BuildStarted _ | Tusk_client.BuildEvent _) -> false
    | Error _ -> false

let run matches =
  let open ArgParser in
  let binary_name =
    get_one matches "package" |> Option.expect ~msg:"binary name required"
  in
  let local_only = get_flag matches "local" in

  println "📦 Installing %s..." binary_name;

  (* First, find which package contains this binary *)
  let cwd = Env.current_dir () |> Result.expect ~msg:"Failed to get cwd" in
  let workspace =
    Workspace_manager.scan cwd |> Result.expect ~msg:"Failed to scan workspace"
  in
  let client =
    Tusk_server.Server_manager.ensure_running ~workspace
    |> Result.expect ~msg:"Failed to start or connect to tusk server"
  in

  match Tusk_client.find_executable client binary_name with
  | Ok (Some (package_name, _binary)) -> (
      Tusk_client.close client;
      println "Building %s (from package %s)..." binary_name package_name;
      if not (build_package package_name) then (
        println "\n❌ Failed to build %s, nothing was installed" binary_name;
        Error (Failure (format "Failed to build %s" binary_name)))
      else
        let root =
          Env.current_dir () |> Result.expect ~msg:"Failed to get cwd"
        in
        let possible_binary_paths =
          [
            Path.(root / Path.v "target/bootstrap" / Path.v binary_name);
            Path.(
              root
              / Path.v "target/bootstrap/out"
              / Path.v (package_name ^ "/" ^ binary_name));
            Path.(root / Path.v "target/debug" / Path.v binary_name);
            Path.(
              root / Path.v "target/debug/out"
              / Path.v (package_name ^ "/" ^ binary_name));
            Path.(
              root / Path.v "target/debug/out"
              / Path.v ("packages/" ^ package_name ^ "/" ^ binary_name));
          ]
        in

        match
          List.find_opt
            (fun path -> Fs.exists path |> Result.unwrap_or ~default:false)
            possible_binary_paths
        with
        | None ->
            println "❌ Binary %s not found after build" binary_name;
            println
              "Note: Only packages with binaries in [[bin]] can be installed";
            Error (Failure (format "Binary not found: %s" binary_name))
        | Some binary_path ->
            let perms = Fs.Permissions.executable in

            (* Always promote to project root *)
            let project_binary = Path.(root / Path.v binary_name) in
            (match Fs.copy ~src:binary_path ~dst:project_binary with
            | Ok () ->
                ignore (Fs.set_permissions project_binary perms);
                println "✅ Promoted %s to %s" binary_name
                  (Path.to_string project_binary)
            | Error _ -> println "⚠️  Failed to promote to project root");

            (* If not --local, also install to ~/.tusk/bin *)
            (if not local_only then
               let home =
                 match Env.home_dir () with
                 | Some h -> h
                 | None ->
                     println "⚠️  HOME not set, skipping global install";
                     Path.v "/tmp"
               in
               let tusk_bin_dir = Path.(home / Path.v ".tusk/bin") in
               let _ =
                 Fs.create_dir_all tusk_bin_dir
                 |> Result.expect ~msg:"Failed to create ~/.tusk/bin"
               in

               let dest_path = Path.(tusk_bin_dir / Path.v binary_name) in
               match Fs.copy ~src:binary_path ~dst:dest_path with
               | Ok () ->
                   ignore (Fs.set_permissions dest_path perms);
                   println "✅ Installed %s to %s" binary_name
                     (Path.to_string dest_path);
                   println "";
                   println
                     "To use %s from anywhere, add ~/.tusk/bin to your PATH:"
                     binary_name;
                   println "  export PATH='$HOME/.tusk/bin:$PATH'"
               | Error _ ->
                   println "⚠️  Failed to install to ~/.tusk/bin (non-fatal)");

            Ok ())
  | Ok None ->
      Tusk_client.close client;
      println "❌ Binary '%s' not found in workspace" binary_name;
      println "Note: Make sure the binary is declared in a [[bin]] section";
      Error (Failure (format "Binary not found: %s" binary_name))
  | Error msg ->
      Tusk_client.close client;
      println "❌ Error: %s" msg;
      Error (Failure msg)
