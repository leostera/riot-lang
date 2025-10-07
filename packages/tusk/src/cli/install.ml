open Std
open Core
open Model
open Server

let command =
  let open ArgParser in
  let open Arg in
  command "install"
  |> about "Install a binary to ~/.tusk/bin"
  |> args [ positional "package" |> help "Package name to install" ]

let build_package package_name =
  let cwd = Env.current_dir () |> Result.expect ~msg:"Operation failed" in
  let workspace =
    Workspace_manager.scan cwd |> Result.expect ~msg:"Operation failed"
  in
  let client_result = Server.Server_manager.ensure_running ~workspace in
  if Result.is_error client_result then false
  else
    let client = client_result |> Result.expect ~msg:"Operation failed" in
    let displayed_packages = Hashtbl.create 32 in
    let result =
      Tusk_jsonrpc.Client.build_streaming client
        (Tusk_jsonrpc.Client.BuildPackage package_name) (function
        | Tusk_jsonrpc.Client.BuildStarted session_id -> ()
        | Tusk_jsonrpc.Client.BuildEvent event ->
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
        | Tusk_jsonrpc.Client.BuildFinished _ -> ())
    in
    Tusk_jsonrpc.Client.close client;
    match result with
    | Ok (Tusk_jsonrpc.Client.BuildFinished (Ok ())) -> true
    | Ok (Tusk_jsonrpc.Client.BuildFinished (Error _)) -> false
    | Ok (Tusk_jsonrpc.Client.BuildStarted _ | Tusk_jsonrpc.Client.BuildEvent _)
      ->
        false
    | Error _ -> false

let run matches =
  let open ArgParser in
  let package_name =
    get_one matches "package" |> Option.expect ~msg:"package required"
  in
  println "📦 Installing %s..." package_name;

  println "Building %s..." package_name;
  if not (build_package package_name) then (
    println "\n❌ Failed to build %s, nothing was installed" package_name;
    Error (Failure (format "Failed to build %s" package_name)))
  else
    let root = Env.current_dir () |> Result.expect ~msg:"Failed to get cwd" in
    let possible_binary_paths =
      [
        Path.(root / Path.v "target/bootstrap" / Path.v package_name);
        Path.(
          root
          / Path.v "target/bootstrap/out"
          / Path.v (package_name ^ "/" ^ package_name));
        Path.(root / Path.v "target/debug" / Path.v package_name);
        Path.(
          root / Path.v "target/debug/out"
          / Path.v (package_name ^ "/" ^ package_name));
        Path.(
          root / Path.v "target/debug/out"
          / Path.v ("packages/" ^ package_name ^ "/" ^ package_name));
      ]
    in

    match
      List.find_opt
        (fun path -> Fs.exists path |> Result.unwrap_or ~default:false)
        possible_binary_paths
    with
    | None ->
        println "❌ Binary for %s not found after build" package_name;
        println "Note: Only packages with main.ml produce installable binaries";
        Error (Failure (format "Binary not found for %s" package_name))
    | Some binary_path -> (
        let home =
          match Env.home_dir () with
          | Some h -> h
          | None -> failwith "HOME not set"
        in
        let tusk_bin_dir = Path.(home / Path.v ".tusk/bin") in
        let _ =
          Fs.create_dir_all tusk_bin_dir
          |> Result.expect ~msg:"Failed to create ~/.tusk/bin"
        in

        let dest_path = Path.(tusk_bin_dir / Path.v package_name) in
        match Fs.copy ~src:binary_path ~dst:dest_path with
        | Ok () ->
            let perms = Fs.Permissions.executable in
            ignore (Fs.set_permissions dest_path perms);

            println "✅ Installed %s to %s" package_name
              (Path.to_string dest_path);
            println "";
            println "To use %s from anywhere, add ~/.tusk/bin to your PATH:"
              package_name;
            println "  export PATH='$HOME/.tusk/bin:$PATH'";
            Ok ()
        | _ ->
            println "❌ Failed to install %s" package_name;
            Error (Failure (format "Failed to install %s" package_name)))
