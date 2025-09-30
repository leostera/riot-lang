open Std
open Core
open Model
open Server

(** Build a package and wait for completion *)
let build_package package_name =
  (* Get workspace *)
  let cwd = Env.current_dir () |> Result.expect ~msg:"Operation failed" in
  let workspace =
    Workspace_manager.scan cwd |> Result.expect ~msg:"Operation failed"
  in
  (* Ensure server is running *)
  let client_result = Server.Server_manager.ensure_running ~workspace in
  if Result.is_error client_result then false
  else
    (* Use JSON-RPC client to send build request *)
    let client = client_result |> Result.expect ~msg:"Operation failed" in
    (* Track packages we've already displayed to avoid duplicates *)
    let displayed_packages = Hashtbl.create 32 in
    let result =
      Tusk_jsonrpc.Client.build_streaming client
        (Tusk_jsonrpc.Client.BuildPackage package_name) (function
        | Tusk_jsonrpc.Client.BuildStarted session_id ->
            (* Don't print session ID in Cargo style *)
            ()
        | Tusk_jsonrpc.Client.BuildEvent event ->
            (* Only display package events once *)
            let should_display =
              match event.kind with
              | CacheHit { package; _ } | CacheMiss { package; _ } ->
                  if Hashtbl.mem displayed_packages package then false
                  else (
                    Hashtbl.add displayed_packages package ();
                    true)
              | PackageComplete { package; success; errors; _ } ->
                  (* Always show failures with errors, but not successes or skips *)
                  success = false && errors <> []
              | _ -> true
            in
            if should_display then
              let formatted = Event_formatter.format event in
              if formatted <> "" then (
                Printf.printf "%s\n%!" formatted;
                flush stdout)
        | Tusk_jsonrpc.Client.BuildFinished _ ->
            (* This is handled below in the result match *)
            ())
    in
    Tusk_jsonrpc.Client.close client;
    match result with
    | Ok (Tusk_jsonrpc.Client.BuildFinished (Ok ())) -> true
    | Ok (Tusk_jsonrpc.Client.BuildFinished (Error _)) -> false
    | Ok (Tusk_jsonrpc.Client.BuildStarted _ | Tusk_jsonrpc.Client.BuildEvent _)
      ->
        false
    | Error _ -> false

(** Execute the install command *)
let run args =
  if List.length args < 1 then (
    Printf.eprintf "Error: Package name required\n";
    Printf.eprintf "Usage: tusk install <package>\n";
    Error (Failure "Package name required"))
  else
    let package_name = List.nth args 0 in
    Printf.printf "📦 Installing %s...\n%!" package_name;

    (* First, build the package *)
    Printf.printf "Building %s...\n%!" package_name;
    if not (build_package package_name) then (
      Printf.eprintf "\n❌ Failed to build %s, nothing was installed\n"
        package_name;
      Error (Failure (Printf.sprintf "Failed to build %s" package_name)))
    else
      (* Look for the binary in various locations *)
      let root = Env.current_dir () |> Result.expect ~msg:"Failed to get cwd" in
      let possible_binary_paths =
        [
          (* Bootstrap location *)
          Path.(root / Path.v "target/bootstrap" / Path.v package_name);
          Path.(
            root / Path.v "target/bootstrap/out"
            / Path.v (package_name ^ "/" ^ package_name));
          (* Debug location *)
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
          Printf.eprintf "❌ Binary for %s not found after build\n" package_name;
          Printf.eprintf
            "Note: Only packages with main.ml produce installable binaries\n";
          Error
            (Failure (Printf.sprintf "Binary not found for %s" package_name))
      | Some binary_path -> (
          (* Create ~/.tusk/bin if it doesn't exist *)
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

          (* Copy the binary to ~/.tusk/bin *)
          let dest_path = Path.(tusk_bin_dir / Path.v package_name) in
          let cp_cmd =
            Printf.sprintf "cp %s %s" (Path.to_string binary_path)
              (Path.to_string dest_path)
          in
          match Command.of_unix_status (Command.system cp_cmd) with
          | Command.Exited 0 ->
              (* Make it executable *)
              let chmod_cmd =
                Printf.sprintf "chmod +x %s" (Path.to_string dest_path)
              in
              ignore (Command.system chmod_cmd);

              Printf.printf "✅ Installed %s to %s\n%!" package_name
                (Path.to_string dest_path);
              Printf.printf "\n%!";
              Printf.printf
                "To use %s from anywhere, add ~/.tusk/bin to your PATH:\n%!"
                package_name;
              Printf.printf "  export PATH='$HOME/.tusk/bin:$PATH'\n%!";
              Ok ()
          | _ ->
              Printf.eprintf "❌ Failed to install %s\n" package_name;
              Error
                (Failure (Printf.sprintf "Failed to install %s" package_name)))
