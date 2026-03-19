open Std
open Std.Collections
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
  let (workspace, _load_errors) =
    Workspace_manager.scan cwd |> Result.expect ~msg:"Operation failed"
  in
  let client_result = Local_session.connect_local ~workspace in
  if Result.is_error client_result then false
  else
    let client = client_result |> Result.expect ~msg:"Operation failed" in
    let displayed_packages = HashMap.create () in
    let result =
      Local_session.build_streaming client
        (Local_session.BuildPackage package_name)
        (function
        | Local_session.BuildStarted session_id -> ()
        | Local_session.BuildEvent _event -> ()
        | Local_session.BuildCompleted _ -> ()
        | Local_session.BuildFailed _ -> ()
        | Local_session.PlanningFailed _ -> ()
        | Local_session.CycleDetected _ -> ())
    in
    Local_session.close client;
    match result with
    | Ok (Local_session.BuildCompleted _) -> true
    | Ok (Local_session.BuildFailed _) -> false
    | Ok (Local_session.BuildStarted _ | Local_session.BuildEvent _) -> false
    | Ok (Local_session.PlanningFailed _ | Local_session.CycleDetected _) ->
        false
    | Error _ -> false

let run matches =
  let open ArgParser in
  let binary_name =
    get_one matches "package" |> Option.expect ~msg:"binary name required"
  in
  let local_only = get_flag matches "local" in

  println ("📦 Installing " ^ binary_name ^ "...");

  (* First, find which package contains this binary *)
  let cwd = Env.current_dir () |> Result.expect ~msg:"Failed to get cwd" in
  let (workspace, _load_errors) =
    Workspace_manager.scan cwd |> Result.expect ~msg:"Failed to scan workspace"
  in
  let client =
    Local_session.connect_local ~workspace
    |> Result.expect ~msg:"Failed to start local tusk session"
  in

  match Local_session.find_executable client binary_name with
  | Ok (Some (package_name, _binary)) -> (
      Local_session.close client;
      println
        ("Building " ^ binary_name ^ " (from package " ^ package_name ^ ")...");
      if not (build_package package_name) then (
        println
          ("\n❌ Failed to build " ^ binary_name ^ ", nothing was installed");
        Error (Failure ("Failed to build " ^ binary_name)))
      else
        let workspace_root = workspace.root in
        let build_root =
          Tusk_model.Tusk_dirs.build_dir_root ~workspace_root
        in
        let debug_out = Tusk_model.Tusk_dirs.out_dir ~workspace_root in
        let possible_binary_paths =
          [
            Path.(build_root / Path.v "bootstrap" / Path.v binary_name);
            Path.(
              build_root / Path.v "bootstrap/out"
              / Path.v (package_name ^ "/" ^ binary_name));
            Path.(build_root / Path.v "debug" / Path.v binary_name);
            Path.(
              debug_out
              / Path.v (package_name ^ "/" ^ binary_name));
            Path.(
              debug_out
              / Path.v ("packages/" ^ package_name ^ "/" ^ binary_name));
          ]
        in

        match
          List.find_opt
            (fun path -> Fs.exists path |> Result.unwrap_or ~default:false)
            possible_binary_paths
        with
        | None ->
            println ("❌ Binary " ^ binary_name ^ " not found after build");
            println
              "Note: Only packages with binaries in [[bin]] can be installed";
            Error (Failure ("Binary not found: " ^ binary_name))
        | Some binary_path ->
            let perms = Fs.Permissions.executable in

            (* Always promote to project root *)
            let project_binary = Path.(workspace_root / Path.v binary_name) in
            (match Fs.copy ~src:binary_path ~dst:project_binary with
            | Ok () ->
                ignore (Fs.set_permissions project_binary perms);
                println
                  ("✅ Promoted " ^ binary_name ^ " to "
                  ^ Path.to_string project_binary)
            | Error _ -> println "⚠️  Failed to promote to project root");

            (* If not --local, also install to ~/.tusk/bin *)
            (if not local_only then
               let tusk_bin_dir =
                 Path.(Tusk_model.Tusk_dirs.dot_tusk / Path.v "bin")
               in
               let _ =
                 Fs.create_dir_all tusk_bin_dir
                 |> Result.expect ~msg:"Failed to create ~/.tusk/bin"
               in

               let dest_path = Path.(tusk_bin_dir / Path.v binary_name) in
               match Fs.copy ~src:binary_path ~dst:dest_path with
               | Ok () ->
                   ignore (Fs.set_permissions dest_path perms);
                   println
                     ("✅ Installed " ^ binary_name ^ " to "
                     ^ Path.to_string dest_path);
                   println "";
                   println
                     ("To use " ^ binary_name
                     ^ " from anywhere, add ~/.tusk/bin to your PATH:");
                   println "  export PATH='$HOME/.tusk/bin:$PATH'"
               | Error _ ->
                   println "⚠️  Failed to install to ~/.tusk/bin (non-fatal)");

            Ok ())
  | Ok None ->
      Local_session.close client;
      println ("❌ Binary '" ^ binary_name ^ "' not found in workspace");
      println "Note: Make sure the binary is declared in a [[bin]] section";
      Error (Failure ("Binary not found: " ^ binary_name))
  | Error msg ->
      Local_session.close client;
      println ("❌ Error: " ^ msg);
      Error (Failure msg)
