open Std
open Riot_model

let command = let open ArgParser in command "clean" |> about "Clean build artifacts"

let run = fun _matches ->
  let cwd = Env.current_dir () |> Result.expect ~msg:"Failed to get current directory" in
  let workspace_manager = Workspace_manager.create () in
  let (workspace, _load_errors) = Workspace_manager.scan workspace_manager cwd
  |> Result.expect ~msg:"Failed to scan workspace. Is this a valid riot project?" in
  let build_dir = Riot_dirs.build_dir_root ~workspace_root:workspace.root in
  println ("🧹 Cleaning build artifacts in " ^ Path.to_string build_dir ^ "...");
  match Fs.exists build_dir with
  | Ok false ->
      println "Nothing to clean.";
      Ok ()
  | Ok true -> (
      match Fs.remove_dir_all build_dir with
      | Ok () ->
          println "Build artifacts cleaned!";
          Ok ()
      | Error _ -> Error (Failure "Failed to clean build artifacts")
    )
  | Error _ ->
      Error (Failure "Failed to check build directory")
