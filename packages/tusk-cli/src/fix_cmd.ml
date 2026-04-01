open Std

let command = Tusk_fix.Cli.command

let current_dir = fun () -> Env.current_dir () |> Result.expect ~msg:"Failed to get current directory"

let set_current_dir = fun path ->
  Env.set_current_dir path
  |> Result.expect ~msg:(("Failed to change directory to " ^ Path.to_string path))

let with_current_dir = fun path fn ->
  let original = current_dir () in
  set_current_dir path;
  try
    let result = fn () in
    set_current_dir original;
    result
  with
  | exn ->
      set_current_dir original;
      raise exn

let build_package = fun ~workspace_root ~package_name ->
  with_current_dir workspace_root
    (fun () ->
      Build.build_command (Some package_name) None)

let run = fun matches -> Tusk_fix.Cli.run ~build_package matches
