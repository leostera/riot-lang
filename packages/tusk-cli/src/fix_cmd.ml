open Std

let command = Tusk_fix.Cli.command

let current_dir () =
  Env.current_dir () |> Result.expect ~msg:"Failed to get current directory"

let set_current_dir path =
  Env.set_current_dir path
  |> Result.expect ~msg:("Failed to change directory to " ^ Path.to_string path)

let with_current_dir path fn =
  let original = current_dir () in
  set_current_dir path;
  try
    let result = fn () in
    set_current_dir original;
    result
  with exn ->
    set_current_dir original;
    raise exn

let raw_fix_args () =
  match Env.args with
  | _binary :: "fix" :: rest -> rest
  | _binary :: rest -> rest
  | [] -> []

let build_fixme_runner scope =
  let workspace_root = Tusk_fix.Config.workspace_root scope in
  let target_dir_root = Tusk_fix.Config.target_dir_root scope in
  let providers = Tusk_fix.Config.providers (Some scope) in
  let plan =
    Tusk_fix.Fixme_runner.materialize ~workspace_root ~target_dir_root providers
  in
  let result =
    with_current_dir plan.workspace_root (fun () ->
        Build.build_command (Some plan.package_name) None)
  in
  match result with
  | Ok () -> Ok plan.binary_path
  | Error _ as err -> err

let run matches =
  let cwd = current_dir () in
  match Tusk_fix.Config.load_scope ~cwd with
  | Some scope when List.length (Tusk_fix.Config.providers (Some scope)) > 0 ->
      let args = raw_fix_args () in
      let command_binary =
        build_fixme_runner scope
        |> Result.expect ~msg:"Failed to build fixme runner"
      in
      Command_executor.execute ~command_binary ~args
  | _ -> Tusk_fix.Cli.run matches
