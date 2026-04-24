open Std
open Riot_model
open Std.Result.Syntax

type package_kind = Riot_init_types.package_kind =
  | Library
  | Binary

type event = Riot_init_types.event =
  | WorkspaceInitializationStarted of { name: string; target_dir: Path.t }
  | ScaffoldCreated of { path: string }
  | WorkspaceInitializationCompleted of {
      next_steps: string list;
      package_hints: (package_kind * string) list
    }

let command =
  let open ArgParser in
    let open Arg in command "init"
    |> about "Initialize a new Riot workspace"
    |> args
      [
        positional "path" |> required false |> help "Path for new workspace (default: current directory)";
        flag "name" |> long "name" |> short 'n' |> help "Workspace name (default: directory basename)";
        flag "lib" |> long "lib" |> help "Create library package (default)";
        flag "bin" |> long "bin" |> help "Create binary package";
      ]

let new_package = Riot_init_package.new_package

let new_standalone_package = Riot_init_package.new_standalone_package

let next_steps = fun ~cwd ~target_dir ~path_arg ~is_library ~package_name ->
  let steps = ref [] in
  if Option.is_some path_arg && not (Path.equal cwd target_dir) then
    steps := !steps @ [ "cd " ^ Path.to_string target_dir ];
  steps := !steps @ [ "riot build"; "riot test" ];
  if not is_library then
    steps := !steps @ [ "riot run " ^ package_name ];
  !steps

let resolve_target_dir = fun ~cwd path_arg ->
  let resolved =
    match path_arg with
    | Some path ->
        let path = Path.v path in
        if Path.is_absolute path then
          path
        else
          Path.(cwd / path)
    | None -> cwd
  in
  Path.normalize resolved

let run = fun ~on_event matches ->
  let open ArgParser in
    let path_arg = get_one matches "path" in
    let name_flag = get_one matches "name" in
    let cwd = Env.current_dir () |> Result.expect ~msg:"Cannot get current directory" in
    let is_library = not (get_flag matches "bin") in
    let target_dir = resolve_target_dir ~cwd path_arg in
    let workspace_name =
      match name_flag with
      | Some name -> name
      | None -> Path.basename target_dir
    in
    let* workspace_name = Riot_init_names.validate_workspace_name workspace_name in
    let package_name = Riot_init_names.starter_package_name workspace_name in
    let* validated_package_name = Riot_init_names.validate_name package_name in
    let validated_package_name = Package_name.to_string validated_package_name in
    let* () =
      match Fs.create_dir_all target_dir with
      | Ok () -> Ok ()
      | Error _ -> Error (Failure "Failed to create directory")
    in
    Riot_init_types.emit
      ~on_event
      (WorkspaceInitializationStarted { name = workspace_name; target_dir });
    let* () =
      Templates.materialize
        Templates.{
          on_event;
          target_dir;
          workspace_name;
          package_name = validated_package_name;
          is_library;
        } |> Result.map_err ~fn:(fun message -> Failure message)
    in
    Riot_init_types.emit
      ~on_event
      (WorkspaceInitializationCompleted {
        next_steps = next_steps ~cwd ~target_dir ~path_arg ~is_library ~package_name:validated_package_name;
        package_hints = Riot_init_types.package_hints
      });
    Ok ()
