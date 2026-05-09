open Std

type error =
  | ScanFailed of Riot_model.Workspace_manager.scan_error
  | LoadErrors of Riot_model.Workspace_manager.load_error list

let error_message = fun __tmp1 ->
  match __tmp1 with
  | ScanFailed error -> Riot_model.Workspace_manager.scan_error_message error
  | LoadErrors errors ->
      errors
      |> List.map ~fn:Riot_model.Workspace_manager.load_error_to_string
      |> String.concat "\n"

let realize = fun (workspace: Riot_model.Workspace_manifest.t) ->
  match workspace.target_dir with
  | Some target_dir ->
      Riot_model.Workspace.make
        ?name:workspace.name
        ~root:workspace.root
        ~packages:workspace.packages
        ~dependencies:workspace.dependencies
        ~dev_dependencies:workspace.dev_dependencies
        ~build_dependencies:workspace.build_dependencies
        ~profile_overrides:workspace.profile_overrides
        ~source_ignore_patterns:workspace.source_ignore_patterns
        ~target_dir
        ()
  | None ->
      Riot_model.Workspace.make
        ?name:workspace.name
        ~root:workspace.root
        ~packages:workspace.packages
        ~dependencies:workspace.dependencies
        ~dev_dependencies:workspace.dev_dependencies
        ~build_dependencies:workspace.build_dependencies
        ~profile_overrides:workspace.profile_overrides
        ~source_ignore_patterns:workspace.source_ignore_patterns
        ()

let load_local = fun ~root ->
  let manager = Riot_model.Workspace_manager.create () in
  match Riot_model.Workspace_manager.scan manager root with
  | Error error -> Error (ScanFailed error)
  | Ok (workspace, load_errors) -> (
      match load_errors with
      | [] -> Ok (realize workspace)
      | errors -> Error (LoadErrors errors)
    )
