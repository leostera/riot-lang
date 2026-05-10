open Std
open Std.Result.Syntax

let command = Riot_fix.Cli.command

let build_mode_of_output_mode = fun __tmp1 ->
  match __tmp1 with
  | Riot_fix.Report Riot_fix.Reporter.Json -> Ui.Json
  | Riot_fix.Report Riot_fix.Reporter.Text
  | Riot_fix.Silent -> Ui.Line

let prepare_workspace = fun (workspace: Riot_model.Workspace_manifest.t) ->
  let workspace_manager = Riot_model.Workspace_manager.create () in
  let* registry =
    Pkgs_ml.Registry.create_filesystem ?riot_home:None ~registry_name:"pkgs.ml" ()
    |> Result.map_err ~fn:(fun err -> Failure (Pkgs_ml.Registry_cache.create_error_message err))
  in
  Riot_deps.ensure_workspace
    ~workspace_manager
    ~mode:Riot_deps.Dep_solver.Refresh
    ~registry
    ~workspace
    ()
  |> Result.map_err ~fn:(fun err -> Failure (Riot_model.Pm_error.message err))

let build_package = fun
  ~mode
  ~(workspace:Riot_model.Workspace_manifest.t)
  ~package_name
  ~profile
  ?(transform_workspace = fun workspace -> workspace)
  () ->
  let* workspace = prepare_workspace workspace in
  Build.build_command
    ~workspace:(transform_workspace workspace)
    ~mode
    ~profile
    (Some package_name)
    None

let run = fun matches ->
  match Riot_fix.fix_request_of_matches matches with
  | Error _ as err -> err
  | Ok request ->
      let output_mode = Riot_fix.output_mode_of_request request in
      match Riot_fix.fix
        ~build_package:(build_package ~mode:(build_mode_of_output_mode output_mode))
        ~output_mode
        request with
      | Error _ as err -> err
      | Ok response ->
          (
            match Riot_fix.response_output response with
            | Some output ->
                print output;
                (
                  match response with
                  | Riot_fix.ListedRules { format = Riot_fix.Reporter.Text; _ }
                  | Riot_fix.ListedDiagnostics { format = Riot_fix.Reporter.Text; _ }
                  | Riot_fix.ExplainedRule _ -> print "\n"
                  | Riot_fix.ListedRules { format = Riot_fix.Reporter.Json; _ }
                  | Riot_fix.ListedDiagnostics { format = Riot_fix.Reporter.Json; _ }
                  | Riot_fix.Completed -> ()
                )
            | None -> ()
          );
          Ok ()
