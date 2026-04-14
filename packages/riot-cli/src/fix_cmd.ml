open Std

let command = Riot_fix.Cli.command

let build_mode_of_output_mode = function
  | Riot_fix.Report Riot_fix.Reporter.Json -> Build.Json
  | Riot_fix.Report Riot_fix.Reporter.Text
  | Riot_fix.Silent -> Build.Human

let prepare_workspace = fun (workspace: Riot_model.Workspace.t) ->
  match Pkgs_ml.Registry.create_filesystem ?riot_home:None ~registry_name:"pkgs.ml" () with
  | Error err -> Error (Failure err)
  | Ok registry -> (
      match Riot_deps.ensure_workspace ~mode:Riot_deps.Dep_solver.Refresh ~registry ~workspace () with
      | Ok workspace -> Ok workspace
      | Error err -> Error (Failure (Riot_model.Pm_error.message err))
    )

let build_package = fun ~mode ~(workspace:Riot_model.Workspace.t) ~package_name ~profile ?(transform_workspace = fun workspace ->
  workspace) () ->
  match prepare_workspace workspace with
  | Error _ as err -> err
  | Ok prepared_workspace ->
      Build.build_command
        ~prepared_workspace:(Riot_build.Prepared_workspace.of_workspace (transform_workspace prepared_workspace))
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
                  | Riot_fix.ListedRules { format=Riot_fix.Reporter.Text; _ }
                  | Riot_fix.ListedDiagnostics { format=Riot_fix.Reporter.Text; _ }
                  | Riot_fix.ExplainedRule _ -> print "\n"
                  | Riot_fix.ListedRules { format=Riot_fix.Reporter.Json; _ }
                  | Riot_fix.ListedDiagnostics { format=Riot_fix.Reporter.Json; _ }
                  | Riot_fix.Completed -> ()
                )
            | None -> ()
          );
          Ok ()
