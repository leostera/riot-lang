open Std

let command = Riot_fix.Cli.command

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

let build_mode_of_output_mode = function
  | Riot_fix.Report Riot_fix.Reporter.Json -> Build.Json
  | Riot_fix.Report Riot_fix.Reporter.Text
  | Riot_fix.Silent -> Build.Human

let build_package = fun ~mode ~workspace_root ~package_name ~profile ->
  with_current_dir
    workspace_root
    (fun () -> Build.build_command ~mode ~profile (Some package_name) None)

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
