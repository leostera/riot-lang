open Std

type config = {
  mode : Runner.mode;
  scope : Fix_config.scope option;
  coordinator : Pid.t;
}

let run_file config file_path =
  try
    Runner.run_file
      ~pipeline_for_file:(Fix_config.pipeline_for_file config.scope)
      ~mode:config.mode file_path
  with exn ->
    Runner.
      {
        file = file_path;
        final_source = "";
        diagnostics = [];
        parse_diagnostics = [];
        applied_fixes = [];
        changed = false;
        error = Some (Exception.to_string exn);
      }

let rec worker_loop config =
  send config.coordinator (Messages.WorkerReady (self ()));
  let selector = function
    | Messages.RunTask file -> `select file
    | _ -> `skip
  in

  match receive ~selector () with
  | file_path ->
      let result = run_file config file_path in
      send config.coordinator (Messages.FileResult { worker = self (); result });
      worker_loop config

let init config () = worker_loop config

let start config = spawn (init config)
