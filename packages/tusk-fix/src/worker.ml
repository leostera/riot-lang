open Std

type config = {
  mode: Runner.mode;
  scope: Fix_config.scope option;
  coordinator: Pid.t;
}

let run_file = fun config file_path ->
  try Runner.run_file
    ~pipeline_for_file:(Fix_config.pipeline_for_file config.scope)
    ~on_progress:(fun event ->
      send config.coordinator (Messages.FileProgress { worker = self (); file = file_path; event }))
    ~mode:config.mode
    file_path with
  | exn ->
      Runner.{
        file = file_path;
        final_source = "";
        diagnostics = [];
        parse_diagnostics = [];
        applied_fixes = [];
        changed = false;
        error = Some (Exception.to_string exn);
      }

let rec worker_loop = fun config ->
  send config.coordinator (Messages.WorkerReady (self ()));
  let selector = function
    | Messages.RunTask file -> `select (`RunTask file)
    | Messages.Stop -> `select `Stop
    | _ -> `skip
  in
  match receive ~selector () with
  | `Stop -> Ok ()
  | `RunTask file_path ->
      let result = run_file config file_path in
      send config.coordinator (Messages.FileResult { worker = self (); result });
      worker_loop config

let init = fun config () -> worker_loop config

let start = fun config -> spawn (init config)
