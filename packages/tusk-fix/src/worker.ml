open Std

type config = { pipeline : Pipeline.t; coordinator : Pid.t }

type lint_result = {
  file : Path.t;
  source : string;
  diagnostics : Diagnostic.t list;
}

let lint_file pipeline file_path =
  match Fs.read file_path with
  | Error _ -> { file = file_path; source = ""; diagnostics = [] }
  | Ok source -> (
      try
        let filename = Path.to_string file_path in
        let result = Pipeline.run pipeline ~filename source in
        { file = file_path; source; diagnostics = result.diagnostics }
      with exn ->
        let error_msg = "Parser error: " ^ Exception.to_string exn in
        let diag =
          Diagnostic.make ~severity:Error ~message:error_msg
            ~span:(Syn.Ceibo.Span.make ~start:0 ~end_:0)
            ~rule_id:"parser-error" ()
        in
        { file = file_path; source; diagnostics = [ diag ] })

let rec worker_loop config =
  (* Tell coordinator we're ready for work *)
  send config.coordinator (Messages.WorkerReady (self ()));

  (* Wait for a task *)
  let selector = function
    | Messages.LintTask file -> `select file
    | _ -> `skip
  in

  match receive ~selector () with
  | file_path -> (
      try
        let result = lint_file config.pipeline file_path in
        let msg_result : Messages.lint_result =
          {
            file = result.file;
            diagnostics = result.diagnostics;
            source = result.source;
          }
        in
        send config.coordinator (Messages.LintResult msg_result)
      with exn ->
        let failure : Messages.worker_failure =
          { file = file_path; worker = self (); reason = Exception.to_string exn }
        in
        send config.coordinator (Messages.WorkerFailed failure));
      worker_loop config

let init config () = worker_loop config

let start config = spawn (init config)
