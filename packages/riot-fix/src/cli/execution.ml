open Std

let fix_trace_enabled = fun () ->
  match Env.get Env.String ~var:"RIOT_FIX_TRACE" with
  | Some ("1" | "true" | "yes") -> true
  | _ -> false

let trace_fix = fun message ->
  if fix_trace_enabled () then
    eprintln ("[riot-fix] " ^ message)

let recommended_concurrency = fun ~limit ->
  let concurrency =
    let recommended = Thread.available_parallelism in
    if recommended <= 0 then
      1
    else
      recommended
  in
  match limit with
  | Some max_diagnostics -> Int.min concurrency max_diagnostics
  | None -> concurrency

let run_result_with = fun ~on_result ~mode ~scope ~limit ~files ->
  let concurrency = recommended_concurrency ~limit in
  let owner = self () in
  let _coordinator = Coordinator.start
    {
      input = Coordinator.Files files;
      concurrency;
      limit;
      mode;
      scope;
      owner;
    }
  in
  let rec loop results_rev diagnostics_seen limit_reached =
    let selector = function
      | Messages.FileResult result -> `select (`FileResult result)
      | Messages.AllComplete summary -> `select (`AllComplete summary)
      | _ -> `skip
    in
    match receive ~selector () with
    | `FileResult { Messages.result; _ } ->
        let remaining_budget =
          Option.map limit ~fn:(fun max_diagnostics -> max_diagnostics - diagnostics_seen)
        in
        let result =
          match remaining_budget with
          | None -> result
          | Some remaining -> Common.clip_result_to_limit remaining result
        in
        let diagnostics_seen = diagnostics_seen + Common.diagnostic_count result in
        let limit_reached_now =
          match limit with
          | Some max_diagnostics when diagnostics_seen >= max_diagnostics -> true
          | _ -> false
        in
        on_result result;
        loop (result :: results_rev) diagnostics_seen (limit_reached || limit_reached_now)
    | `AllComplete _summary ->
        let files = List.reverse results_rev in
        let summary = Runner.summarize files in
        {
          Types.result =
            Runner.{ files; summary };
          limit_reached;
        }
  in
  loop [] 0 false

let run_result = fun ~mode ~scope ~limit ~files ->
  run_result_with ~mode ~scope ~limit ~files ~on_result:(fun _ -> ())

let run_with_coordinator = fun ?(on_event = Types.no_event) ~output_mode ~mode ~scope ~limit ~roots () ->
  let concurrency = recommended_concurrency ~limit in
  on_event (Types.Start { mode; concurrency });
  (
    match output_mode with
    | Types.Silent -> ()
    | Types.Report Reporter.Text -> eprintln
      ("Scanning with " ^ Int.to_string concurrency ^ " workers...")
    | Types.Report Reporter.Json -> Reporting.print_json_event
      (Event.to_json (Types.Start { mode; concurrency }))
  );
  let outcome =
    let owner = self () in
    let _coordinator = Coordinator.start
      {
        input = Coordinator.Roots roots;
        concurrency;
        limit;
        mode;
        scope;
        owner;
      }
    in
    let rec loop results_rev diagnostics_seen limit_reached =
      let selector = function
        | Messages.FileStarted file -> `select (`FileStarted file)
        | Messages.FileProgress progress -> `select (`FileProgress progress)
        | Messages.FileResult result -> `select (`FileResult result)
        | Messages.AllComplete summary -> `select (`AllComplete summary)
        | _ -> `skip
      in
      match receive ~selector () with
      | `FileStarted file ->
          on_event (Types.FileStarted { file });
          (
            match output_mode with
            | Types.Silent -> ()
            | Types.Report Reporter.Text -> ()
            | Types.Report Reporter.Json -> Reporting.print_json_event
              (Event.to_json (Types.FileStarted { file }))
          );
          loop results_rev diagnostics_seen limit_reached
      | `FileProgress { Messages.file; event; _ } ->
          on_event (Types.FileProgress { file; progress = event });
          (
            match output_mode with
            | Types.Report Reporter.Json -> Reporting.print_json_event
              (Event.to_json (Types.FileProgress { file; progress = event }))
            | Types.Silent
            | Types.Report Reporter.Text -> ()
          );
          loop results_rev diagnostics_seen limit_reached
      | `FileResult { Messages.result; _ } ->
          let remaining_budget =
            Option.map limit ~fn:(fun max_diagnostics -> max_diagnostics - diagnostics_seen)
          in
          let result =
            match remaining_budget with
            | None -> result
            | Some remaining -> Common.clip_result_to_limit remaining result
          in
          let diagnostics_seen = diagnostics_seen + Common.diagnostic_count result in
          let limit_reached_now =
            match limit with
            | Some max_diagnostics when diagnostics_seen >= max_diagnostics -> true
            | _ -> false
          in
          on_event (Types.FileResult result);
          (
            match output_mode with
            | Types.Silent -> ()
            | Types.Report Reporter.Json -> Reporting.print_json_event
              (Event.to_json (Types.FileResult result))
            | Types.Report Reporter.Text -> Reporting.print_text_result mode result
          );
          loop (result :: results_rev) diagnostics_seen (limit_reached || limit_reached_now)
      | `AllComplete _summary ->
          let files = List.reverse results_rev in
          let summary = Runner.summarize files in
          {
            Types.result =
              Runner.{ files; summary };
            limit_reached;
          }
    in
    loop [] 0 false
  in
  (
    on_event
      (Types.Summary { summary = outcome.result.summary; limit_reached = outcome.limit_reached });
    match output_mode with
    | Types.Silent ->
        ()
    | Types.Report Reporter.Json ->
        Reporting.print_json_event
          (Event.to_json
            (Types.Summary {
              summary = outcome.result.summary;
              limit_reached = outcome.limit_reached
            }))
    | Types.Report Reporter.Text ->
        if outcome.result.summary.total_files = 0 then
          println "No OCaml files found."
        else (
          if outcome.limit_reached then
            (
              println "";
              println
                ("\027[1;33m!\027[0m Reached diagnostic limit "
                ^ (limit |> Option.map ~fn:Int.to_string |> Option.unwrap_or ~default:"0")
                ^ "; stopped early")
            );
          Reporting.print_text_summary mode outcome.result.summary
        )
  );
  if outcome.result.summary.failed_files > 0 || outcome.result.summary.remaining_diagnostics > 0 then
    Error (Failure "Issues remain after riot fix")
  else
    Ok ()

let absolute_target = fun ~cwd target ->
  if Path.is_absolute target then
    Path.normalize target
  else
    Path.normalize Path.(cwd / target)

let generated_runner_args = fun ~cwd ~mode ~limit ~target ~output_mode ->
  let args = ref [] in
  let push item =
    args := !args @ [ item ]
  in
  let push_pair name value =
    args := !args @ [ name; value ]
  in
  (
    match mode with
    | Runner.Apply -> push "--apply"
    | Runner.Check -> push "--check"
  );
  (
    match output_mode with
    | Types.Report Reporter.Json -> push "--json"
    | Types.Report Reporter.Text
    | Types.Silent -> ()
  );
  (
    match limit with
    | Some limit -> push_pair "--limit" (Int.to_string limit)
    | None -> ()
  );
  push (Path.to_string (absolute_target ~cwd target));
  !args

let run_generated_runner = fun ~cwd ~(build_package:Types.build_package) ~report_output ~mode ~limit ~target ~output_mode scope ->
  let workspace = Fix_config.workspace scope in
  let workspace_root = Fix_config.workspace_root scope in
  let target_dir_root = Fix_config.target_dir_root scope in
  let providers = Fix_config.providers (Some scope) in
  trace_fix
    ("materializing generated runner for " ^ Int.to_string (List.length providers) ^ " providers");
  let plan = Fixme_runner.materialize ~workspace_root ~target_dir_root providers in
  let args = generated_runner_args ~cwd ~mode ~limit ~target ~output_mode in
  trace_fix
    ("building generated runner package " ^ Riot_model.Package_name.to_string plan.package_name);
  match
    build_package ~workspace ~package_name:plan.package_name ~profile:"release"
      ~transform_workspace:(fun workspace ->
        Fixme_runner.attach_to_workspace workspace plan)
      ()
  with
  | Error err ->
      trace_fix ("building generated runner failed: " ^ Kernel.Exception.to_string err);
      Error err
  | Ok () ->
      let command = Command.make (Path.to_string plan.binary_path) ~cwd:(Path.to_string cwd) ~args in
      trace_fix ("running generated runner " ^ Path.to_string plan.binary_path);
      if report_output then
        match Command.status command with
        | Ok status when Int.equal status 0 ->
            trace_fix "generated runner exited with status 0";
            Ok ()
        | Ok status ->
            trace_fix ("generated runner exited with status " ^ Int.to_string status);
            Error (Failure "Issues remain after riot fix")
        | Error (Command.SystemError error) ->
            Error (Failure error)
      else
        match Command.output command with
        | Ok output when Int.equal output.status 0 ->
            trace_fix "generated runner exited with status 0";
            Ok ()
        | Ok output ->
            trace_fix ("generated runner exited with status " ^ Int.to_string output.status);
            Error (Failure "Issues remain after riot fix")
        | Error (Command.SystemError error) ->
            Error (Failure error)
