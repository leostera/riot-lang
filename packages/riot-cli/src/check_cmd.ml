open Std
module Check = Riot_check.Check

let yellow_bold = "\027[1;33m"

let reset = "\027[0m"

let monotonic_now_us = fun () -> Int64.(to_int (div (Kernel.Time.monotonic_time_nanos ()) 1_000L))

let default_stdout = fun buf ->
  if String.ends_with ~suffix:"\n" buf then
    println (String.sub buf 0 (String.length buf - 1))
  else
    print buf

let default_stderr = fun buf ->
  if String.ends_with ~suffix:"\n" buf then
    eprintln (String.sub buf 0 (String.length buf - 1))
  else
    eprint buf

let command = Riot_check.command

let fail = fun ?(stderr = default_stderr) err ->
  let message = Riot_check.Error.message err in
  if Riot_check.Error.should_print err then
    stderr ("\027[1;31mError\027[0m: " ^ message ^ "\n");
  Error (Failure message)

let stamp_json_event = fun ~command_started_at json ->
  let emitted_at_us = Int.max 0 (monotonic_now_us () - command_started_at) in
  match json with
  | Data.Json.Object fields ->
      if Option.is_some (List.assoc_opt "emitted_at_us" fields) then
        json
      else
        Data.Json.Object (fields @ [ ("emitted_at_us", Data.Json.Int emitted_at_us) ])
  | _ -> json

let emit_json = fun ~stdout ~workspace_root ~command_started_at event ->
  let json = Check.Event.to_json ~workspace_root event |> stamp_json_event ~command_started_at in
  stdout (Data.Json.to_string json ^ "\n")

let package_progress_line = fun label package_name ->
  let padding = String.make (Int.max 0 (12 - String.length label)) ' ' in
  padding ^ yellow_bold ^ label ^ reset ^ " " ^ package_name ^ "\n"

let emit_human = fun ~stdout ~stderr ~workspace_root ~quiet event ->
  match event with
  | Check.Event.Start _ ->
      ()
  | Check.Event.WorkspacePrepared _ ->
      ()
  | Check.Event.Package { package_name } ->
      if not quiet then
        stderr (package_progress_line "Checking" package_name)
  | Check.Event.PackageCached { package_name } ->
      ignore package_name
  | Check.Event.PackageEngineSelected _ ->
      ()
  | Check.Event.PackagePlanningStarted _
  | Check.Event.PackagePlanningFinished _
  | Check.Event.PackageSourcePreparationStarted _
  | Check.Event.PackageSourcePreparationFinished _
  | Check.Event.PackageSourcePreparationFailed _
  | Check.Event.PackageCheckedGroupEmitStarted _
  | Check.Event.PackageCheckedGroupEmitFinished _ ->
      ()
  | Check.Event.Typ { event=_ } ->
      ()
  | Check.Event.File checked_file ->
      let rendered = Check.Reporter.render_checked_file ~workspace_root checked_file in
      List.iter stdout rendered.stdout;
      List.iter stderr rendered.stderr
  | Check.Event.Diagnostic _ ->
      ()
  | Check.Event.Summary { summary } -> (
      match Check.Reporter.success_summary ~quiet summary with
      | Some line -> stdout line
      | None -> ()
    )
  | Check.Event.Explanation { explanation } ->
      stdout (Typ.Diagnostics.Explanations.format explanation ^ "\n")

let run = fun ~(workspace:Riot_model.Workspace.t) ?(stdout = default_stdout) ?(stderr = default_stderr) matches ->
  let json = ArgParser.get_flag matches "json" in
  let quiet = ArgParser.get_flag matches "quiet" in
  let command_started_at = monotonic_now_us () in
  let on_event =
    if json then
      emit_json ~stdout ~workspace_root:workspace.root ~command_started_at
    else
      emit_human ~stdout ~stderr ~workspace_root:workspace.root ~quiet
  in
  match Riot_check.run ~workspace ~on_event matches with
  | Ok () -> Ok ()
  | Error err -> fail ~stderr err
