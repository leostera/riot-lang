open Std
module Check = Riot_check.Check

let yellow_bold = "\027[1;33m"

let reset = "\027[0m"

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

let emit_json = fun ~stdout ~workspace_root event ->
  stdout (Data.Json.to_string (Check.Event.to_json ~workspace_root event) ^ "\n")

let package_progress_line = fun label package_name ->
  let padding = String.make (Int.max 0 (12 - String.length label)) ' ' in
  padding ^ yellow_bold ^ label ^ reset ^ " " ^ package_name ^ "\n"

let emit_human = fun ~stdout ~stderr ~workspace_root ~quiet event ->
  match event with
  | Check.Event.Start _ ->
      ()
  | Check.Event.Package { package_name } ->
      if not quiet then
        stderr (package_progress_line "Checking" package_name)
  | Check.Event.PackageCached { package_name } ->
      ignore package_name
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
  let on_event =
    if json then
      emit_json ~stdout ~workspace_root:workspace.root
    else
      emit_human ~stdout ~stderr ~workspace_root:workspace.root ~quiet
  in
  match Riot_check.run ~workspace ~on_event matches with
  | Ok () -> Ok ()
  | Error err -> fail ~stderr err
