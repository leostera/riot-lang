open Std
open Std.ArgParser
open Riot_model

let command =
  let open Arg in command "fmt"
  |> about "Format OCaml with krasny"
  |> args
    [
      flag "check" |> long "check" |> help "Check if files need formatting";
      flag "verify" |> long "verify" |> help "Verify formatting would preserve syntax hashes";
      flag "json" |> long "json" |> help "Emit machine-readable JSONL events";
      option "explain" |> long "explain" |> help "Explain a syn parse error code (e.g. E0001)";
      positional "path" |> required false |> multiple |> help "OCaml file or directory to format/check/verify (default: workspace packages or current directory)";
    ]

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

let writer_of_emit = fun emit ->
  let module Write = struct
    type t = string -> unit

    type err = unit

    let write = fun emit ~buf ->
      emit buf;
      Ok (String.length buf)

    let write_owned_vectored = fun _ ~bufs:_ -> unimplemented ()

    let flush = fun _ -> Ok ()
  end in
  IO.Writer.of_write_src (module Write) emit

let workspace_roots = fun workspace ->
  workspace.Workspace.packages |> List.map (fun (pkg: Package.t) -> Path.(workspace.root / pkg.path))

type package_scope = {
  package_root: Path.t;
  config: Fmt_config.t;
}

type fmt_scope = {
  workspace_root: Path.t;
  workspace_config: Fmt_config.t;
  packages: package_scope list;
}

let resolve_root = function
  | Some workspace -> workspace.Workspace.root
  | None -> Env.current_dir () |> Result.expect ~msg:"Failed to get current directory"

let resolve_search_roots = fun workspace ->
  match workspace with
  | Some workspace -> workspace_roots workspace
  | None -> [ resolve_root None ]

let load_fmt_scope = function
  | Some workspace ->
      let workspace_toml = Path.(workspace.Workspace.root / Path.v "riot.toml") in
      let packages =
        workspace.Workspace.packages
        |> List.map
          (fun (pkg: Package.t) ->
            let package_toml = Path.(pkg.path / Path.v "riot.toml") in
            { package_root = pkg.path; config = Fmt_config.load package_toml })
      in
      Some {
        workspace_root = workspace.Workspace.root;
        workspace_config = Fmt_config.load workspace_toml;
        packages
      }
  | None ->
      let cwd = resolve_root None in
      let toml_path = Path.(cwd / Path.v "riot.toml") in
      if Fs.exists toml_path |> Result.unwrap_or ~default:false then
        Some { workspace_root = cwd; workspace_config = Fmt_config.load toml_path; packages = [] }
      else
        None

let default_concurrency = fun () -> max 1 (min System.available_parallelism 50)

let relative_or_absolute = fun ~root path ->
  match Path.strip_prefix path ~prefix:root with
  | Ok rel -> Path.to_string rel
  | Error _ -> Path.to_string path

let compare_paths = fun left right ->
  String.compare (Path.to_string left) (Path.to_string right)

let matches_ignore_pattern = fun ~root pattern path ->
  String.contains (relative_or_absolute ~root path) pattern

let find_package_scope = fun scope file ->
  scope.packages |> List.filter_map
    (fun package_scope ->
      match Path.strip_prefix file ~prefix:package_scope.package_root with
      | Ok _ -> Some (String.length (Path.to_string package_scope.package_root), package_scope)
      | Error _ -> None) |> List.sort
    (fun ((left_len, _)) ((right_len, _)) ->
      Int.compare right_len left_len) |> List.map snd |> function
  | package_scope :: _ -> Some package_scope
  | [] -> None

let should_ignore_file = fun scope file ->
  match scope with
  | None -> false
  | Some scope ->
      let matches patterns ~root =
        List.exists (fun pattern -> matches_ignore_pattern ~root pattern file) patterns
      in
      if matches scope.workspace_config.ignore_patterns ~root:scope.workspace_root then
        true
      else
        match find_package_scope scope file with
        | Some package_scope -> matches package_scope.config.ignore_patterns ~root:package_scope.package_root
        | None -> false

let write_text_file = fun ~writer ~root file_result ->
  Krasny.Report.write_text_file_result ~writer ~root file_result |> Result.expect ~msg:"failed to write fmt result"

let write_json_event = fun ~writer ~root event ->
  Krasny.Report.write_json_event ~writer ~root event |> Result.expect ~msg:"failed to write fmt JSON event"

let write_json_start = fun ~writer ~root ~mode ~concurrency ->
  write_json_event ~writer ~root (Krasny.Report.Start { mode; concurrency })

let write_json_file = fun ~writer ~root file_result ->
  write_json_event ~writer ~root (Krasny.Report.File file_result)

let write_json_summary = fun ~writer ~root (summary: Krasny.Runner.summary) ->
  write_json_event ~writer ~root (Krasny.Report.Summary summary)

let write_text_summary = fun ~writer ~mode (summary: Krasny.Runner.summary) ->
  Krasny.Report.write_text_summary ~writer ~mode summary |> Result.expect ~msg:"failed to write fmt summary"

let format_failed_file = fun (file_result: Krasny.Runner.file_result) ->
  let error_text = file_result.error |> Option.unwrap_or ~default:"Formatting failed" in
  match Fs.read file_result.file with
  | Error _ -> Path.to_string file_result.file ^ ": " ^ error_text ^ "\n"
  | Ok source ->
      let parsed = Syn.parse ~filename:file_result.file source in
      if List.is_empty parsed.diagnostics then
        Path.to_string file_result.file ^ ": " ^ error_text ^ "\n"
      else
        Syn.DiagnosticReporter.format ~file:(Path.to_string file_result.file) ~source parsed.diagnostics

let write_failed_file = fun ~writer file_result ->
  IO.write_all writer ~buf:(format_failed_file file_result) |> Result.expect ~msg:"failed to write fmt diagnostics"

let write_silent_failures = fun ~writer (result: Krasny.Runner.run_result) ->
  result.files
  |> List.filter (fun (file_result: Krasny.Runner.file_result) -> file_result.status = Failed)
  |> List.iter (write_failed_file ~writer)

type output_mode =
  | Silent
  | Text
  | Json
  | QuietCheck

let explicit_targets = fun matches ->
  ArgParser.get_many matches "path" |> List.map Path.v |> List.sort_uniq compare_paths

let no_event = fun (_: Krasny.Report.event) -> ()

let run_mode = fun ?workspace ?(stdout = default_stdout) ?(stderr = default_stderr) ?(on_event = no_event) ~mode ~output_mode ~explicit_targets () ->
  let stdout_writer = writer_of_emit stdout in
  let stderr_writer = writer_of_emit stderr in
  let root = resolve_root workspace in
  let concurrency = default_concurrency () in
  let fmt_scope = load_fmt_scope workspace in
  on_event (Krasny.Report.Start { mode; concurrency });
  (
    match output_mode with
    | Json -> write_json_start ~writer:stdout_writer ~root ~mode ~concurrency
    | QuietCheck -> ()
    | Text
    | Silent -> ()
  );
  let on_result file_result =
    on_event (Krasny.Report.File file_result);
    match output_mode with
    | Json -> write_json_file ~writer:stdout_writer ~root file_result
    | Text -> (
        match file_result.status with
        | Krasny.Runner.Failed -> write_failed_file ~writer:stdout_writer file_result
        | _ -> write_text_file ~writer:stdout_writer ~root file_result
      )
    | QuietCheck ->
        (
          match mode, file_result.status with
          | Krasny.Runner.Check, Krasny.Runner.Already_formatted -> ()
          | Krasny.Runner.Check, Krasny.Runner.Failed -> write_failed_file ~writer:stdout_writer file_result
          | Krasny.Runner.Check, _ -> write_text_file ~writer:stdout_writer ~root file_result
          | _ -> ()
        )
    | Silent -> ()
  in
  let result: Krasny.Runner.run_result =
    if List.is_empty explicit_targets then
      match mode with
      | Krasny.Runner.Check -> Krasny.Runner.run_checks_streaming
        ~concurrency
        ~should_ignore:(should_ignore_file fmt_scope)
        ~roots:(resolve_search_roots workspace)
        ~on_result
        ()
      | Krasny.Runner.Verify -> Krasny.Runner.run_verify_streaming
        ~concurrency
        ~should_ignore:(should_ignore_file fmt_scope)
        ~roots:(resolve_search_roots workspace)
        ~on_result
        ()
      | Krasny.Runner.Format -> Krasny.Runner.run_format_streaming
        ~concurrency
        ~should_ignore:(should_ignore_file fmt_scope)
        ~roots:(resolve_search_roots workspace)
        ~on_result
        ()
    else
      match mode with
      | Krasny.Runner.Check -> Krasny.Runner.run_checks_streaming
        ~concurrency
        ~should_ignore:(should_ignore_file fmt_scope)
        ~roots:explicit_targets
        ~on_result
        ()
      | Krasny.Runner.Verify -> Krasny.Runner.run_verify_streaming
        ~concurrency
        ~should_ignore:(should_ignore_file fmt_scope)
        ~roots:explicit_targets
        ~on_result
        ()
      | Krasny.Runner.Format -> Krasny.Runner.run_format_streaming
        ~concurrency
        ~should_ignore:(should_ignore_file fmt_scope)
        ~roots:explicit_targets
        ~on_result
        ()
  in
  on_event (Krasny.Report.Summary result.summary);
  (
    match output_mode with
    | Json -> write_json_summary ~writer:stdout_writer ~root result.summary
    | Text -> write_text_summary ~writer:stdout_writer ~mode result.summary
    | QuietCheck -> ()
    | Silent -> ()
  );
  if mode = Krasny.Runner.Format && output_mode = Silent && result.summary.failed_files > 0 then
    write_silent_failures ~writer:stderr_writer result;
  match mode with
  | Krasny.Runner.Check ->
      if result.summary.needs_formatting = 0 && result.summary.failed_files = 0 then
        Ok ()
      else
        Error (Failure "Formatting check failed")
  | Krasny.Runner.Verify ->
      if result.summary.unsafe_to_format = 0 && result.summary.failed_files = 0 then
        Ok ()
      else
        Error (Failure "Formatting verification failed")
  | Krasny.Runner.Format ->
      if result.summary.failed_files = 0 then
        Ok ()
      else
        Error (Failure "Formatting failed")

let run_check_paths = fun ?workspace ?(on_event = no_event) paths ->
  run_mode
    ?workspace
    ~on_event
    ~mode:Krasny.Runner.Check
    ~output_mode:Silent
    ~explicit_targets:(List.sort_uniq compare_paths paths)
    ()

let run_explain = fun ?(stdout = default_stdout) ?(stderr = default_stderr) error_code ->
  match Syn.Error.id_of_string error_code with
  | Some id ->
      stdout (Syn.Error.explain id ^ "\n");
      Ok ()
  | None ->
      stderr ("Unknown error code: " ^ error_code ^ "\n");
      Error (Failure ("Unknown error code: " ^ error_code))

let run = fun ?workspace ?stdout ?stderr fmt_matches ->
  let check = get_flag fmt_matches "check" in
  let verify = get_flag fmt_matches "verify" in
  let explain = get_one fmt_matches "explain" in
  let has_paths = not (List.is_empty (explicit_targets fmt_matches)) in
  match check, verify, explain with
  | true, true, _ ->
      eprintln "riot fmt cannot use both --check and --verify";
      Error (Failure "riot fmt cannot use both --check and --verify")
  | _, _, Some _ when check || verify || get_flag fmt_matches "json" || has_paths ->
      eprintln "riot fmt --explain cannot be combined with formatting flags or paths";
      Error (Failure "riot fmt --explain cannot be combined with formatting flags or paths")
  | _, _, Some error_code ->
      run_explain ?stdout ?stderr error_code
  | _ ->
      let mode =
        if check then
          Krasny.Runner.Check
        else if verify then
          Krasny.Runner.Verify
        else
          Krasny.Runner.Format
      in
      let output_mode =
        if get_flag fmt_matches "json" then
          Json
        else if mode = Krasny.Runner.Format then
          Silent
        else if mode = Krasny.Runner.Check then
          QuietCheck
        else
          Text
      in
      run_mode
        ?workspace
        ?stdout
        ?stderr
        ~mode
        ~output_mode
        ~explicit_targets:(explicit_targets fmt_matches)
        ()
