open Std
open Std.ArgParser
open Tusk_model

let command =
  let open Arg in
  command "fmt" |> about "Check OCaml formatting with krasny"
  |> args
       [
         flag "check" |> long "check"
         |> help "Check if files need formatting";
         flag "json" |> long "json"
         |> help "Emit machine-readable JSONL events";
       ]

let output_writer =
  let module Write = struct
    type t = unit
    type err = unit

    let write () ~buf =
      print buf;
      Ok (String.length buf)

    let write_owned_vectored () ~bufs:_ = unimplemented ()
    let flush () = Ok ()
  end in
  IO.Writer.of_write_src (module Write) ()

let workspace_roots workspace =
  workspace.Workspace.packages
  |> List.map (fun (pkg : Package.t) -> Path.(workspace.root / pkg.path))

type package_scope = {
  package_root : Path.t;
  config : Fmt_config.t;
}

type fmt_scope = {
  workspace_root : Path.t;
  workspace_config : Fmt_config.t;
  packages : package_scope list;
}

let resolve_root = function
  | Some workspace -> workspace.Workspace.root
  | None ->
      Env.current_dir ()
      |> Result.expect ~msg:"Failed to get current directory"

let resolve_search_roots workspace =
  match workspace with
  | Some workspace -> workspace_roots workspace
  | None -> [ resolve_root None ]

let load_fmt_scope = function
  | Some workspace ->
      let workspace_toml = Path.(workspace.Workspace.root / Path.v "tusk.toml") in
      let packages =
        workspace.Workspace.packages
        |> List.map (fun (pkg : Package.t) ->
               let package_toml = Path.(pkg.path / Path.v "tusk.toml") in
               {
                 package_root = pkg.path;
                 config = Fmt_config.load package_toml;
               })
      in
      Some
        {
          workspace_root = workspace.Workspace.root;
          workspace_config = Fmt_config.load workspace_toml;
          packages;
        }
  | None ->
      let cwd = resolve_root None in
      let toml_path = Path.(cwd / Path.v "tusk.toml") in
      if Fs.exists toml_path |> Result.unwrap_or ~default:false then
        Some
          {
            workspace_root = cwd;
            workspace_config = Fmt_config.load toml_path;
            packages = [];
          }
      else
        None

let default_concurrency () =
  max 1 (min System.available_parallelism 50)

let relative_or_absolute ~root path =
  match Path.strip_prefix path ~prefix:root with
  | Ok rel -> Path.to_string rel
  | Error _ -> Path.to_string path

let matches_ignore_pattern ~root pattern path =
  String.contains (relative_or_absolute ~root path) pattern

let find_package_scope scope file =
  scope.packages
  |> List.filter_map (fun package_scope ->
         match Path.strip_prefix file ~prefix:package_scope.package_root with
         | Ok _ ->
             Some
               ( String.length (Path.to_string package_scope.package_root),
                 package_scope )
         | Error _ -> None)
  |> List.sort (fun (left_len, _) (right_len, _) -> Int.compare right_len left_len)
  |> List.map snd
  |> function
  | package_scope :: _ -> Some package_scope
  | [] -> None

let should_ignore_file scope file =
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
        | Some package_scope ->
            matches package_scope.config.ignore_patterns
              ~root:package_scope.package_root
        | None -> false

let write_text_file ~root file_result =
  Krasny.Report.write_text_file_result ~writer:output_writer ~root file_result
  |> Result.expect ~msg:"failed to write fmt result"

let write_json_event ~root event =
  Krasny.Report.write_json_event ~writer:output_writer ~root event
  |> Result.expect ~msg:"failed to write fmt JSON event"

let write_json_start ~root ~concurrency =
  write_json_event ~root (Krasny.Report.Start { concurrency })

let write_json_file ~root file_result =
  write_json_event ~root (Krasny.Report.File file_result)

let write_json_summary ~root (summary : Krasny.Runner.summary) =
  write_json_event ~root (Krasny.Report.Summary summary)

let write_text_summary (summary : Krasny.Runner.summary) =
  Krasny.Report.write_text_summary ~writer:output_writer summary
  |> Result.expect ~msg:"failed to write fmt summary"

let stream_result_writer ~json ~root ~concurrency =
  if json then write_json_start ~root ~concurrency;
  fun file_result ->
    if json then
      write_json_file ~root file_result
    else
      write_text_file ~root file_result

let unsupported_mode () =
  eprintln "tusk fmt currently only supports --check";
  Error (Failure "tusk fmt currently only supports --check")

let run ?workspace fmt_matches =
  if not (get_flag fmt_matches "check") then
    unsupported_mode ()
  else
    let root = resolve_root workspace in
    let json = get_flag fmt_matches "json" in
    let concurrency = default_concurrency () in
    let fmt_scope = load_fmt_scope workspace in
    let result : Krasny.Runner.run_result =
      Krasny.Runner.run_checks_streaming ~concurrency
        ~should_ignore:(should_ignore_file fmt_scope)
        ~roots:(resolve_search_roots workspace)
        ~on_result:(stream_result_writer ~json ~root ~concurrency) ()
    in
    if json then
      write_json_summary ~root result.summary
    else
      write_text_summary result.summary;
    if
      result.summary.needs_formatting = 0
      && result.summary.failed_files = 0
    then
      Ok ()
    else
      Error (Failure "Formatting check failed")
