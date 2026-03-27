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

let resolve_root = function
  | Some workspace -> workspace.Workspace.root
  | None ->
      Env.current_dir ()
      |> Result.expect ~msg:"Failed to get current directory"

let resolve_search_roots workspace =
  match workspace with
  | Some workspace -> workspace_roots workspace
  | None -> [ resolve_root None ]

let default_concurrency () =
  max 1 (min System.available_parallelism 50)

let write_text_result ~root (result : Krasny.Runner.run_result) =
  List.iter
    (fun file_result ->
      Krasny.Report.write_text_file_result ~writer:output_writer ~root file_result
      |> Result.expect ~msg:"failed to write fmt result")
    result.Krasny.Runner.files;
  Krasny.Report.write_text_summary ~writer:output_writer result.summary
  |> Result.expect ~msg:"failed to write fmt summary"

let write_json_result ~root ~concurrency (result : Krasny.Runner.run_result) =
  let emit event =
    Krasny.Report.write_json_event ~writer:output_writer ~root event
    |> Result.expect ~msg:"failed to write fmt JSON event"
  in
  emit (Krasny.Report.Start { total_files = result.summary.total_files; concurrency });
  List.iter (fun file_result -> emit (Krasny.Report.File file_result)) result.files;
  emit (Krasny.Report.Summary result.summary)

let unsupported_mode () =
  eprintln "tusk fmt currently only supports --check";
  Error (Failure "tusk fmt currently only supports --check")

let run ?workspace fmt_matches =
  if not (get_flag fmt_matches "check") then
    unsupported_mode ()
  else
    let root = resolve_root workspace in
    let files =
      Krasny.Runner.collect_ocaml_files ~roots:(resolve_search_roots workspace)
    in
    let concurrency = default_concurrency () in
    let result : Krasny.Runner.run_result =
      Krasny.Runner.run_checks ~concurrency files
    in
    if get_flag fmt_matches "json" then
      write_json_result ~root ~concurrency result
    else
      write_text_result ~root result;
    if
      result.summary.needs_formatting = 0
      && result.summary.failed_files = 0
    then
      Ok ()
    else
      Error (Failure "Formatting check failed")
