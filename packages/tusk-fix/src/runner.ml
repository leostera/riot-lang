open Std

type mode = Check | Apply

type file_result = {
  file : Path.t;
  final_source : string;
  diagnostics : Diagnostic.t list;
  parse_diagnostics : Syn.Diagnostic.t list;
  applied_fixes : Fix.fix list;
  changed : bool;
  error : string option;
}

type summary = {
  total_files : int;
  changed_files : int;
  remaining_diagnostics : int;
  applied_fixes : int;
  failed_files : int;
}

type run_result = { files : file_result list; summary : summary }

let empty_result file error =
  {
    file;
    final_source = "";
    diagnostics = [];
    parse_diagnostics = [];
    applied_fixes = [];
    changed = false;
    error;
  }

let has_errors diagnostics =
  List.exists (fun diag -> Diagnostic.severity diag = Diagnostic.Error) diagnostics

let run_pipeline pipeline file source =
  Pipeline.run pipeline ~filename:(Path.to_string file) source

let resolve_pipeline ?pipeline ?pipeline_for_file file =
  match pipeline_for_file with
  | Some resolve -> resolve file
  | None -> Option.unwrap_or ~default:(Pipeline.default ()) pipeline

let run_file ?pipeline ?pipeline_for_file ~mode file =
  let pipeline = resolve_pipeline ?pipeline ?pipeline_for_file file in
  match Fs.read file with
  | Error _ ->
      empty_result file
        (Some ("Failed to read " ^ Path.to_string file))
  | Ok source -> (
      let initial = run_pipeline pipeline file source in
      let safe_fixes = List.filter_map Diagnostic.fix initial.diagnostics in
      match mode with
      | Check ->
          {
            file;
            final_source = source;
            diagnostics = initial.diagnostics;
            parse_diagnostics = initial.parse_diagnostics;
            applied_fixes = [];
            changed = false;
            error = None;
          }
      | Apply ->
          if
            List.length initial.parse_diagnostics > 0
            || has_errors initial.diagnostics
            || List.length safe_fixes = 0
          then
            {
              file;
              final_source = source;
              diagnostics = initial.diagnostics;
              parse_diagnostics = initial.parse_diagnostics;
              applied_fixes = [];
              changed = false;
              error = None;
            }
          else
            match Fix.apply_fixes ~source safe_fixes with
            | Error reason ->
                empty_result file
                  (Some
                     ("Failed to apply fixes for " ^ Path.to_string file ^ ": "
                    ^ reason))
            | Ok updated_source ->
                if String.equal updated_source source then
                  {
                    file;
                    final_source = source;
                    diagnostics = initial.diagnostics;
                    parse_diagnostics = initial.parse_diagnostics;
                    applied_fixes = [];
                    changed = false;
                    error = None;
                  }
                else
                  match Fs.write updated_source file with
                  | Error _ ->
                      empty_result file
                        (Some ("Failed to write " ^ Path.to_string file))
                  | Ok () ->
                      let final = run_pipeline pipeline file updated_source in
                      {
                        file;
                        final_source = updated_source;
                        diagnostics = final.diagnostics;
                        parse_diagnostics = final.parse_diagnostics;
                        applied_fixes = safe_fixes;
                        changed = true;
                        error = None;
                      })

let summarize files =
  List.fold_left
    (fun acc result ->
      {
        total_files = acc.total_files + 1;
        changed_files = acc.changed_files + if result.changed then 1 else 0;
        remaining_diagnostics =
          acc.remaining_diagnostics
          + List.length result.diagnostics
          + List.length result.parse_diagnostics;
        applied_fixes = acc.applied_fixes + List.length result.applied_fixes;
        failed_files = acc.failed_files + if Option.is_some result.error then 1 else 0;
      })
    {
      total_files = 0;
      changed_files = 0;
      remaining_diagnostics = 0;
      applied_fixes = 0;
      failed_files = 0;
    }
    files

let run_files ?pipeline ?pipeline_for_file ~mode files =
  let files = List.sort (fun a b -> String.compare (Path.to_string a) (Path.to_string b)) files in
  let results =
    List.map (fun file -> run_file ?pipeline ?pipeline_for_file ~mode file) files
  in
  { files = results; summary = summarize results }

let summary_to_json summary =
  let open Data.Json in
  Object
    [
      ("total_files", Int summary.total_files);
      ("changed_files", Int summary.changed_files);
      ("remaining_diagnostics", Int summary.remaining_diagnostics);
      ("applied_fixes", Int summary.applied_fixes);
      ("failed_files", Int summary.failed_files);
    ]

let file_result_to_json result =
  let open Data.Json in
  Object
    [
      ("file", String (Path.to_string result.file));
      ("changed", Bool result.changed);
      ("error", match result.error with Some err -> String err | None -> Null);
      ("applied_fixes", Array (List.map Fix.to_json result.applied_fixes));
      ( "parse_diagnostics",
        Array (List.map Syn.Diagnostic.to_json result.parse_diagnostics) );
      ("diagnostics", Array (List.map Diagnostic.to_json result.diagnostics));
    ]

let run_result_to_json result =
  let open Data.Json in
  Object
    [
      ("summary", summary_to_json result.summary);
      ("files", Array (List.map file_result_to_json result.files));
    ]
