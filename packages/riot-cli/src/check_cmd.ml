open Std
open Std.Collections
open Riot_model

type action =
  | Explain of { diagnostic_id: string; json: bool }
  | Check of { paths: Path.t list; json: bool; quiet: bool }

type error =
  | ExplainAndPath of { path: Path.t }
  | InvalidPath of { path: Path.t; reason: string }
  | NoTargets
  | UnknownDiagnosticId of { diagnostic_id: string }

type diagnostic =
  | Parse of Syn.Diagnostic.t
  | Lowering of Typ.Diagnostic.t
  | Typing of Typ.Diagnostic.t

let command =
  let open ArgParser in
    let open Arg in command "check"
    |> about "Typecheck OCaml files in workspace members or the current directory"
    |> args
      [
        flag "json" |> long "json" |> help "Emit machine-readable JSON output";
        flag "quiet" |> long "quiet" |> help "Suppress success output when no diagnostics are found";
        option "explain" |> long "explain" |> help "Explain a typ diagnostic id such as TYP2001";
        positional "path"
        |> required false
        |> multiple
        |> help
          "OCaml file(s) or directory(ies) to typecheck (default: workspace packages or current directory)";
      ]

type checked_file =
  | Typed of {
      path: Path.t;
      report: Typ.Check_result.t;
      diagnostics: diagnostic list;
    }
  | Unreadable of {
      path: Path.t;
      reason: string;
    }

type checked_summary = {
  checked_files: int;
  read_failures: int;
  diagnostics: int;
  warnings: int;
  has_error: bool;
}

let empty_checked_summary = {
  checked_files = 0;
  read_failures = 0;
  diagnostics = 0;
  warnings = 0;
  has_error = false;
}

type package_scope = {
  package_root: Path.t;
  config: Fmt_config.t;
}

type check_scope = {
  workspace_root: Path.t;
  workspace_config: Fmt_config.t;
  packages: package_scope list;
}

let compare_paths = fun left right ->
  String.compare (Path.to_string left) (Path.to_string right)

let dedupe_paths = fun paths ->
  let seen = HashSet.create () in
  let rec loop acc remaining =
    match remaining with
    | [] -> List.rev acc
    | head :: tail ->
        let key = Path.to_string head in
        if HashSet.contains seen key then
          loop acc tail
        else (
          let _ = HashSet.insert seen key in
          loop (head :: acc) tail)
  in
  loop [] (List.sort compare_paths paths)

let workspace_roots = fun (workspace : Workspace.t) ->
  workspace.packages
  |> List.filter Package.is_workspace_member
  |> List.map (fun (pkg: Package.t) -> pkg.path)
  |> dedupe_paths

let is_supported_source_file = fun path ->
  match Path.extension path with
  | Some ".ml" | Some ".mli" -> true
  | _ -> false

let workspace_scope = fun (workspace : Workspace.t option) ->
  let scope_of_path path =
    let package_toml = Path.(path / Path.v "riot.toml") in
    {
      package_root = path;
      config = Fmt_config.load package_toml;
    }
  in
  match workspace with
  | Some workspace ->
      let workspace_toml = Path.(workspace.root / Path.v "riot.toml") in
      Some
        {
          workspace_root = workspace.root;
          workspace_config = Fmt_config.load workspace_toml;
          packages =
            workspace.packages |> List.map (fun (pkg: Package.t) -> scope_of_path pkg.path);
        }
  | None ->
      let cwd = Env.current_dir () |> Result.unwrap_or ~default:(Path.v ".") in
      let toml_path = Path.(cwd / Path.v "riot.toml") in
      if Fs.exists toml_path |> Result.unwrap_or ~default:false then
        Some
          {
            workspace_root = cwd;
            workspace_config = Fmt_config.load toml_path;
            packages = [];
          }
      else
        None

let resolve_root = fun (workspace : Workspace.t option) ->
  match workspace with
  | Some workspace -> workspace.root
  | None -> Env.current_dir () |> Result.unwrap_or ~default:(Path.v ".")

let resolve_search_roots = fun (workspace : Workspace.t option) ->
  match workspace with
  | Some workspace -> Ok (workspace_roots workspace)
  | None -> Ok [ resolve_root None ]

let matches_ignore_pattern = fun ~root pattern path ->
  let rel =
    match Path.strip_prefix path ~prefix:root with
    | Ok rel -> Path.to_string rel
    | Error _ -> Path.to_string path
  in
  String.contains rel pattern

let find_package_scope = fun scope file ->
  scope.packages
  |> List.filter_map
    (fun package_scope ->
      match Path.strip_prefix file ~prefix:package_scope.package_root with
      | Ok _ -> Some (String.length (Path.to_string package_scope.package_root), package_scope)
      | Error _ -> None)
  |> List.sort (fun ((left_len, _) ) ((right_len, _)) -> Int.compare right_len left_len)
  |> List.map snd
  |> function
  | package_scope :: _ -> Some package_scope
  | [] -> None

let should_ignore_file = fun scope file ->
  match scope with
  | None -> false
  | Some scope ->
      if
        List.exists
          (fun pattern -> matches_ignore_pattern ~root:scope.workspace_root pattern file)
          scope.workspace_config.ignore_patterns
      then
        true
      else
        match find_package_scope scope file with
        | Some package_scope ->
            List.exists
              (fun pattern -> matches_ignore_pattern ~root:package_scope.package_root pattern file)
              package_scope.config.ignore_patterns
        | None -> false

let validate_explicit_target = fun path ->
  if not (Path.exists path) then
    Error (InvalidPath { path; reason = "path does not exist" })
  else if Path.is_file path && not (is_supported_source_file path) then
    Error
      (InvalidPath
        { path; reason = "path is not an OCaml source file (.ml/.mli) or directory" })
  else
    Ok path

let validate_explicit_targets = fun roots ->
  let rec loop roots acc =
    match roots with
    | [] -> Ok acc
    | head :: tail -> (
        match validate_explicit_target head with
        | Error _ as err -> err
        | Ok root -> loop tail (root :: acc))
  in
  loop roots []

let resolve_targets = fun ?workspace paths ->
  let scope = workspace_scope workspace in
  let collect_ordered_files = fun roots ->
    roots
    |> List.concat_map
      (fun root ->
        Krasny.Runner.collect_ocaml_files
          ~should_ignore:(should_ignore_file scope)
          ~roots:[ root ]
          ()
        |> List.sort compare_paths)
  in
  let roots =
    if List.is_empty paths then
      resolve_search_roots workspace
    else
      validate_explicit_targets paths |> Result.map (List.sort_uniq compare_paths)
  in
  match roots with
  | Error err -> Error err
  | Ok validated_roots ->
      let target_files = collect_ordered_files validated_roots in
      if List.is_empty target_files then
        Error NoTargets
      else
        Ok target_files

let message = function
  | ExplainAndPath { path } ->
      "cannot use --explain together with a path (" ^ Path.to_string path ^ ")"
  | InvalidPath { path; reason } -> "invalid check path " ^ Path.to_string path ^ ": " ^ reason
  | NoTargets -> "no OCaml files found"
  | UnknownDiagnosticId { diagnostic_id } -> "unknown typ diagnostic id: " ^ diagnostic_id

let fail = fun err ->
  eprintln ("\027[1;31mError\027[0m: " ^ message err);
  Error (Failure (message err))

let action_of_matches = fun matches ->
  let json = ArgParser.get_flag matches "json" in
  let quiet = ArgParser.get_flag matches "quiet" in
  let paths = ArgParser.get_many matches "path" |> List.map Path.v in
  match ArgParser.get_one matches "explain", paths with
  | Some diagnostic_id, [] -> Ok (Explain { diagnostic_id; json })
  | Some _, path :: _ -> Error (ExplainAndPath { path })
  | None, paths -> Ok (Check { paths; json; quiet })

let diagnostics_of_report = fun (report: Typ.Check_result.t) ->
  (report.parse_diagnostics |> List.map (fun diagnostic -> Parse diagnostic))
  @ (report.lowering_diagnostics |> List.map (fun diagnostic -> Lowering diagnostic))
  @ (report.typing_diagnostics |> List.map (fun diagnostic -> Typing diagnostic))

let has_errors = fun diagnostics ->
  List.exists
    (function
      | Parse _ -> true
      | Lowering diagnostic
      | Typing diagnostic -> (
          match Typ.Diagnostic.severity diagnostic with
          | Typ.Diagnostic.Error -> true
          | Typ.Diagnostic.Warning -> false))
    diagnostics

let has_warning_diagnostic = function
  | Parse _ -> false
  | Lowering diagnostic
  | Typing diagnostic -> (Typ.Diagnostic.severity diagnostic = Typ.Diagnostic.Warning)

let has_warnings = fun diagnostics ->
  List.exists
    has_warning_diagnostic
    diagnostics

let update_checked_summary = fun summary checked_file ->
  match checked_file with
  | Unreadable _ ->
      {
        checked_files = summary.checked_files + 1;
        read_failures = summary.read_failures + 1;
        diagnostics = summary.diagnostics;
        warnings = summary.warnings;
        has_error = true;
      }
  | Typed { diagnostics; _ } ->
      let warning_count =
        diagnostics
        |> List.filter has_warning_diagnostic
        |> List.length
      in
      {
        checked_files = summary.checked_files + 1;
        read_failures = summary.read_failures;
        diagnostics = summary.diagnostics + List.length diagnostics;
        warnings = summary.warnings + warning_count;
        has_error = summary.has_error || has_errors diagnostics;
      }

let span_to_json = fun (span: Syn.Ceibo.Span.t) ->
  Data.Json.Object [ ("start", Data.Json.Int span.start); ("end", Data.Json.Int span.end_) ]

let severity_string_of_diagnostic = function
  | Parse _ -> "error"
  | Lowering diagnostic
  | Typing diagnostic -> Typ.Diagnostic.severity_to_string (Typ.Diagnostic.severity diagnostic)

let code_of_diagnostic = function
  | Parse diagnostic -> Syn.Diagnostic.id diagnostic
  | Lowering diagnostic
  | Typing diagnostic -> Typ.Diagnostic.code diagnostic

let message_of_diagnostic = function
  | Parse diagnostic ->
      let expected = Syn.Diagnostic.expected_message diagnostic in
      let main_message = Syn.Diagnostic.main_message diagnostic in
      if String.length expected > 0 then
        main_message ^ " (expected " ^ expected ^ ")"
      else
        main_message
  | Lowering diagnostic
  | Typing diagnostic -> Typ.Diagnostic.message diagnostic

let phase_of_diagnostic = function
  | Parse _ -> "parse"
  | Lowering _ -> "lowering"
  | Typing _ -> "typing"

let source_of_diagnostic = function
  | Parse _ -> "syn"
  | Lowering _
  | Typing _ -> "typ"

let data_of_diagnostic = function
  | Parse diagnostic -> Syn.Diagnostic.to_json diagnostic
  | Lowering diagnostic
  | Typing diagnostic -> Typ.Diagnostic.to_json diagnostic

let diagnostic_to_json = fun diagnostic ->
  Data.Json.Object [
    ("phase", Data.Json.String (phase_of_diagnostic diagnostic));
    ("source", Data.Json.String (source_of_diagnostic diagnostic));
    ("severity", Data.Json.String (severity_string_of_diagnostic diagnostic));
    ("code", Data.Json.String (code_of_diagnostic diagnostic));
    ("message", Data.Json.String (message_of_diagnostic diagnostic));
    ("span", span_to_json
      (match diagnostic with
       | Parse diagnostic -> diagnostic.Syn.Diagnostic.span
       | Lowering diagnostic
       | Typing diagnostic -> Typ.Diagnostic.primary_span diagnostic));
    ("data", data_of_diagnostic diagnostic);
  ]

let span_of_diagnostic = function
  | Parse diagnostic -> diagnostic.Syn.Diagnostic.span
  | Lowering diagnostic
  | Typing diagnostic -> Typ.Diagnostic.primary_span diagnostic

let position_of_offset = fun text offset -> Std.Unicode.Utf16.position_of_offset text ~offset

let make_source_layout = fun source ->
  let lines = String.split_on_char '\n' source |> Array.of_list in
  let line_starts = Array.make (Array.length lines) 0 in
  let offset = ref 0 in
  for index = 0 to Array.length lines - 1 do
    line_starts.(index) <- !offset;
    offset := !offset + String.length lines.(index) + 1
  done;
  (lines, line_starts)

let source_layout_line_for_pos = fun source_text (_, line_starts) pos ->
  let rec loop low high best =
    if low > high then
      best
    else
      let mid = (low + high) / 2 in
      if line_starts.(mid) <= pos then
        loop (mid + 1) high mid
      else
        loop low (mid - 1) best
  in
  if Array.length line_starts = 0 then
    (0, 0)
  else
    let last = Array.length line_starts - 1 in
    let line_idx = loop 0 last 0 |> fun line_idx -> Int.min last (Int.max 0 line_idx) in
    let _line_start = line_starts.(line_idx) in
    (line_idx, Int.max 0 (position_of_offset source_text pos).character)

let extract_snippet = fun source_layout source_text (span : Syn.Ceibo.Span.t) ->
  if Array.length (fst source_layout) = 0 then
    None
  else
    let start_position = position_of_offset source_text span.start in
    let end_position = position_of_offset source_text span.end_ in
    let line_idx = source_layout_line_for_pos source_text source_layout span.start |> fst in
    if line_idx < 0 || line_idx >= Array.length (fst source_layout) then
      None
    else
      let line_text = (fst source_layout).(line_idx) in
      let start_col = start_position.character in
      let pointer_span =
        if end_position.line = start_position.line then
          Int.max 1 (end_position.character - start_col)
        else
          1
      in
      Some (line_idx + 1, start_col, line_text, pointer_span)

let format_diagnostic = fun ~path_text ~source_layout ~source_text diagnostic ->
  let span : Syn.Ceibo.Span.t = span_of_diagnostic diagnostic in
  let start_position = position_of_offset source_text span.start in
  let line = start_position.line + 1 in
  let column = start_position.character + 1 in
  let header =
    path_text
    ^ ":"
    ^ Int.to_string line
    ^ ":"
    ^ Int.to_string column
    ^ ": "
    ^ severity_string_of_diagnostic diagnostic
    ^ " ["
    ^ phase_of_diagnostic diagnostic
    ^ "] "
    ^ code_of_diagnostic diagnostic
    ^ ": "
    ^ message_of_diagnostic diagnostic
  in
  match extract_snippet source_layout source_text span with
  | None -> header
  | Some (line_num, start_col, code_line, pointer_span) ->
      let line_label = Int.to_string line_num in
      let gutter = String.length line_label in
      let pointer = String.make (Int.max 0 start_col) ' ' ^ String.make pointer_span '^' in
      header
      ^ "\n"
      ^ (String.make gutter ' ')
      ^ " |\n"
      ^ line_label
      ^ " | "
      ^ code_line
      ^ "\n"
      ^ (String.make gutter ' ')
      ^ " | "
      ^ pointer
      ^ "\n"

let read_report_to_json = fun ~path reason ->
  Data.Json.Object [
    ("path", Data.Json.String (Path.to_string path));
    ("ok", Data.Json.Bool false);
    ("error", Data.Json.String reason);
    ("diagnostics", Data.Json.Array []);
    (
      "summary",
      Data.Json.Object [
        ("parse", Data.Json.Int 0);
        ("lowering", Data.Json.Int 0);
        ("typing", Data.Json.Int 0);
        ("total", Data.Json.Int 0);
      ]
    );
  ]

let check_source_file = fun path ->
  match Fs.read path with
  | Error err -> Unreadable { path; reason = IO.error_message err }
  | Ok source ->
      let report = Typ.Batch.check_source ~filename:path source in
      let diagnostics = diagnostics_of_report report in
      Typed { path; report; diagnostics }

let check_all = fun ?workspace ?on_result paths ->
  match resolve_targets ?workspace paths with
  | Error err -> Error err
  | Ok target_files ->
      (
        match target_files with
        | [] -> Error NoTargets
        | _ ->
            target_files
            |> List.map
              (fun path ->
                let checked_file = check_source_file path in
                (match on_result with
                | Some callback -> callback checked_file
                | None -> ());
                checked_file)
            |> Result.ok
      )

let print_checked_file = fun checked_file ->
  match checked_file with
  | Unreadable { path; reason } ->
      eprintln (Path.to_string path ^ ": " ^ reason)
  | Typed { path; report; diagnostics } ->
      if List.is_empty diagnostics then
        ()
      else (
        let source_layout = make_source_layout report.source in
        let path_text = Path.to_string path in
        List.iter
          (fun diagnostic ->
            println
              (format_diagnostic
                ~path_text
                ~source_layout
                ~source_text:report.source
                diagnostic))
          diagnostics)

let checked_file_to_json = fun checked_file ->
  match checked_file with
  | Typed { path; report } ->
      let diagnostics = diagnostics_of_report report in
      let summary =
        Data.Json.Object [
          ("parse", Data.Json.Int (List.length report.parse_diagnostics));
          ("lowering", Data.Json.Int (List.length report.lowering_diagnostics));
          ("typing", Data.Json.Int (List.length report.typing_diagnostics));
          ("total", Data.Json.Int (List.length diagnostics));
        ]
      in
      Data.Json.Object [
        ("path", Data.Json.String (Path.to_string path));
        ("ok", Data.Json.Bool (not (has_errors diagnostics)));
        ("summary", summary);
      ]
  | Unreadable { path; reason } -> read_report_to_json ~path reason

let checked_file_diagnostics_to_json = fun path diagnostics ->
  let path_text = Path.to_string path in
  let index = ref 0 in
  List.iter
    (fun diagnostic ->
      let json =
        Data.Json.Object [
          ("type", Data.Json.String "check_diagnostic");
          ("path", Data.Json.String path_text);
          ("diagnostic_index", Data.Json.Int !index);
          ("diagnostic", diagnostic_to_json diagnostic);
        ]
      in
      Data.Json.to_string json |> println;
      index := !index + 1)
    diagnostics

let print_checked_file_json = fun checked_file ->
  match checked_file with
  | Unreadable _ ->
      Data.Json.Object [
        ("type", Data.Json.String "check_file");
        ("result", checked_file_to_json checked_file);
      ]
      |> Data.Json.to_string
      |> println
  | Typed { path; report; diagnostics } ->
      let file_json =
        Data.Json.Object [
          ("type", Data.Json.String "check_file");
          ("result", checked_file_to_json (Typed { path; report; diagnostics }));
        ]
      in
      Data.Json.to_string file_json |> println;
      checked_file_diagnostics_to_json path diagnostics

let print_checked_files_summary = fun checked_summary quiet ->
  if quiet then
    ()
  else if checked_summary.has_error then
    ()
  else if checked_summary.checked_files = 1 then
    println "Checked 1 file: ok"
  else
    println
      ("Checked " ^ Int.to_string checked_summary.checked_files ^ " files: ok")

let checked_files_ok = fun checked_files ->
  checked_files
  |> List.for_all
    (function
      | Unreadable _ -> false
      | Typed { diagnostics; _ } -> not (has_errors diagnostics))

let checked_files_summary = fun checked_files ->
  let files = List.length checked_files in
  let read_failures =
    checked_files
    |> List.filter (function | Unreadable _ -> true | Typed _ -> false)
    |> List.length
  in
  let diagnostics_count =
    checked_files
    |> List.filter_map
      (function
        | Unreadable _ -> Some 0
        | Typed { diagnostics; _ } -> Some (List.length diagnostics))
    |> List.fold_left (fun acc n -> acc + n) 0
  in
  let warning_count =
    checked_files
    |> List.fold_left
      (fun count checked_file ->
        match checked_file with
        | Unreadable _ -> count
        | Typed { diagnostics; _ } ->
            count + List.length (List.filter has_warning_diagnostic diagnostics))
      0
  in
  Data.Json.Object [
    ("files", Data.Json.Int files);
    ("read_failures", Data.Json.Int read_failures);
    ("diagnostics", Data.Json.Int diagnostics_count);
    ("warnings", Data.Json.Int warning_count);
  ]

let print_json = fun checked_files ->
  checked_files |> List.iter print_checked_file_json;
  let files = List.length checked_files in
  let read_failures =
    checked_files
    |> List.filter (function | Unreadable _ -> true | Typed _ -> false)
    |> List.length
  in
  let diagnostics =
    checked_files
    |> List.filter_map
      (function
        | Unreadable _ -> Some 0
        | Typed { diagnostics; _ } -> Some (List.length diagnostics))
    |> List.fold_left (fun acc n -> acc + n) 0
  in
  let warnings =
    checked_files
    |> List.fold_left
      (fun count checked_file ->
        match checked_file with
        | Unreadable _ -> count
        | Typed { diagnostics; _ } ->
            count + List.length (List.filter has_warning_diagnostic diagnostics))
      0
  in
  Data.Json.Object [
    ("type", Data.Json.String "check_summary");
    ("ok", Data.Json.Bool (checked_files_ok checked_files));
    ("summary", Data.Json.Object [
      ("files", Data.Json.Int files);
      ("read_failures", Data.Json.Int read_failures);
      ("diagnostics", Data.Json.Int diagnostics);
      ("warnings", Data.Json.Int warnings);
    ]);
  ] |> Data.Json.to_string |> println

let run_explain = fun ~json diagnostic_id ->
  match Typ.Explanations.explain diagnostic_id with
  | None -> fail (UnknownDiagnosticId { diagnostic_id })
  | Some explanation ->
      if json then
        Typ.Explanations.to_json explanation |> Data.Json.to_string |> println
      else
        Typ.Explanations.format explanation |> println;
      Ok ()

let run = fun ?workspace matches ->
  match action_of_matches matches with
  | Error err -> fail err
  | Ok (Explain { diagnostic_id; json }) -> run_explain ~json diagnostic_id
  | Ok (Check { paths; json; quiet }) -> (
      let summary = ref empty_checked_summary in
      let on_result =
        fun checked_file ->
          summary := update_checked_summary !summary checked_file;
          if json then
            print_checked_file_json checked_file
          else
            print_checked_file checked_file
      in
      match check_all ?workspace ~on_result paths with
      | Error err -> fail (match err with NoTargets -> NoTargets | _ -> err)
      | Ok checked_files ->
          if json then
            print_json checked_files
          else
            print_checked_files_summary !summary quiet;
          if !summary.has_error then
            Error (Failure "typecheck failed")
          else
            Ok ()
      )
