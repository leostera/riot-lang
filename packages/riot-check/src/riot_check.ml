open Std
open Std.Collections
open Tty
open Riot_model

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

type action =
  | Explain of { diagnostic_id: string; json: bool }
  | Check of { paths: Path.t list; package_filter: string option; json: bool; quiet: bool }

type error =
  | ExplainAndPath of { path: Path.t }
  | InvalidPath of { path: Path.t; reason: string }
  | NoTargets
  | PackageFilterRequiresWorkspace of { package_name: string }
  | UnknownPackage of { package_name: string }
  | UnknownDiagnosticId of { diagnostic_id: string }
  | RegistryInitializationFailed of { registry: string; error: string }
  | WorkspacePreparationFailed of { path: Path.t; error: string }

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
        flag "quiet" |> long "quiet" |> help "Suppress the final success summary when the check succeeds";
        option "package" |> short 'p' |> long "package" |> help "Typecheck sources from a specific workspace package";
        option "explain" |> long "explain" |> help "Explain a typ diagnostic id such as TYP2001";
        positional "path" |> required false |> multiple |> help "OCaml file(s) or directory(ies) to typecheck (default: workspace packages or current directory)";
      ]

type checked_file =
  | Typed of { path: Path.t; report: Typ.Check_result.t; diagnostics: diagnostic list }
  | Unreadable of { path: Path.t; reason: string }

type prepared_source =
  | Readable_source of {
      path: Path.t;
      source: string;
      source_id: Typ.SourceId.t;
      typ_source: Typ.Source.t
    }
  | Unreadable_source of { path: Path.t; reason: string }

type readable_typ_source = {
  path: Path.t;
  text: string;
  source_id: Typ.SourceId.t;
  source: Typ.Source.t;
}

type package_typ_source = {
  internal_module_name: string;
  public_module_name: string option;
  display_path: Path.t;
  source_id: Typ.SourceId.t;
  source: Typ.Source.t;
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
        else
          (
            let _ = HashSet.insert seen key in
            loop (head :: acc) tail
          )
  in
  loop [] (List.sort compare_paths paths)

let workspace_roots = fun (workspace: Workspace.t) ->
  workspace.packages
  |> List.filter Package.is_workspace_member
  |> List.map (fun (pkg: Package.t) -> pkg.path)
  |> dedupe_paths

let workspace_roots_for_package = fun (workspace: Workspace.t) package_name ->
  workspace.packages
  |> List.filter
    (fun (pkg: Package.t) -> Package.is_workspace_member pkg && String.equal pkg.name package_name)
  |> List.map (fun (pkg: Package.t) -> pkg.path)
  |> dedupe_paths

let is_supported_source_file = fun path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" -> true
  | _ -> false

let relative_or_absolute = fun ~workspace_root path ->
  let normalize = Path.normalize in
  let path = normalize path in
  match workspace_root with
  | Some root -> (
      let root = normalize root in
      match Path.strip_prefix path ~prefix:root with
      | Ok rel -> Path.to_string rel
      | Error _ -> Path.to_string path
    )
  | None -> Path.to_string path

let workspace_scope = fun (workspace: Workspace.t option) ->
  let scope_of_path path =
    let package_toml = Path.(path / Path.v "riot.toml") in
    { package_root = path; config = Fmt_config.load package_toml }
  in
  match workspace with
  | Some workspace ->
      let workspace_toml = Path.(workspace.root / Path.v "riot.toml") in
      Some {
        workspace_root = workspace.root;
        workspace_config = Fmt_config.load workspace_toml;
        packages = workspace.packages |> List.map (fun (pkg: Package.t) -> scope_of_path pkg.path)
      }
  | None ->
      let cwd = Env.current_dir () |> Result.unwrap_or ~default:(Path.v ".") in
      let toml_path = Path.(cwd / Path.v "riot.toml") in
      if Fs.exists toml_path |> Result.unwrap_or ~default:false then
        Some { workspace_root = cwd; workspace_config = Fmt_config.load toml_path; packages = [] }
      else
        None

let resolve_root = fun (workspace: Workspace.t option) ->
  match workspace with
  | Some workspace -> workspace.root
  | None -> Env.current_dir () |> Result.unwrap_or ~default:(Path.v ".")

let default_registry_name = "pkgs.ml"

let workspace_manifest_path = fun (workspace: Workspace.t) ->
  Path.(workspace.root / Path.v "riot.toml")

let workspace_can_be_prepared = fun (workspace: Workspace.t) ->
  match Fs.exists (workspace_manifest_path workspace) with
  | Ok true -> true
  | Ok false
  | Error _ -> false

let prepare_workspace = fun (workspace: Workspace.t) ->
  if not (workspace_can_be_prepared workspace) then
    Ok workspace
  else
    match Pkgs_ml.Registry.create_filesystem ?riot_home:None ~registry_name:default_registry_name () with
    | Error err -> Error (RegistryInitializationFailed {
      registry = default_registry_name;
      error = err
    })
    | Ok registry -> Riot_deps.ensure_workspace
      ~mode:Riot_deps.Dep_solver.Refresh
      ~registry
      ~workspace
      ()
    |> Result.map_error
      (fun err ->
        WorkspacePreparationFailed { path = workspace.root; error = Riot_model.Pm_error.message err })

let resolve_search_roots = fun ?package_filter (workspace: Workspace.t option) ->
  match workspace, package_filter with
  | Some workspace, Some package_name -> (
      match workspace_roots_for_package workspace package_name with
      | [] -> Error (UnknownPackage { package_name })
      | roots -> Ok roots
    )
  | Some workspace, None ->
      Ok (workspace_roots workspace)
  | None, Some package_name ->
      Error (PackageFilterRequiresWorkspace { package_name })
  | None, None ->
      Ok [ resolve_root None ]

let matches_ignore_pattern = fun ~root pattern path ->
  let rel =
    match Path.strip_prefix path ~prefix:root with
    | Ok rel -> Path.to_string rel
    | Error _ -> Path.to_string path
  in
  String.contains rel pattern

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
      if
        List.exists
          (fun pattern -> matches_ignore_pattern ~root:scope.workspace_root pattern file)
          scope.workspace_config.ignore_patterns
      then
        true
      else
        match find_package_scope scope file with
        | Some package_scope -> List.exists
          (fun pattern -> matches_ignore_pattern ~root:package_scope.package_root pattern file)
          package_scope.config.ignore_patterns
        | None -> false

let resolve_explicit_target_path = fun ?workspace path ->
  if Path.is_absolute path then
    Path.normalize path
  else
    let cwd_candidate =
      match Env.current_dir () with
      | Ok cwd -> Path.normalize Path.(cwd / path)
      | Error _ -> Path.normalize path
    in
    match workspace with
    | None -> cwd_candidate
    | Some (workspace: Workspace.t) ->
        let workspace_contains path =
          let root = Path.normalize workspace.root in
          let path = Path.normalize path in
          Path.equal path root || match Path.strip_prefix path ~prefix:root with
          | Ok _ -> true
          | Error _ -> false
        in
        if workspace_contains cwd_candidate then
          cwd_candidate
        else
          let workspace_candidate = Path.normalize Path.(workspace.root / path) in
          if Path.exists workspace_candidate then
            workspace_candidate
          else
            cwd_candidate

let validate_explicit_target = fun ?workspace path ->
  let resolved = resolve_explicit_target_path ?workspace path in
  if not (Path.exists resolved) then
    Error (InvalidPath { path; reason = "path does not exist" })
  else if Path.is_file resolved && not (is_supported_source_file resolved) then
    Error (InvalidPath { path; reason = "path is not an OCaml source file (.ml/.mli) or directory" })
  else
    Ok resolved

let validate_explicit_targets = fun ?workspace roots ->
  let rec loop roots acc =
    match roots with
    | [] -> Ok acc
    | head :: tail -> (
        match validate_explicit_target ?workspace head with
        | Error _ as err -> err
        | Ok root -> loop tail (root :: acc)
      )
  in
  loop roots []

let resolve_targets = fun ?workspace ?package_filter paths ->
  let scope = workspace_scope workspace in
  let collect_ordered_files roots =
    let explicit_files, directory_roots =
      roots
      |> List.fold_left
        (fun (files, directories) root ->
          if Path.is_file root then
            (root :: files, directories)
          else
            (files, root :: directories))
        ([], [])
    in
    let walked_files = directory_roots
    |> List.concat_map
      (fun root ->
        Krasny.Runner.collect_ocaml_files ~should_ignore:(should_ignore_file scope) ~roots:[ root ] ()
        |> List.sort compare_paths) in
    dedupe_paths (explicit_files @ walked_files)
  in
  let roots =
    if List.is_empty paths then
      resolve_search_roots ?package_filter workspace
    else
      validate_explicit_targets ?workspace paths |> Result.map (List.sort_uniq compare_paths)
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
  | ExplainAndPath { path } -> "cannot use --explain together with a path ("
  ^ Path.to_string path
  ^ ")"
  | InvalidPath { path; reason } -> "invalid check path " ^ Path.to_string path ^ ": " ^ reason
  | NoTargets -> "no OCaml files found"
  | PackageFilterRequiresWorkspace { package_name } -> "cannot use --package " ^ package_name ^ " outside a riot workspace"
  | UnknownPackage { package_name } -> "unknown workspace package: " ^ package_name
  | UnknownDiagnosticId { diagnostic_id } -> "unknown typ diagnostic id: " ^ diagnostic_id
  | RegistryInitializationFailed { registry; error } -> "failed to initialize registry '"
  ^ registry
  ^ "': "
  ^ error
  | WorkspacePreparationFailed { path; error } -> "failed to prepare workspace '"
  ^ Path.to_string path
  ^ "': "
  ^ error

let fail = fun ?(stderr = default_stderr) err ->
  stderr ("\027[1;31mError\027[0m: " ^ message err ^ "\n");
  Error (Failure (message err))

let action_of_matches = fun matches ->
  let json = ArgParser.get_flag matches "json" in
  let quiet = ArgParser.get_flag matches "quiet" in
  let package_filter = ArgParser.get_one matches "package" in
  let paths = ArgParser.get_many matches "path" |> List.map Path.v in
  match ArgParser.get_one matches "explain", paths with
  | Some diagnostic_id, [] -> Ok (Explain { diagnostic_id; json })
  | Some _, path :: _ -> Error (ExplainAndPath { path })
  | None, paths -> Ok (Check { paths; package_filter; json; quiet })

let diagnostics_of_report = fun (report: Typ.Check_result.t) ->
  (report.parse_diagnostics |> List.map (fun diagnostic -> Parse diagnostic))
  @ (report.lowering_diagnostics |> List.map (fun diagnostic -> Lowering diagnostic))
  @ (report.typing_diagnostics |> List.map (fun diagnostic -> Typing diagnostic))

let has_errors = fun diagnostics ->
  List.exists
    (
      function
      | Parse _ -> true
      | Lowering diagnostic
      | Typing diagnostic -> (
          match Typ.Diagnostic.severity diagnostic with
          | Typ.Diagnostic.Error -> true
          | Typ.Diagnostic.Warning -> false
        )
    )
    diagnostics

let has_warning_diagnostic = function
  | Parse _ -> false
  | Lowering diagnostic
  | Typing diagnostic -> (Typ.Diagnostic.severity diagnostic = Typ.Diagnostic.Warning)

let has_warnings = fun diagnostics ->
  List.exists has_warning_diagnostic diagnostics

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
      let warning_count = diagnostics |> List.filter has_warning_diagnostic |> List.length in
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

let severity_style = fun severity ->
  match severity with
  | "warning" -> Style.default |> Style.fg (Color.make "#E5C07B") |> Style.bold
  | "note" -> Style.default |> Style.fg (Color.make "#56B6C2") |> Style.bold
  | _ -> Style.default |> Style.fg (Color.make "#E06C75") |> Style.bold

let fix_style = Style.default |> Style.fg (Color.make "#98C379") |> Style.bold

let diagnostic_fix = function
  | Parse diagnostic -> Syn.Diagnostic.fix_message diagnostic
  | _ -> None

let diagnostic_expected = function
  | Parse diagnostic -> (
      let expected = Syn.Diagnostic.expected_message diagnostic in
      if String.length expected = 0 then
        None
      else
        Some ("expected " ^ expected)
    )
  | _ -> None

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
    (
      "span",
      span_to_json
        (
          match diagnostic with
          | Parse diagnostic -> diagnostic.Syn.Diagnostic.span
          | Lowering diagnostic
          | Typing diagnostic -> Typ.Diagnostic.primary_span diagnostic
        )
    );
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
    let line_idx =
      loop 0 last 0
      |> fun line_idx ->
        Int.min last (Int.max 0 line_idx)
    in
    let _line_start = line_starts.(line_idx) in
    (line_idx, Int.max 0 (position_of_offset source_text pos).character)

let extract_snippet = fun source_layout source_text (span: Syn.Ceibo.Span.t) ->
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
  let span: Syn.Ceibo.Span.t = span_of_diagnostic diagnostic in
  let start_position = position_of_offset source_text span.start in
  let line = start_position.line + 1 in
  let column = start_position.character + 1 in
  let severity = severity_string_of_diagnostic diagnostic in
  let style = severity_style severity in
  let severity_label = Style.styled style (severity ^ ":") in
  let header =
    path_text
    ^ ":"
    ^ Int.to_string line
    ^ ":"
    ^ Int.to_string column
    ^ ":"
    ^ "\n\n"
    ^ severity_label
    ^ " ["
    ^ phase_of_diagnostic diagnostic
    ^ "] "
    ^ code_of_diagnostic diagnostic
    ^ ": "
    ^ message_of_diagnostic diagnostic
  in
  let explain_msg =
    match diagnostic with
    | Parse diagnostic ->
        let id = Syn.Diagnostic.id diagnostic in
        "  For more information about this error, try `riot fmt --explain " ^ id ^ "`"
    | Lowering _
    | Typing _ -> "  For more information about this error, try `riot check --explain "
    ^ code_of_diagnostic diagnostic
    ^ "`"
  in
  match extract_snippet source_layout source_text span with
  | None -> header
  | Some (line_num, start_col, code_line, pointer_span) ->
      let line_label = Int.to_string line_num in
      let indent_prefix = String.make (String.length line_label + 1) ' ' in
      let pointer = String.make (Int.max 0 start_col) ' ' ^ String.make pointer_span '^' in
      let styled_pointer = Style.styled style pointer in
      let styled_expected =
        match diagnostic_expected diagnostic with
        | None -> ""
        | Some msg -> " " ^ (Style.styled style msg)
      in
      let fix_line =
        match diagnostic_fix diagnostic with
        | None -> ""
        | Some msg -> indent_prefix ^ Style.styled fix_style "fix:" ^ " " ^ msg ^ "\n\n"
      in
      header
      ^ "\n"
      ^ indent_prefix
      ^ "|\n"
      ^ line_label
      ^ " | "
      ^ code_line
      ^ "\n"
      ^ indent_prefix
      ^ "| "
      ^ styled_pointer
      ^ styled_expected
      ^ "\n"
      ^ indent_prefix
      ^ "|\n\n"
      ^ fix_line
      ^ indent_prefix
      ^ explain_msg
      ^ "\n"

let read_report_to_json = fun ~workspace_root ~path reason ->
  Data.Json.Object [
    ("path", Data.Json.String (relative_or_absolute ~workspace_root path));
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

let report_of_analysis = fun path (analysis: Typ.SourceAnalysis.t) ->
  let (item_tree, body_arena, origin_map) =
    match analysis.semantic_tree with
    | Some semantic_tree -> (
      Some semantic_tree.item_tree,
      Some semantic_tree.body_arena,
      Some semantic_tree.origin_map
    )
    | None -> (None, None, None)
  in
  {
    Typ.Check_result.source_id = analysis.source.source_id;
    filename = path;
    source = analysis.source.text;
    parse_diagnostics = analysis.parse_diagnostics;
    item_tree;
    body_arena;
    origin_map;
    semantic_tree = analysis.semantic_tree;
    lowering_diagnostics = analysis.lowering_diagnostics;
    typing_diagnostics = analysis.typing_diagnostics;
    file_summary = analysis.file_summary;
    type_index = analysis.type_index;
    exports = Typ.SourceAnalysis.exports analysis;
    item_traces = analysis.item_traces;
    expr_traces = analysis.expr_traces;
  }

let workspace_package_by_name = fun (workspace: Workspace.t) package_name ->
  workspace.packages |> List.find_opt
    (fun (pkg: Package.t) ->
      String.equal pkg.name package_name)

let package_typ_source_files = fun ?(include_dev = false) (pkg: Package.t) ->
  let scoped_sources =
    if include_dev then
      pkg.sources.src @ pkg.sources.tests @ pkg.sources.examples @ pkg.sources.bench
    else
      pkg.sources.src
  in
  scoped_sources
  |> List.filter is_supported_source_file
  |> List.map (fun relative -> Path.(pkg.path / relative))
  |> dedupe_paths

let package_library_typ_source_files = fun (pkg: Package.t) ->
  match pkg.library with
  | None -> package_typ_source_files pkg
  | Some { path=library_path } ->
      let interface_path = Path.(add_extension (remove_extension library_path) ~ext:"mli") in
      let has_interface =
        match Fs.exists interface_path with
        | Ok true -> true
        | Ok false
        | Error _ -> false
      in
      let files =
        if has_interface then
          [ interface_path; library_path ]
        else
          [ library_path ]
      in
      files |> List.filter is_supported_source_file |> dedupe_paths

let package_typ_support_source_files = fun (pkg: Package.t) ->
  package_typ_source_files ~include_dev:false pkg

let merge_module_exports = fun preferred fallback ->
  let rec loop seen acc remaining =
    match remaining with
    | [] -> List.rev acc
    | ((name, _) as export) :: tail ->
        if List.mem name seen then
          loop seen acc tail
        else
          loop (name :: seen) (export :: acc) tail
  in
  loop [] [] (preferred @ fallback)

let type_decl_key = fun (type_decl: Typ.FileSummary.type_decl) ->
  String.concat "." (type_decl.scope_path @ [ type_decl.declaration.type_name ])

let merge_module_type_decls = fun preferred fallback ->
  let rec loop seen acc remaining =
    match remaining with
    | [] -> List.rev acc
    | ((type_decl: Typ.FileSummary.type_decl) as candidate) :: tail ->
        let key = type_decl_key candidate in
        if List.mem key seen then
          loop seen acc tail
        else
          loop (key :: seen) (candidate :: acc) tail
  in
  loop [] [] (preferred @ fallback)

let typings_with_payload = fun template ~source_hash ~exports ~type_decls ->
  let module_name = Typ.ModuleTypings.module_name template in
  match Typ.ModuleTypings.export_result template, exports with
  | Typ.FileSummary.TrustedExport _, _ -> Typ.ModuleTypings.trusted
    ~module_name
    ~source_hash
    ~type_decls
    exports
  | Typ.FileSummary.ErroredExport _, _ -> Typ.ModuleTypings.errored
    ~module_name
    ~source_hash
    ~type_decls
    exports
  | Typ.FileSummary.NoExport, [] -> Typ.ModuleTypings.missing ~module_name ~source_hash ~type_decls ()
  | Typ.FileSummary.NoExport, _ -> Typ.ModuleTypings.errored ~module_name ~source_hash ~type_decls exports

let merge_module_typings = fun preferred fallback ->
  let module_name = Typ.ModuleTypings.module_name preferred in
  let exports = merge_module_exports
    (Typ.ModuleTypings.exports preferred)
    (Typ.ModuleTypings.exports fallback) in
  let type_decls = merge_module_type_decls
    (Typ.ModuleTypings.type_decls preferred)
    (Typ.ModuleTypings.type_decls fallback) in
  let template =
    match Typ.ModuleTypings.export_result preferred, exports with
    | Typ.FileSummary.NoExport, _ :: _ -> fallback
    | _ -> preferred
  in
  let source_hash = Typ.ModuleTypings.synthetic_source_hash
    ~module_name
    ~export_result:(Typ.ModuleTypings.export_result template)
    ~type_decls in
  typings_with_payload template ~source_hash ~exports ~type_decls

let merge_loaded_module_typings = fun preferred fallback ->
  let rec loop order merged remaining =
    match remaining with
    | [] ->
        order |> List.rev |> List.filter_map
          (fun module_name ->
            List.assoc_opt module_name merged)
    | summary :: tail ->
        let module_name = Typ.ModuleTypings.module_name summary in
        let (order, merged) =
          match List.assoc_opt module_name merged with
          | None -> (module_name :: order, (module_name, summary) :: merged)
          | Some existing -> (
            order,
            (module_name, merge_module_typings existing summary) :: List.remove_assoc module_name merged
          )
        in
        loop order merged tail
  in
  loop [] [] (preferred @ fallback)

let workspace_typ_store_root = fun (workspace: Workspace.t) ->
  Path.(workspace.target_dir_root / Path.v "typ-cache")

let workspace_typ_store = fun (workspace: Workspace.t) ->
  let contentstore = Contentstore.create
    ~root:(workspace_typ_store_root workspace)
    ~policy:Contentstore.Policy.default
    () in
  Typ.Store.create contentstore ()

let workspace_build_store = fun (workspace: Workspace.t) ~profile ~target ->
  Riot_store.Store.create_for_lane ~workspace ~profile ~target

let resolve_typ_profile = fun ~(workspace:Workspace.t) ~(pkg:Package.t) ->
  let base_profile = Profile.apply_overrides Profile.debug workspace.profile_overrides in
  let profile = Profile.apply_overrides base_profile pkg.compiler.profile_overrides in
  let target_platform = Build_ctx.make ~session_id:(Session_id.make ()) ~profile () |> Build_ctx.target_platform_name in
  match List.assoc_opt target_platform pkg.compiler.target_overrides with
  | Some { Package.profile_override=Some override } -> Profile.apply_override profile override
  | Some { Package.profile_override=None }
  | None -> profile

let workspace_typ_toolchain = fun (workspace: Workspace.t) ->
  Riot_toolchain.init ~config:(Toolchain_config.from_workspace workspace)

let source_origin_path_for_module = fun (mod_: Riot_model.Module.t) ->
  let qualified_name = mod_ |> Riot_model.Module.module_name |> Riot_model.Module_name.qualified_name in
  match Riot_model.Module.kind mod_ with
  | `interface -> Path.v (qualified_name ^ ".mli")
  | `implementation -> Path.v (qualified_name ^ ".ml")

let public_module_name_of_module = fun (pkg: Package.t) (mod_: Riot_model.Module.t) ->
  let package_namespace = String.capitalize_ascii pkg.name in
  let module_name = Riot_model.Module.module_name mod_ in
  let namespace = module_name |> Riot_model.Module_name.namespace |> Riot_model.Namespace.to_list in
  let simple_name = Riot_model.Module_name.to_string module_name in
  match namespace with
  | [] when String.equal simple_name package_namespace -> Some simple_name
  | [ root ] when String.equal root package_namespace -> Some simple_name
  | _ -> None

let injected_open_lines = fun (
  open_modules: Riot_planner.Module_node.t Std.Graph.SimpleGraph.node list
) ->
  open_modules |> List.filter_map
    (fun (node: Riot_planner.Module_node.t Std.Graph.SimpleGraph.node) ->
      match node.value.kind with
      | Riot_planner.Module_node.ML mod_
      | Riot_planner.Module_node.MLI mod_ -> Some ("open " ^ Riot_model.Module.namespaced_name mod_)
      | _ -> None)

let planner_source_text = fun ~pkg (node: Riot_planner.Module_node.t Std.Graph.SimpleGraph.node) ->
  let source_result =
    match node.value.file with
    | Riot_planner.Module_node.Concrete path ->
        let display_path = Path.(pkg.Package.path / path) in
        Fs.read display_path |> Result.map (fun text -> (text, display_path))
    | Riot_planner.Module_node.Generated { path; contents } ->
        let display_path = Path.(pkg.Package.path / path) in
        Ok (contents, display_path)
  in
  match source_result with
  | Error _ as err -> err
  | Ok (raw_text, display_path) ->
      let prelude = injected_open_lines node.value.open_modules in
      let text =
        if List.is_empty prelude then
          raw_text
        else
          String.concat "\n" (prelude @ [ ""; raw_text ])
      in
      Ok (text, display_path)

let sanitize_module_name = fun name ->
  String.map
    (fun ch ->
      if ch = '-' then
        '_'
      else
        ch)
    name

let module_name_for_path = fun path ->
  path |> Path.remove_extension |> Path.basename |> sanitize_module_name |> String.capitalize_ascii

let source_rank_for_typ_order = fun path ->
  match Path.extension path with
  | Some ".mli" -> 0
  | Some ".ml" -> 1
  | _ -> 2

let package_typ_sources_from_planner = fun ~(workspace:Workspace.t) ~(pkg:Package.t) ->
  match workspace_typ_toolchain workspace with
  | Error _ -> None
  | Ok toolchain ->
      let profile = resolve_typ_profile ~workspace ~pkg in
      let build_ctx = Build_ctx.make ~session_id:(Session_id.make ()) ~profile () in
      let store = workspace_build_store
        workspace
        ~profile:profile.name
        ~target:(Riot_dirs.host_target ()) in
      let input =
        Riot_planner.Module_planner.{
          package = pkg;
          profile;
          ctx = build_ctx;
          toolchain;
          workspace;
          planning_root = Path.v "src";
          depset = [];
          store;
        }
      in
      match Riot_planner.Module_planner.plan_node input with
      | Error _ -> None
      | Ok plan -> (
          match Std.Graph.SimpleGraph.topo_sort plan.module_graph with
          | Error _ -> None
          | Ok nodes ->
              nodes |> List.filter_map
                (fun (node: Riot_planner.Module_node.t Std.Graph.SimpleGraph.node) ->
                  match node.value.kind with
                  | Riot_planner.Module_node.ML mod_
                  | Riot_planner.Module_node.MLI mod_ -> (
                      match planner_source_text ~pkg node with
                      | Error _ -> None
                      | Ok (text, display_path) ->
                          let source_id = Typ.SourceId.of_int
                            (Std.Graph.SimpleGraph.Node_id.to_int node.id) in
                          let source =
                            Typ.Source.make ~source_id
                              ~kind:((
                                match node.value.file with
                                | Riot_planner.Module_node.Generated _ -> Typ.Source.Generated
                                | Riot_planner.Module_node.Concrete _ -> Typ.Source.File
                              ))
                              ~origin:(Typ.Source.Path (source_origin_path_for_module mod_))
                              ~revision:0
                              ~text
                          in
                          Some {
                            internal_module_name = mod_
                            |> Riot_model.Module.module_name
                            |> Riot_model.Module_name.qualified_name;
                            public_module_name = public_module_name_of_module pkg mod_;
                            display_path;
                            source_id;
                            source;
                          }
                    )
                  | Riot_planner.Module_node.C
                  | Riot_planner.Module_node.H
                  | Riot_planner.Module_node.Other _
                  | Riot_planner.Module_node.Root
                  | Riot_planner.Module_node.Native _
                  | Riot_planner.Module_node.Library _
                  | Riot_planner.Module_node.Binary _ -> None) |> Option.some
        )

let qualify_typings_exports = fun module_name exports ->
  List.map (fun (name, scheme) -> (module_name ^ "." ^ name, scheme)) exports

let qualify_typings_type_decls = fun module_name type_decls ->
  List.map
    (fun (type_decl: Typ.FileSummary.type_decl) ->
      {
        Typ.FileSummary.scope_path = module_name :: type_decl.scope_path;
        declaration = type_decl.declaration
      })
    type_decls

let ambient_env_for_loaded_modules = fun current_module_name loaded_modules ->
  loaded_modules
  |> List.filter
    (fun typings -> not (String.equal current_module_name (Typ.ModuleTypings.module_name typings)))
  |> List.map
    (fun typings ->
      qualify_typings_exports
        (Typ.ModuleTypings.module_name typings)
        (Typ.ModuleTypings.exports typings))
  |> List.flatten

let ambient_type_decls_for_loaded_modules = fun current_module_name loaded_modules ->
  loaded_modules
  |> List.filter
    (fun typings -> not (String.equal current_module_name (Typ.ModuleTypings.module_name typings)))
  |> List.map
    (fun typings ->
      qualify_typings_type_decls
        (Typ.ModuleTypings.module_name typings)
        (Typ.ModuleTypings.type_decls typings))
  |> List.flatten

let readable_typ_source_of_prepared = function
  | Unreadable_source _ -> None
  | Readable_source { path; source=text; source_id; typ_source } -> Some {
    path;
    text;
    source_id;
    source = typ_source
  }

let create_typ_session = fun config paths ->
  let rec loop session prepared_sources = function
    | [] -> (session, List.rev prepared_sources)
    | path :: tail -> (
        match Fs.read path with
        | Error err -> loop
          session
          (Unreadable_source { path; reason = IO.error_message err } :: prepared_sources)
          tail
        | Ok source ->
            let parse_result = Syn.parse ~filename:path source in
            let cst = Syn.build_cst parse_result in
            let (session, source_id) = Typ.Session.create_prepared_source
              session
              ~kind:Typ.Source.File
              ~origin:(Typ.Source.Path path)
              ~text:source
              ~parse_result
              ~cst in
            let typ_source = Typ.Source.make_prepared
              ~source_id
              ~kind:Typ.Source.File
              ~origin:(Typ.Source.Path path)
              ~revision:0
              ~text:source
              ~parse_result
              ~cst in
            loop
              session
              (Readable_source { path; source; source_id; typ_source } :: prepared_sources)
              tail
      )
  in
  loop (Typ.Session.empty ~config) [] paths

let source_id_of_prepared = function
  | Readable_source { source_id; _ } -> Some source_id
  | Unreadable_source _ -> None

let missing_requirements_reason = fun missing ->
  "missing type requirements: " ^ Data.Json.to_string (Typ.MissingRequirements.to_json missing)

let readable_typ_sources_from_paths = fun paths ->
  paths |> List.fold_left
    (fun (next_source_id, readable_sources) path ->
      match Fs.read path with
      | Error _ -> (next_source_id, readable_sources)
      | Ok text ->
          let source_id = Typ.SourceId.of_int next_source_id in
          let parse_result = Syn.parse ~filename:path text in
          let cst = Syn.build_cst parse_result in
          let readable_source = {
            path;
            text;
            source_id;
            source = Typ.Source.make_prepared
              ~source_id
              ~kind:Typ.Source.File
              ~origin:(Typ.Source.Path path)
              ~revision:0
              ~text
              ~parse_result
              ~cst
          } in
          (next_source_id + 1, readable_sources @ [ readable_source ]))
    (1, []) |> snd

let local_module_dependencies_for_source = fun local_module_names (source: readable_typ_source) ->
  match Syn.Deps.of_parse_result source.source.parse_result with
  | Ok deps -> Syn.Deps.modules deps
  |> List.filter
    (fun module_name ->
      Collections.HashSet.contains local_module_names module_name
      && not (String.equal module_name (module_name_for_path source.path)))
  |> List.sort_uniq String.compare
  | Error _ -> []

let ordered_readable_typ_sources = fun readable_sources ->
  let grouped_by_module =
    readable_sources
    |> List.fold_left
      (fun grouped (source: readable_typ_source) ->
        let module_name = module_name_for_path source.path in
        let existing =
          match List.assoc_opt module_name grouped with
          | Some sources -> sources
          | None -> []
        in
        (module_name, existing @ [ source ]) :: List.remove_assoc module_name grouped)
      []
    |> List.rev
    |> List.map
      (fun (module_name, sources) ->
        let sources =
          List.sort
            (fun (left: readable_typ_source) (right: readable_typ_source) ->
              Int.compare
                (source_rank_for_typ_order left.path)
                (source_rank_for_typ_order right.path))
            sources
        in
        (module_name, sources))
  in
  let module_order = grouped_by_module |> List.map fst in
  let local_module_names = Collections.HashSet.of_list module_order in
  let deps_by_module =
    grouped_by_module
    |> List.map
      (fun (module_name, sources) ->
        let dependencies = sources
        |> List.concat_map (local_module_dependencies_for_source local_module_names)
        |> List.sort_uniq String.compare in
        (module_name, dependencies))
  in
  let permanent = Collections.HashSet.with_capacity (List.length module_order) in
  let temporary = Collections.HashSet.with_capacity (List.length module_order) in
  let rec visit ordered module_name =
    if Collections.HashSet.contains permanent module_name then
      ordered
    else if Collections.HashSet.contains temporary module_name then
      ordered
    else
      let () = Collections.HashSet.insert temporary module_name |> ignore in
      let ordered =
        match List.assoc_opt module_name deps_by_module with
        | Some dependencies -> dependencies |> List.fold_left visit ordered
        | None -> ordered
      in
      let () = Collections.HashSet.remove temporary module_name |> ignore in
      let () = Collections.HashSet.insert permanent module_name |> ignore in
      module_name :: ordered
  in
  let ordered_modules = module_order |> List.fold_left visit [] |> List.rev in
  ordered_modules |> List.concat_map
    (fun module_name ->
      match List.assoc_opt module_name grouped_by_module with
      | Some sources -> sources
      | None -> [])

let analyze_package_typ_sources_in_order = fun config ordered_sources ->
  let rec loop loaded_modules analyses = function
    | [] -> List.rev analyses
    | (source: package_typ_source) :: rest ->
        let config = config
        |> Typ.Config.with_loaded_modules ~loaded_modules
        |> Typ.Config.with_ambient
          ~ambient:(ambient_env_for_loaded_modules source.internal_module_name loaded_modules)
        |> Typ.Config.with_ambient_type_decls
          ~ambient_type_decls:(ambient_type_decls_for_loaded_modules source.internal_module_name loaded_modules) in
        let analysis = Typ.SourceAnalysis.analyze ~config source.source in
        let typings = Typ.ModuleTypings.of_file_summary
          ~module_name:source.internal_module_name
          ~source_hash:(Typ.Source.input_hash source.source)
          analysis.file_summary in
        let () =
          match config.Typ.Config.store with
          | Some store -> ignore (Typ.Store.save_module_typings store typings)
          | None -> ()
        in
        let loaded_modules = merge_loaded_module_typings [ typings ] loaded_modules in
        loop loaded_modules ((source.source_id, analysis) :: analyses) rest
  in
  loop config.Typ.Config.loaded_modules [] ordered_sources

let package_module_typings_of_analyses = fun ordered_sources analyses ->
  analyses |> List.filter_map
    (fun (source_id, analysis) ->
      match
        List.find_opt
          (fun (source: package_typ_source) ->
            Typ.SourceId.equal source.source_id source_id)
          ordered_sources
      with
      | None -> None
      | Some { public_module_name=None; _ } -> None
      | Some source -> Some (Typ.ModuleTypings.of_file_summary
        ~module_name:(Option.expect ~msg:"public module name" source.public_module_name)
        ~source_hash:(Typ.Source.input_hash source.source)
        analysis.Typ.SourceAnalysis.file_summary)) |> merge_loaded_module_typings []

let load_package_module_typings_from_store = fun store (pkg: Package.t) ->
  Typ.Store.load_package_module_typings store ~package_name:pkg.name

let persist_module_typings = fun store ?package_name typings ->
  typings |> List.iter (fun typings -> ignore (Typ.Store.save_module_typings store typings));
  match package_name with
  | Some package_name -> ignore (Typ.Store.save_package_module_typings store ~package_name typings)
  | None -> ()

let workspace_dependency_packages = fun ~include_dev (workspace: Workspace.t) (pkg: Package.t) ->
  let dependencies =
    if include_dev then
      Package.build_graph_dependencies pkg
    else
      pkg.dependencies
  in
  dependencies
  |> List.filter_map
    (fun (dependency: Package.dependency) -> workspace_package_by_name workspace dependency.name)
  |> List.sort_uniq
    (fun (left: Package.t) (right: Package.t) ->
      String.compare left.name right.name)

let workspace_module_typings_for_package =
  let rec load cache typ_store (workspace: Workspace.t) ?(visiting = []) (pkg: Package.t) =
    match List.assoc_opt pkg.name !cache with
    | Some typings ->
        typings
    | None when List.mem pkg.name visiting ->
        []
    | None ->
        let dependency_typings = workspace_dependency_packages ~include_dev:false workspace pkg
        |> List.concat_map
          (fun dependency_pkg -> load cache typ_store workspace ~visiting:((pkg.name :: visiting)) dependency_pkg) in
        let package_typings =
          match load_package_module_typings_from_store typ_store pkg with
          | Some typings -> typings
          | None ->
              let loaded_modules = merge_loaded_module_typings
                dependency_typings
                Typ.Config.default.loaded_modules in
              let config = Typ.Config.default
              |> Typ.Config.with_loaded_modules ~loaded_modules
              |> Typ.Config.with_store ~store:(Some typ_store)
              |> Typ.Config.with_capture_traces ~capture_traces:false in
              let typings =
                match package_typ_sources_from_planner ~workspace ~pkg with
                | Some ordered_sources -> analyze_package_typ_sources_in_order config ordered_sources
                |> package_module_typings_of_analyses ordered_sources
                | None ->
                    let session_paths = package_typ_support_source_files pkg in
                    let root_paths = package_library_typ_source_files pkg in
                    let (session, prepared_sources) = create_typ_session config session_paths in
                    prepared_sources |> List.filter_map
                      (
                        function
                        | Readable_source { path; source_id; _ } when List.exists
                          (fun root_path ->
                            Path.equal root_path path)
                          root_paths -> Some (module_name_for_path path, source_id)
                        | Readable_source _
                        | Unreadable_source _ -> None
                      ) |> List.fold_left
                      (fun grouped (module_name, source_id) ->
                        let existing =
                          match List.assoc_opt module_name grouped with
                          | Some source_ids -> source_ids
                          | None -> []
                        in
                        (module_name, existing @ [ source_id ]) :: List.remove_assoc module_name grouped)
                      [] |> List.rev |> List.concat_map
                      (fun (_module_name, roots) ->
                        match Typ.Session.prepare_snapshot session ~roots with
                        | Ok snapshot -> Typ.Snapshot.module_typings snapshot
                        | Error _ -> [])
              in
              let () = persist_module_typings typ_store ~package_name:pkg.name typings in
              typings
        in
        let typings = merge_loaded_module_typings package_typings dependency_typings in
        let () =
          cache := (pkg.name, typings) :: !cache
        in
        typings
  in
  load

let workspace_typing_targets = fun (workspace: Workspace.t) package_names ->
  if List.is_empty package_names then
    workspace.packages |> List.filter Package.is_workspace_member
  else
    package_names |> List.filter_map (workspace_package_by_name workspace) |> List.sort_uniq
      (fun (left: Package.t) (right: Package.t) ->
        String.compare left.name right.name)

let populate_workspace_typings = fun ~workspace ~package_names () ->
  match prepare_workspace workspace with
  | Error _ -> ()
  | Ok prepared_workspace ->
      let typ_store = workspace_typ_store prepared_workspace in
      let summary_cache = ref [] in
      workspace_typing_targets prepared_workspace package_names |> List.iter
        (fun pkg ->
          let _ = workspace_module_typings_for_package summary_cache typ_store prepared_workspace pkg in
          ())

let typ_config_for_source_group = fun ?workspace ~summary_cache paths ->
  match workspace, paths with
  | Some workspace, path :: _ -> (
      match Workspace.find_package_for_path workspace ~path with
      | None -> Typ.Config.default |> Typ.Config.with_capture_traces ~capture_traces:false
      | Some pkg ->
          let typ_store = workspace_typ_store workspace in
          let dependency_typings = workspace_dependency_packages ~include_dev:true workspace pkg
          |> List.concat_map
            (fun dependency_pkg ->
              workspace_module_typings_for_package summary_cache typ_store workspace dependency_pkg) in
          let loaded_modules = merge_loaded_module_typings dependency_typings Typ.Config.default.loaded_modules in
          Typ.Config.default
          |> Typ.Config.with_loaded_modules ~loaded_modules
          |> Typ.Config.with_store ~store:(Some typ_store)
          |> Typ.Config.with_capture_traces ~capture_traces:false
    )
  | _ -> Typ.Config.default |> Typ.Config.with_capture_traces ~capture_traces:false

let path_key = fun path -> Path.normalize path |> Path.to_string

let checked_file_path = function
  | Typed { path; _ }
  | Unreadable { path; _ } -> path

let checked_file_of_analysis = fun path (analysis: Typ.SourceAnalysis.t) ->
  let report = report_of_analysis path analysis in
  let diagnostics = diagnostics_of_report report in
  Typed { path; report; diagnostics }

let prepared_source_path = function
  | Readable_source { path; _ }
  | Unreadable_source { path; _ } -> path

let prepared_sources_by_path = fun prepared_sources ->
  prepared_sources
  |> List.fold_left
    (fun prepared_by_path prepared -> (path_key (prepared_source_path prepared), prepared) :: prepared_by_path)
    []

type checked_group = {
  checked_files: checked_file list;
  module_typings: Typ.ModuleTypings.t list;
}

let checked_group_for_session = fun config session prepared_by_path root_paths ->
  let target_prepared_sources =
    root_paths
    |> List.filter_map
      (fun path ->
        List.assoc_opt (path_key path) prepared_by_path)
  in
  let root_source_ids = target_prepared_sources |> List.filter_map source_id_of_prepared in
  match Typ.Session.prepare_snapshot session ~roots:root_source_ids with
  | Error missing ->
      let reason = missing_requirements_reason missing in
      {
        checked_files =
          target_prepared_sources |> List.map
            (
              function
              | Unreadable_source { path; reason } -> Unreadable { path; reason }
              | Readable_source { path; _ } -> Unreadable { path; reason }
            );
        module_typings = [];
      }
  | Ok snapshot ->
      let () =
        match config.Typ.Config.store with
        | Some store -> persist_module_typings store (Typ.Snapshot.module_typings snapshot)
        | None -> ()
      in
      {
        checked_files =
          target_prepared_sources |> List.map
            (
              function
              | Unreadable_source { path; reason } -> Unreadable { path; reason }
              | Readable_source { path; source_id; _ } -> (
                  match Typ.Query.analysis_of_source snapshot source_id with
                  | Some analysis -> checked_file_of_analysis path analysis
                  | None ->
                      let reason = "missing type analysis for " ^ Path.to_string path in
                      Unreadable { path; reason }
                )
            );
        module_typings = Typ.Snapshot.module_typings snapshot;
      }

let checked_files_for_session = fun config session prepared_by_path root_paths ->
  checked_group_for_session config session prepared_by_path root_paths
  |> fun checked_group -> checked_group.checked_files

let check_source_group = fun ?workspace ~summary_cache ?roots paths ->
  let root_paths =
    match roots with
    | Some roots -> roots
    | None -> paths
  in
  let config = typ_config_for_source_group ?workspace ~summary_cache root_paths in
  let (session, prepared_sources) = create_typ_session config paths in
  checked_files_for_session config session (prepared_sources_by_path prepared_sources) root_paths

let package_root_for_target = fun (workspace: Workspace.t) path ->
  workspace.packages |> List.filter Package.is_workspace_member |> List.sort
    (fun (left: Package.t) (right: Package.t) ->
      Int.compare
        (String.length (Path.to_string right.path))
        (String.length (Path.to_string left.path))) |> List.find_opt
    (fun (pkg: Package.t) ->
      Path.equal path pkg.path || match Path.strip_prefix path ~prefix:pkg.path with
      | Ok _ -> true
      | Error _ -> false) |> Option.map (fun (pkg: Package.t) -> pkg.path)

let grouped_targets_for_session = fun ?workspace target_files ->
  let group_key_for path =
    match workspace with
    | Some workspace -> (
        match package_root_for_target workspace path with
        | Some package_root -> Path.to_string package_root
        | None -> Path.to_string (Path.dirname path)
      )
    | None -> "__riot-check-session__"
  in
  target_files |> List.fold_left
    (fun groups path ->
      let key = group_key_for path in
      let existing =
        match List.assoc_opt key groups with
        | Some existing -> existing
        | None -> []
      in
      (key, existing @ [ path ]) :: List.remove_assoc key groups)
    [] |> List.rev

let session_source_paths_for_group = fun ?workspace ~scan_mode group_targets ->
  match (scan_mode, workspace, group_targets) with
  | (false, Some workspace, path :: _) -> (
      match Workspace.find_package_for_path workspace ~path with
      | Some pkg -> (
          match package_typ_source_files ~include_dev:true pkg with
          | [] -> group_targets
          | session_paths -> dedupe_paths (group_targets @ session_paths)
        )
      | None -> group_targets
    )
  | _ -> group_targets

let grouped_root_targets_for_session = fun group_targets ->
  group_targets |> List.fold_left
    (fun groups path ->
      let module_name = module_name_for_path path in
      let existing =
        match List.assoc_opt module_name groups with
        | Some existing -> existing
        | None -> []
      in
      (module_name, existing @ [ path ]) :: List.remove_assoc module_name groups)
    [] |> List.rev

let ordered_grouped_root_targets_for_session = fun prepared_by_path group_targets ->
  let grouped = grouped_root_targets_for_session group_targets in
  let ordered_unique_module_names =
    let rec loop seen ordered = function
      | [] -> List.rev ordered
      | module_name :: rest ->
          if List.mem module_name seen then
            loop seen ordered rest
          else
            loop (module_name :: seen) (module_name :: ordered) rest
    in
    loop [] []
  in
  let ordered_module_names =
    group_targets
    |> List.filter_map
      (fun path ->
        List.assoc_opt (path_key path) prepared_by_path)
    |> List.filter_map readable_typ_source_of_prepared
    |> ordered_readable_typ_sources
    |> List.map (fun source -> module_name_for_path source.path)
    |> ordered_unique_module_names
  in
  let ordered_groups =
    ordered_module_names
    |> List.filter_map
      (fun module_name ->
        grouped |> List.find_opt
          (fun (name, _) ->
            String.equal name module_name))
  in
  let remaining_groups = grouped
  |> List.filter (fun (module_name, _) -> not (List.mem module_name ordered_module_names)) in
  ordered_groups @ remaining_groups

let check_target_files = fun ?workspace ~scan_mode ?on_result target_files ->
  if not scan_mode && Option.is_none workspace then
    target_files |> List.map
      (fun path ->
        let checked_file = check_source_file path in
        let () =
          match on_result with
          | Some callback -> callback checked_file
          | None -> ()
        in
        checked_file)
  else
    let summary_cache = ref [] in
    let checked_by_path = ref [] in
    let emit checked_file =
      checked_by_path := (path_key (checked_file_path checked_file), checked_file) :: !checked_by_path;
      match on_result with
      | Some callback -> callback checked_file
      | None -> ()
    in
    let () =
      grouped_targets_for_session ?workspace target_files
      |> List.iter
        (fun (_, group_targets) ->
          let session_paths = session_source_paths_for_group ?workspace ~scan_mode group_targets in
          let config = typ_config_for_source_group ?workspace ~summary_cache group_targets in
          let (session, prepared_sources) = create_typ_session config session_paths in
          let prepared_by_path = prepared_sources_by_path prepared_sources in
          let _ =
            ordered_grouped_root_targets_for_session prepared_by_path group_targets
            |> List.fold_left
              (fun (session, config) (_module_name, root_targets) ->
                let checked_group = checked_group_for_session config session prepared_by_path root_targets in
                checked_group.checked_files |> List.iter emit;
                let loaded_modules = merge_loaded_module_typings
                  checked_group.module_typings
                  config.Typ.Config.loaded_modules in
                let config = Typ.Config.with_loaded_modules config ~loaded_modules in
                let session = Typ.Session.with_config session ~config in
                (session, config))
              (session, config)
          in
          ())
    in
    target_files
    |> List.map
      (fun path ->
        !checked_by_path
        |> List.assoc_opt (path_key path)
        |> Option.expect ~msg:(("missing checked result for " ^ Path.to_string path)))

let start_to_json = fun ~workspace_root ~target_count ->
  let workspace_root_json =
    match workspace_root with
    | Some root -> Data.Json.String (Path.to_string root)
    | None -> Data.Json.Null
  in
  Data.Json.Object [
    ("type", Data.Json.String "check_start");
    ("workspace_root", workspace_root_json);
    ("target_count", Data.Json.Int target_count);
  ]

let checked_summary_to_json = fun (summary: checked_summary) ->
  Data.Json.Object [
    ("files", Data.Json.Int summary.checked_files);
    ("read_failures", Data.Json.Int summary.read_failures);
    ("diagnostics", Data.Json.Int summary.diagnostics);
    ("warnings", Data.Json.Int summary.warnings);
  ]

type check_run_summary = {
  target_count: int;
  summary: checked_summary;
}

let check_all = fun ?workspace ?package_filter ?on_start ?on_result paths ->
  match resolve_targets ?workspace ?package_filter paths with
  | Error err -> Error err
  | Ok target_files ->
      match target_files with
      | [] -> Error NoTargets
      | _ ->
          let summary = ref empty_checked_summary in
          let scan_mode = List.is_empty paths in
          let _ =
            match on_start with
            | Some callback -> callback (List.length target_files)
            | None -> ()
          in
          let _checked_files =
            check_target_files ?workspace ~scan_mode
              ~on_result:(fun checked_file ->
                summary := update_checked_summary !summary checked_file;
                match on_result with
                | Some callback -> callback checked_file
                | None -> ())
              target_files
          in
          Ok { target_count = List.length target_files; summary = !summary }

let print_checked_file = fun ~stdout ~stderr ~workspace_root checked_file ->
  match checked_file with
  | Unreadable { path; reason } -> stderr
    (relative_or_absolute ~workspace_root path ^ ": " ^ reason ^ "\n")
  | Typed { path; report; diagnostics } ->
      if List.is_empty diagnostics then
        ()
      else
        (
          let source_layout = make_source_layout report.source in
          let path_text = relative_or_absolute ~workspace_root path in
          List.iter
            (fun diagnostic ->
              stdout
                (format_diagnostic ~path_text ~source_layout ~source_text:report.source diagnostic))
            diagnostics
        )

let checked_file_to_json = fun ~workspace_root checked_file ->
  match checked_file with
  | Typed { path; report } ->
      let diagnostics = diagnostics_of_report report in
      let summary = Data.Json.Object [
        ("parse", Data.Json.Int (List.length report.parse_diagnostics));
        ("lowering", Data.Json.Int (List.length report.lowering_diagnostics));
        ("typing", Data.Json.Int (List.length report.typing_diagnostics));
        ("total", Data.Json.Int (List.length diagnostics));
      ] in
      Data.Json.Object [
        ("path", Data.Json.String (relative_or_absolute ~workspace_root path));
        ("ok", Data.Json.Bool (not (has_errors diagnostics)));
        ("summary", summary);
      ]
  | Unreadable { path; reason } -> read_report_to_json ~workspace_root ~path reason

let checked_file_diagnostics_to_json = fun ~workspace_root path diagnostics ->
  let path_text = relative_or_absolute ~workspace_root path in
  let index = ref 0 in
  diagnostics |> List.map
    (fun diagnostic ->
      let json = Data.Json.Object [
        ("type", Data.Json.String "check_diagnostic");
        ("path", Data.Json.String path_text);
        ("diagnostic_index", Data.Json.Int !index);
        ("diagnostic", diagnostic_to_json diagnostic);
      ] in
      index := !index + 1;
      json)

let checked_file_events_to_json = fun ~workspace_root checked_file ->
  match checked_file with
  | Unreadable _ -> [
    Data.Json.Object [
      ("type", Data.Json.String "check_file");
      ("result", checked_file_to_json ~workspace_root checked_file);
    ]
  ]
  | Typed { path; report; diagnostics } ->
      let file_json = Data.Json.Object [
        ("type", Data.Json.String "check_file");
        ("result", checked_file_to_json ~workspace_root (Typed { path; report; diagnostics }));
      ] in
      file_json :: checked_file_diagnostics_to_json ~workspace_root path diagnostics

let print_json_lines = fun ~stdout events ->
  events |> List.iter (fun json -> stdout (Data.Json.to_string json ^ "\n"))

let print_checked_file_json = fun ~stdout ~workspace_root checked_file ->
  checked_file |> checked_file_events_to_json ~workspace_root |> print_json_lines ~stdout

let print_checked_files_summary = fun ~stdout checked_summary quiet ->
  if quiet then
    ()
  else if checked_summary.diagnostics = 0 && checked_summary.read_failures = 0 then
    ()
  else if checked_summary.has_error then
    ()
  else if checked_summary.checked_files = 1 then
    stdout "Checked 1 file: ok\n"
  else
    stdout ("Checked " ^ Int.to_string checked_summary.checked_files ^ " files: ok\n")

let print_json_summary = fun ~stdout (summary: checked_summary) ->
  Data.Json.Object [
    ("type", Data.Json.String "check_summary");
    ("ok", Data.Json.Bool (not summary.has_error));
    ("summary", checked_summary_to_json summary);
  ]
  |> Data.Json.to_string
  |> fun line -> stdout (line ^ "\n")

let run_explain = fun ?(stdout = default_stdout) ?(stderr = default_stderr) ~json diagnostic_id ->
  match Typ.Explanations.explain diagnostic_id with
  | None -> fail ~stderr (UnknownDiagnosticId { diagnostic_id })
  | Some explanation ->
      if json then
        stdout (Data.Json.to_string (Typ.Explanations.to_json explanation) ^ "\n")
      else
        stdout (Typ.Explanations.format explanation ^ "\n");
        Ok ()

let run = fun ?workspace ?(stdout = default_stdout) ?(stderr = default_stderr) matches ->
  match action_of_matches matches with
  | Error err ->
      fail ~stderr err
  | Ok (Explain { diagnostic_id; json }) ->
      run_explain ~stdout ~stderr ~json diagnostic_id
  | Ok (Check { paths; package_filter; json; quiet }) -> (
      let workspace =
        match workspace with
        | None -> Ok None
        | Some workspace -> prepare_workspace workspace |> Result.map Option.some
      in
      match workspace with
      | Error err -> fail ~stderr err
      | Ok workspace ->
          let workspace_root = workspace_scope workspace
          |> Option.map (fun scope -> scope.workspace_root) in
          let on_start target_count =
            if json then
              print_json_lines ~stdout [ start_to_json ~workspace_root ~target_count ]
          in
          let on_result checked_file =
            if json then
              print_checked_file_json ~stdout ~workspace_root checked_file
            else
              print_checked_file ~stdout ~stderr ~workspace_root checked_file
          in
          match check_all ?workspace ?package_filter ~on_start ~on_result paths with
          | Error err ->
              fail ~stderr
                (
                  match err with
                  | NoTargets -> NoTargets
                  | PackageFilterRequiresWorkspace _ as err -> err
                  | UnknownPackage _ as err -> err
                  | _ -> err
                )
          | Ok { summary; _ } ->
              if json then
                print_json_summary ~stdout summary
              else
                print_checked_files_summary ~stdout summary quiet;
                if summary.has_error then
                  Error (Failure "typecheck failed")
                else
                  Ok ()
    )
