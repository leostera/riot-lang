open Std

type action =
  | Explain of { diagnostic_id: string; json: bool }
  | Check of { path: Path.t; json: bool; quiet: bool }

type error =
  | MissingPath
  | ExplainAndPath of { path: Path.t }
  | ReadFailed of { path: Path.t; reason: string }
  | UnknownDiagnosticId of { diagnostic_id: string }

type diagnostic =
  | Parse of Syn.Diagnostic.t
  | Lowering of Typ.Diagnostic.t
  | Typing of Typ.Diagnostic.t

let command =
  let open ArgParser in
    let open Arg in command "check"
    |> about "Typecheck one OCaml file with Riot's prototype checker"
    |> args
      [
        flag "json" |> long "json" |> help "Emit machine-readable JSON output";
        flag "quiet" |> long "quiet" |> help "Suppress the success summary when no diagnostics are found";
        option "explain" |> long "explain" |> help "Explain a typ diagnostic id such as TYP2001";
        positional "path" |> required false |> help "OCaml file to typecheck";
      ]

let message = function
  | MissingPath -> "missing OCaml file path"
  | ExplainAndPath { path } -> "cannot use --explain together with a file path ("
  ^ Path.to_string path
  ^ ")"
  | ReadFailed { path; reason } -> "failed to read " ^ Path.to_string path ^ ": " ^ reason
  | UnknownDiagnosticId { diagnostic_id } -> "unknown typ diagnostic id: " ^ diagnostic_id

let fail = fun err ->
  eprintln ("\027[1;31mError\027[0m: " ^ message err);
  Error (Failure (message err))

let action_of_matches = fun matches ->
  let json = ArgParser.get_flag matches "json" in
  let quiet = ArgParser.get_flag matches "quiet" in
  let path = ArgParser.get_path matches "path" in
  match ArgParser.get_one matches "explain", path with
  | Some diagnostic_id, None -> Ok (Explain { diagnostic_id; json })
  | Some _, Some path -> Error (ExplainAndPath { path })
  | None, Some path -> Ok (Check { path; json; quiet })
  | None, None -> Error MissingPath

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

let span_to_json = fun (span: Syn.Ceibo.Span.t) ->
  Data.Json.Object [ ("start", Data.Json.Int span.start); ("end", Data.Json.Int span.end_); ]

let severity_string_of_diagnostic = function
  | Parse _ -> "error"
  | Lowering diagnostic
  | Typing diagnostic -> Typ.Diagnostic.severity diagnostic |> Typ.Diagnostic.severity_to_string

let code_of_diagnostic = function
  | Parse diagnostic -> Syn.Diagnostic.id diagnostic
  | Lowering diagnostic
  | Typing diagnostic -> Typ.Diagnostic.code diagnostic

let message_of_diagnostic = function
  | Parse diagnostic -> Syn.Diagnostic.main_message diagnostic
  | Lowering diagnostic
  | Typing diagnostic -> Typ.Diagnostic.message diagnostic

let span_of_diagnostic = function
  | Parse diagnostic -> diagnostic.Syn.Diagnostic.span
  | Lowering diagnostic
  | Typing diagnostic -> Typ.Diagnostic.primary_span diagnostic

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
    ("span", span_to_json (span_of_diagnostic diagnostic));
    ("data", data_of_diagnostic diagnostic);
  ]

let position_of_offset = fun text offset -> Std.Unicode.Utf16.position_of_offset text ~offset

let human_diagnostic_line = fun ~path_text ~source_text diagnostic ->
  let span = span_of_diagnostic diagnostic in
  let position = position_of_offset source_text span.start in
  let line = position.line + 1 in
  let column = position.character + 1 in
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

let report_to_json = fun ~path (report: Typ.Check_result.t) ->
  let diagnostics = diagnostics_of_report report in
  let total = List.length diagnostics in
  let parse_count = List.length report.parse_diagnostics in
  let lowering_count = List.length report.lowering_diagnostics in
  let typing_count = List.length report.typing_diagnostics in
  Data.Json.Object [
    ("path", Data.Json.String (Path.to_string path));
    ("ok", Data.Json.Bool (not (has_errors diagnostics)));
    (
      "summary",
      Data.Json.Object [
        ("parse", Data.Json.Int parse_count);
        ("lowering", Data.Json.Int lowering_count);
        ("typing", Data.Json.Int typing_count);
        ("total", Data.Json.Int total);
      ]
    );
    ("diagnostics", Data.Json.Array (List.map diagnostic_to_json diagnostics));
  ]

let run_explain = fun ~json diagnostic_id ->
  match Typ.Explanations.explain diagnostic_id with
  | None -> fail (UnknownDiagnosticId { diagnostic_id })
  | Some explanation ->
      if json then
        Typ.Explanations.to_json explanation |> Data.Json.to_string |> println
      else
        Typ.Explanations.format explanation |> println;
        Ok ()

let run_check = fun ~path ~json ~quiet ->
  match Fs.read path with
  | Error err -> fail (ReadFailed { path; reason = IO.error_message err })
  | Ok source ->
      let report = Typ.Batch.check_source ~filename:path source in
      let diagnostics = diagnostics_of_report report in
      let ok = not (has_errors diagnostics) in
      if json then
        report_to_json ~path report |> Data.Json.to_string |> println
      else
        (
          let path_text = Path.to_string path in
          match diagnostics with
          | [] ->
              if not quiet then
                println ("Checked " ^ path_text ^ ": ok")
          | diagnostics -> List.iter
            (fun diagnostic ->
              println (human_diagnostic_line ~path_text ~source_text:source diagnostic))
            diagnostics
        );
        if ok then
          Ok ()
        else
          Error (Failure "typecheck failed")

let run = fun matches ->
  match action_of_matches matches with
  | Error err -> fail err
  | Ok (Explain { diagnostic_id; json }) -> run_explain ~json diagnostic_id
  | Ok (Check { path; json; quiet }) -> run_check ~path ~json ~quiet
