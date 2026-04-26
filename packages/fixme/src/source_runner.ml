open Std

type progress_phase =
  | Parsed of { parse_diagnostics: int }
  | AstReady
  | RuleStarted of { rule_id: Rule_id.t }
  | RuleFinished of { rule_id: Rule_id.t; diagnostics: int }

type progress_event = {
  timestamp_ms: int;
  phase: progress_phase;
}

type result = {
  tree: Rule.syntax_tree;
  diagnostics: Diagnostic.t list;
  parse_diagnostics: Syn.Diagnostic.t list;
}

let timestamp_ms = fun () ->
  Time.SystemTime.now () |> Time.SystemTime.nanos |> Int64.div 1_000_000L |> Int64.to_int

let emit_progress = fun on_progress phase ->
  match on_progress with
  | Some on_progress -> on_progress { timestamp_ms = timestamp_ms (); phase }
  | None -> ()

let trace_enabled = Env.get Env.Bool ~var:"RIOT_FIX_TRACE" |> Option.unwrap_or ~default:false

let trace = fun ?filename message ->
  if trace_enabled then
    let file_path =
      match filename with
      | Some filename -> Path.to_string filename
      | None -> "<stdin>"
    in
    eprintln ("[riot-fix] " ^ file_path ^ " " ^ message)

let source_slice = fun source ->
  match IO.IoVec.IoSlice.from_string source with
  | Ok slice -> slice
  | Error error -> panic ("failed to create parser source slice: " ^ IO.IoSlice.error_message error)

let parse ?filename source: Syn.Parser.parse_result =
  let source = source_slice source in
  match filename with
  | Some filename -> Syn.parse ~filename source
  | None -> Syn.parse_implementation source

let lint_diagnostics = fun ~rules ?filename ?on_progress ~source (
  parse_result: Syn.Parser.parse_result
) ->
  let parse_diagnostic_count = Std.Collections.Vector.length parse_result.diagnostics in
  emit_progress on_progress (Parsed { parse_diagnostics = parse_diagnostic_count });
  trace ?filename ("parsed (" ^ Int.to_string parse_diagnostic_count ^ " diagnostics)");
  if not (Int.equal parse_diagnostic_count 0) then
    []
  else
    let source_file = Syn.Ast.SourceFile.make parse_result.tree in
    let root = Syn.Ast.root parse_result.tree in
    emit_progress on_progress AstReady;
    trace ?filename "ast ready";
    let source_text = source in
    let file_path =
      match filename with
      | Some filename -> Path.to_string filename
      | None -> "<stdin>"
    in
    let ctx = Rule.{ file_path; source = source_text; source_file } in
    rules |> List.map
      ~fn:(fun rule ->
        let rule_id = Rule.id rule in
        emit_progress on_progress (RuleStarted { rule_id });
        trace ?filename ("rule start " ^ Rule_id.to_string rule_id);
        let diagnostics = Rule.run rule ctx root in
        emit_progress on_progress (RuleFinished { rule_id; diagnostics = List.length diagnostics });
        trace
          ?filename
          ("rule finish "
          ^ Rule_id.to_string rule_id
          ^ " ("
          ^ Int.to_string (List.length diagnostics)
          ^ " diagnostics)");
        diagnostics) |> List.concat

let run = fun ~rules ?filename ?on_progress source ->
  let parse_result = parse ?filename source in
  {
    tree = parse_result.tree;
    diagnostics = lint_diagnostics ~rules ?filename ?on_progress ~source parse_result;
    parse_diagnostics = Std.Collections.Vector.to_array parse_result.diagnostics |> Array.to_list
  }

let run_rule = fun ~rule ?filename ?on_progress source ->
  run ~rules:[ rule ] ?filename ?on_progress source

let has_parse_errors = fun result -> not (List.is_empty result.parse_diagnostics)

let has_errors = fun result ->
  List.any result.diagnostics ~fn:(fun diag -> Diagnostic.severity diag = Diagnostic.Error)

let safe_fixes = fun result -> List.filter_map result.diagnostics ~fn:Diagnostic.fix

let can_apply_safe_fixes = fun result ->
  not (has_parse_errors result) && not (has_errors result) && not (List.is_empty (safe_fixes result))

let apply_safe_fixes = fun ~source result ->
  let fixes = safe_fixes result in
  if has_parse_errors result || has_errors result || List.is_empty fixes then
    Ok None
  else
    match Fix.apply_fixes ~source fixes with
    | Error _ as err -> err
    | Ok updated_source ->
        if String.equal updated_source source then
          Ok None
        else
          Ok (Some (updated_source, fixes))
