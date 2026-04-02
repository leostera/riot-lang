open Std

type progress_phase =
  | Parsed of { parse_diagnostics: int }
  | CstBuilt
  | RuleStarted of { rule_id: string }
  | RuleFinished of { rule_id: string; diagnostics: int }

type progress_event = {
  timestamp_ms: int;
  phase: progress_phase;
}

type result = {
  tree: Rule.green_tree;
  diagnostics: Diagnostic.t list;
  parse_diagnostics: Syn.Diagnostic.t list;
}

let timestamp_ms = fun () ->
  Time.SystemTime.now () |> Time.SystemTime.nanos |> Int64.div 1_000_000L |> Int64.to_int

let emit_progress = fun on_progress phase ->
  match on_progress with
  | Some on_progress -> on_progress { timestamp_ms = timestamp_ms (); phase }
  | None -> ()

let trace_enabled = Env.var Env.Bool ~name:"RIOT_FIX_TRACE" |> Option.unwrap_or ~default:false

let trace = fun ?filename message ->
  if trace_enabled then
    let file_path =
      match filename with
      | Some filename -> Path.to_string filename
      | None -> "<stdin>"
    in
    eprintln ("[riot-fix] " ^ file_path ^ " " ^ message)

let parse ?filename source: Syn.Parser.parse_result =
  match filename with
  | Some filename -> Syn.parse ~filename source
  | None -> Syn.parse_implementation source

let lint_diagnostics = fun ~rules ?filename ?on_progress (parse_result: Syn.Parser.parse_result) ->
  emit_progress on_progress (Parsed { parse_diagnostics = List.length parse_result.diagnostics });
  trace
    ?filename
    ("parsed (" ^ Int.to_string (List.length parse_result.diagnostics) ^ " diagnostics)");
  if not (List.is_empty parse_result.diagnostics) then
    []
  else
    match Syn.build_cst parse_result with
    | Error _ -> []
    | Ok cst ->
        emit_progress on_progress CstBuilt;
        trace ?filename "built cst";
        let red_tree = Syn.Ceibo.Red.new_root parse_result.tree in
        let file_path =
          match filename with
          | Some filename -> Path.to_string filename
          | None -> "<stdin>"
        in
        let ctx = Rule.{ file_path; cst } in
        rules |> List.concat_map
          (fun rule ->
            let rule_id = Rule.id rule in
            emit_progress on_progress (RuleStarted { rule_id });
            trace ?filename ("rule start " ^ rule_id);
            let diagnostics = Rule.run rule ctx red_tree in
            emit_progress
              on_progress
              (RuleFinished { rule_id; diagnostics = List.length diagnostics });
            trace
              ?filename
              ("rule finish " ^ rule_id ^ " (" ^ Int.to_string (List.length diagnostics) ^ " diagnostics)");
            diagnostics)

let run = fun ~rules ?filename ?on_progress source ->
  let parse_result = parse ?filename source in
  {
    tree = parse_result.tree;
    diagnostics = lint_diagnostics ~rules ?filename ?on_progress parse_result;
    parse_diagnostics = parse_result.diagnostics
  }

let run_rule = fun ~rule ?filename ?on_progress source ->
  run ~rules:[ rule ] ?filename ?on_progress source

let has_parse_errors = fun result -> not (List.is_empty result.parse_diagnostics)

let has_errors = fun result ->
  List.exists (fun diag -> Diagnostic.severity diag = Diagnostic.Error) result.diagnostics

let safe_fixes = fun result ->
  List.filter_map Diagnostic.fix result.diagnostics

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
