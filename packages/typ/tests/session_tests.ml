open Std
open Typ

let export_names = function
  | Some (FileSummary.TrustedExport { exports })
  | Some (FileSummary.ErroredExport { exports }) -> List.map fst exports
  | Some FileSummary.NoExport
  | None -> []

let inferred_type_at = fun snapshot source_id offset ->
  Query.type_at snapshot source_id (Position.make ~offset) |> function
  | Some ty -> Some (TypePrinter.type_to_string ty)
  | None -> None

let has_unbound_name = fun snapshot source_id ->
  Query.diagnostics snapshot source_id |> List.exists (
    function
    | Query.Typing (Diagnostic.UnboundName _) -> true
    | _ -> false)

let diagnostic_strings = fun snapshot source_id ->
  Query.diagnostics snapshot source_id |> List.map (
    function
    | Query.Parse diagnostic -> Syn.Diagnostic.to_string diagnostic
    | Query.Lowering diagnostic
    | Query.Typing diagnostic -> Diagnostic.to_string diagnostic)

let trace_debug = fun snapshot source_id ->
  match Query.analysis_of_source snapshot source_id with
  | None -> []
  | Some analysis ->
      let item_lines =
        analysis.item_traces
        |> List.map
          (fun (trace: Check_result.item_trace) ->
            "item "
            ^ ItemId.to_string trace.item_id
            ^ " -> ["
            ^ String.concat ", " (List.map fst trace.exports_after)
            ^ "]")
      in
      let expr_lines =
        analysis.expr_traces
        |> List.map
          (fun (trace: Check_result.expr_trace) ->
            "expr "
            ^ ExprId.to_string trace.expr_id
            ^ " -> ["
            ^ String.concat ", " (List.map fst trace.env_before)
            ^ "]")
      in
      item_lines @ expr_lines

let test_source_id_stays_stable_across_updates = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "stable.ml")
    ~text:"let x = 1" in
  let snapshot_before = Session.snapshot session in
  let session = Session.update_source_text session source_id ~text:"let y = 2" in
  let snapshot_after = Session.snapshot session in
  let before_names = export_names (Query.export_of snapshot_before source_id) in
  let after_names = export_names (Query.export_of snapshot_after source_id) in
  let () = Test.assert_equal ~expected:[ "x" ] ~actual:before_names in
  let () = Test.assert_equal ~expected:[ "y" ] ~actual:after_names in
  Ok ()

let test_snapshots_remain_immutable_after_updates = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "immutable.ml")
    ~text:"let x = 1" in
  let snapshot_before = Session.snapshot session in
  let session = Session.update_source_text session source_id ~text:"let x = true" in
  let snapshot_after = Session.snapshot session in
  let before_type = inferred_type_at snapshot_before source_id 8 in
  let after_type = inferred_type_at snapshot_after source_id 8 in
  let () = Test.assert_equal ~expected:(Some "int") ~actual:before_type in
  let () = Test.assert_equal ~expected:(Some "bool") ~actual:after_type in
  Ok ()

let test_type_at_uses_smallest_indexed_expression = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = "let id x = x\nlet answer = id 42\n" in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "type_at.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let callee_type = inferred_type_at snapshot source_id 26 in
  let argument_type = inferred_type_at snapshot source_id 29 in
  let () = Test.assert_equal ~expected:(Some "int -> int") ~actual:callee_type in
  let () = Test.assert_equal ~expected:(Some "int") ~actual:argument_type in
  Ok ()

let test_snapshot_exposes_implicit_file_modules = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, colors_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"module RGB = struct let blend x y = x end\nlet to_string value = value\n" in
  let (session, demo_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "blend_demo.ml")
    ~text:"open Colors\nlet midpoint = RGB.blend 1 2\nlet label = to_string \"ok\"\n" in
  let snapshot = Session.snapshot session in
  let demo_has_unbound_name = has_unbound_name snapshot demo_source_id in
  let demo_diagnostics = diagnostic_strings snapshot demo_source_id in
  let midpoint_type = inferred_type_at snapshot demo_source_id 34 in
  let label_type = inferred_type_at snapshot demo_source_id 58 in
  let color_exports = export_names (Query.export_of snapshot colors_source_id) in
  let () = Test.assert_equal ~expected:[ "RGB.blend"; "to_string" ] ~actual:color_exports in
  if demo_has_unbound_name then
    Error (String.concat "\n" (demo_diagnostics @ trace_debug snapshot demo_source_id))
  else
    let () = Test.assert_equal ~expected:(Some "int -> int -> int") ~actual:midpoint_type in
    let () = Test.assert_equal ~expected:(Some "string -> string") ~actual:label_type in
    Ok ()

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests = [
        Test.case "source id stays stable across updates" test_source_id_stays_stable_across_updates;
        Test.case "snapshots remain immutable after updates" test_snapshots_remain_immutable_after_updates;
        Test.case "type_at uses smallest indexed expression" test_type_at_uses_smallest_indexed_expression;
        Test.case "snapshot exposes implicit file modules" test_snapshot_exposes_implicit_file_modules;
      ] in
      Test.Cli.main ~name:"typ:session" ~tests ~args)
    ~args:Env.args
    ()
