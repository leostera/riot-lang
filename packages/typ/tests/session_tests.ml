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
  Query.diagnostics snapshot source_id |> List.exists
    (
      function
      | Query.Typing (Diagnostic.UnboundName _) -> true
      | _ -> false
    )

let diagnostic_strings = fun snapshot source_id ->
  Query.diagnostics snapshot source_id |> List.map
    (
      function
      | Query.Parse diagnostic -> Syn.Diagnostic.to_string diagnostic
      | Query.Lowering diagnostic
      | Query.Typing diagnostic -> Diagnostic.to_string diagnostic
    )

let trace_debug = fun snapshot source_id ->
  match Query.analysis_of_source snapshot source_id with
  | None -> []
  | Some analysis ->
      let item_lines = analysis.item_traces
      |> List.map
        (fun (trace: Check_result.item_trace) ->
          "item "
          ^ ItemId.to_string trace.item_id
          ^ " -> ["
          ^ String.concat ", " (List.map fst trace.exports_after)
          ^ "]") in
      let expr_lines = analysis.expr_traces
      |> List.map
        (fun (trace: Check_result.expr_trace) ->
          "expr "
          ^ ExprId.to_string trace.expr_id
          ^ " -> ["
          ^ String.concat ", " (List.map fst trace.env_before)
          ^ "]") in
      item_lines @ expr_lines

let persisted_summary_jsons = fun snapshot ->
  Snapshot.persisted_summaries snapshot |> List.map PersistedSummary.Json.to_json

let module_summary_jsons = fun snapshot ->
  Snapshot.module_summaries snapshot |> List.map ModuleSummary.Json.to_json

let prepare_snapshot_or_error = fun session ~roots ->
  match Session.prepare_snapshot session ~roots with
  | Ok snapshot -> Ok snapshot
  | Error missing -> Error ("unexpected missing requirements: "
  ^ Data.Json.to_string (MissingRequirements.to_json missing))

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

let test_snapshot_collects_persisted_summaries = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "alpha.ml")
    ~text:"let id x = x\n" in
  let (session, _) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "beta.ml")
    ~text:"let broken = missing\n" in
  let snapshot = Session.snapshot session in
  let summaries = persisted_summary_jsons snapshot in
  let tags =
    summaries
    |> List.filter_map
      (
        function
        | Data.Json.Object fields -> (
            match List.assoc_opt "export_result" fields with
            | Some (Data.Json.Object export_fields) -> (
                match List.assoc_opt "tag" export_fields with
                | Some (Data.Json.String tag) -> Some tag
                | _ -> None
              )
            | _ -> None
          )
        | _ -> None
      )
  in
  let () = Test.assert_equal ~expected:[ "trusted_export"; "errored_export" ] ~actual:tags in
  Ok ()

let test_source_input_hash_ignores_source_id_and_revision = fun _ctx ->
  let source_a = Source.make
    ~source_id:(SourceId.of_int 0)
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~revision:1
    ~text:"let id x = x\n" in
  let source_b = Source.make
    ~source_id:(SourceId.of_int 99)
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~revision:42
    ~text:"let id x = x\n" in
  let source_c = Source.make
    ~source_id:(SourceId.of_int 99)
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~revision:43
    ~text:"let id x = (x, x)\n" in
  let hash_a = Source.input_hash source_a |> Crypto.Digest.hex in
  let hash_b = Source.input_hash source_b |> Crypto.Digest.hex in
  let hash_c = Source.input_hash source_c |> Crypto.Digest.hex in
  let () = Test.assert_equal ~expected:hash_a ~actual:hash_b in
  if String.equal hash_a hash_c then
    Error "expected source input hash to change when source text changes"
  else
    Ok ()

let test_snapshot_uses_loaded_module_summaries = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, colors_source_id) = Session.create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"module RGB = struct let blend x y = x end\nlet to_string value = value\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_colors =
    match Query.module_summary_of seed_snapshot colors_source_id with
    | Some summary -> summary
    | None -> panic "expected seed module summary"
  in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ loaded_colors ] in
  let session = Session.empty ~config in
  let (session, demo_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "blend_demo.ml")
    ~text:"open Colors\nlet midpoint = RGB.blend 1 2\nlet label = to_string \"ok\"\n" in
  let snapshot = Session.snapshot session in
  let demo_has_unbound_name = has_unbound_name snapshot demo_source_id in
  let midpoint_type = inferred_type_at snapshot demo_source_id 34 in
  let label_type = inferred_type_at snapshot demo_source_id 58 in
  let summary_modules =
    module_summary_jsons snapshot
    |> List.filter_map
      (
        function
        | Data.Json.Object fields -> (
            match List.assoc_opt "module_name" fields with
            | Some (Data.Json.String module_name) -> Some module_name
            | _ -> None
          )
        | _ -> None
      )
  in
  let () = Test.assert_equal ~expected:[ "Blend_demo" ] ~actual:summary_modules in
  if demo_has_unbound_name then
    Error (String.concat "\n" (diagnostic_strings snapshot demo_source_id))
  else
    let () = Test.assert_equal ~expected:(Some "int -> int -> int") ~actual:midpoint_type in
    let () = Test.assert_equal ~expected:(Some "string -> string") ~actual:label_type in
    Ok ()

let test_prepare_snapshot_is_rooted = fun _ctx ->
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
  match prepare_snapshot_or_error session ~roots:[ demo_source_id ] with
  | Error message -> Error message
  | Ok snapshot ->
      let root_ids = Snapshot.roots snapshot in
      let demo_has_unbound_name = has_unbound_name snapshot demo_source_id in
      let demo_analysis_exists = Option.is_some (Query.analysis_of_source snapshot demo_source_id) in
      let colors_analysis_exists = Option.is_some
        (Query.analysis_of_source snapshot colors_source_id) in
      let midpoint_type = inferred_type_at snapshot demo_source_id 34 in
      let label_type = inferred_type_at snapshot demo_source_id 58 in
      let () = Test.assert_equal ~expected:[ demo_source_id ] ~actual:root_ids in
      let () = Test.assert_equal ~expected:true ~actual:demo_analysis_exists in
      let () = Test.assert_equal ~expected:false ~actual:colors_analysis_exists in
      if demo_has_unbound_name then
        Error (String.concat "\n" (diagnostic_strings snapshot demo_source_id))
      else
        let () = Test.assert_equal ~expected:(Some "int -> int -> int") ~actual:midpoint_type in
        let () = Test.assert_equal ~expected:(Some "string -> string") ~actual:label_type in
        Ok ()

let test_prepare_snapshot_reports_missing_roots = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let missing_root = SourceId.of_int 99 in
  match Session.prepare_snapshot session ~roots:[ missing_root ] with
  | Ok _ -> Error "expected rooted snapshot preparation to report the missing root"
  | Error missing ->
      let actual = MissingRequirements.to_json missing in
      let expected = Data.Json.Array [
        Data.Json.Object [
          ("tag", Data.Json.String "missing_root_source");
          ("source_id", Data.Json.Int 99);
        ]
      ] in
      let () = Test.assert_equal ~expected ~actual in
      Ok ()

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests = [
        Test.case "source id stays stable across updates" test_source_id_stays_stable_across_updates;
        Test.case "snapshots remain immutable after updates" test_snapshots_remain_immutable_after_updates;
        Test.case "type_at uses smallest indexed expression" test_type_at_uses_smallest_indexed_expression;
        Test.case "snapshot exposes implicit file modules" test_snapshot_exposes_implicit_file_modules;
        Test.case "snapshot collects persisted summaries" test_snapshot_collects_persisted_summaries;
        Test.case "source input hash ignores source id and revision" test_source_input_hash_ignores_source_id_and_revision;
        Test.case "snapshot uses loaded module summaries" test_snapshot_uses_loaded_module_summaries;
        Test.case "prepare_snapshot is rooted" test_prepare_snapshot_is_rooted;
        Test.case "prepare_snapshot reports missing roots" test_prepare_snapshot_reports_missing_roots;
      ] in
      Test.Cli.main ~name:"typ:session" ~tests ~args)
    ~args:Env.args
    ()
