open Std
open Model

let check_source = fun ~filename ~parse_result ~cst ->
  let config = TypConfig.default in
  let session = Session.empty ~config in
  let origin = Source.Path filename in
  let module_name = Source.infer_module_name origin in
  let implicit_opens = [] in
  let source_hash = Source.hash ~implicit_opens ~cst in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~module_name
    ~implicit_opens
    ~origin
    ~source_hash
    ~parse_result
    ~cst in
  let source = Source.make_prepared
    ~source_id
    ~kind:Source.File
    ~module_name
    ~implicit_opens
    ~origin
    ~revision:0
    ~source_hash
    ~parse_result
    ~cst in
  let fallback_analysis = Session.SourceAnalysis.analyze ~config source in
  let analysis =
    match Session.prepare_snapshot session ~roots:[ source_id ] with
    | Ok snapshot -> (
        match Query.analysis_of_source snapshot source_id with
        | Some analysis -> analysis
        | None -> fallback_analysis
      )
    | Error _ -> fallback_analysis
  in
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
    Analysis.Check_result.source_id;
    filename;
    parse_diagnostics = analysis.parse_diagnostics;
    item_tree;
    body_arena;
    origin_map;
    semantic_tree = analysis.semantic_tree;
    lowering_diagnostics = analysis.lowering_diagnostics;
    typing_diagnostics = analysis.typing_diagnostics;
    file_summary = analysis.file_summary;
    type_index = analysis.type_index;
    exports =
      Session.SourceAnalysis.exports analysis
      |> List.map (fun (name, scheme) -> (SurfacePath.to_string name, scheme));
    item_traces = analysis.item_traces;
    expr_traces = analysis.expr_traces;
  }
