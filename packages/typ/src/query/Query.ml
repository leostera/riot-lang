open Std
open Model

type diagnostic =
  | Parse of Syn.Diagnostic.t
  | Lowering of Diagnostic.t
  | Typing of Diagnostic.t

type definition = ModuleTypings.definition_site

let analysis_of_source = Session.Snapshot.find_analysis

let diagnostics = fun snapshot source_id ->
  match analysis_of_source snapshot source_id with
  | None -> []
  | Some analysis -> (analysis.parse_diagnostics |> List.map (fun diagnostic -> Parse diagnostic))
  @ (analysis.lowering_diagnostics |> List.map (fun diagnostic -> Lowering diagnostic))
  @ (analysis.typing_diagnostics |> List.map (fun diagnostic -> Typing diagnostic))

let file_summary_of = fun snapshot source_id ->
  match analysis_of_source snapshot source_id with
  | Some analysis -> Some analysis.file_summary
  | None -> None

let module_typings_of = Session.Snapshot.find_module_typings

let export_of = fun snapshot source_id ->
  file_summary_of snapshot source_id |> Option.map (fun summary -> summary.FileSummary.export_result)

let semantic_tree_of_source = fun snapshot source_id ->
  match analysis_of_source snapshot source_id with
  | Some analysis -> analysis.semantic_tree
  | None -> None

let source_file_of_source = fun snapshot source_id ->
  match analysis_of_source snapshot source_id with
  | Some analysis -> Some analysis.cst
  | None -> None

let type_at = fun snapshot source_id position ->
  match analysis_of_source snapshot source_id with
  | None -> None
  | Some analysis ->
      Analysis.TypeIndex.find_at analysis.type_index position |> function
      | Some entry -> Some entry.inferred_type
      | None -> None

let definition_target_of_position = fun (analysis: Session.SourceAnalysis.t) position ->
  Option.and_then
    (Analysis.TypeIndex.find_at analysis.type_index position)
    (fun entry ->
      analysis.expr_traces |> List.find_map
        (fun (trace: Analysis.Check_result.expr_trace) ->
          if ExprId.equal trace.expr_id entry.expr_id then
            trace.resolved_binding
          else
            None))

let resolve_export_path =
  let rec loop snapshot visited path =
    if List.exists (IdentPath.equal path) visited then
      None
    else
      match IdentPath.uncons path with
      | Some (module_name, export_path) when not (IdentPath.is_empty export_path) -> (
          match Session.Snapshot.find_module_typings_by_name snapshot module_name with
          | Some typings -> (
              match ModuleTypings.find_value_definition typings ~export_name:(IdentPath.to_string export_path) with
              | Some (ModuleTypings.Site definition) -> Some definition
              | Some (ModuleTypings.Export redirected) -> loop snapshot (path :: visited) redirected
              | None -> None
            )
          | None -> None
        )
      | _ -> None
  in
  fun snapshot path -> loop snapshot [] path

let definition_at = fun snapshot source_id position ->
  match analysis_of_source snapshot source_id with
  | None -> None
  | Some analysis ->
      Option.and_then
        (definition_target_of_position analysis position)
        (fun binding_ref ->
          Option.and_then
            (Session.SourceAnalysis.definition_target_of_binding_ref analysis binding_ref)
            (
              function
              | ModuleTypings.Site definition -> Some definition
              | ModuleTypings.Export path -> resolve_export_path snapshot path
            ))
