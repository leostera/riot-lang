open Std
open Analysis
open Diagnostics
open Infer
open Lower
open Model
module Typ_diagnostic = Diagnostic
open Syn

type t = {
  source: Source.t;
  parse_diagnostics: Syn.Diagnostic.t list;
  cst: Syn.Cst.source_file;
  semantic_tree: SemanticTree.file option;
  lowering_diagnostics: Typ_diagnostic.t list;
  typing_diagnostics: Typ_diagnostic.t list;
  ambient_type_decls: FileSummary.type_decl list;
  file_summary: FileSummary.t;
  export_bindings: Check_result.binding_ref list;
  type_index: TypeIndex.t;
  item_traces: Check_result.item_trace list;
  expr_traces: Check_result.expr_trace list;
}

let exports = fun analysis -> FileSummary.exports analysis.file_summary

let definition_site_of_origin_id = fun analysis origin_id ->
  match analysis.semantic_tree with
  | None -> None
  | Some semantic_tree -> OriginMap.find semantic_tree.origin_map origin_id
  |> Option.map
    (fun (origin: OriginMap.origin) ->
      ({ origin = analysis.source.origin; span = origin.span }: ModuleTypings.definition_site))

let declared_value_origin_id = fun (semantic_tree: SemanticTree.file) ~scope_path ~name ->
  ItemTree.items semantic_tree.item_tree |> List.find_map
    (
      function
      | ItemTree.DeclaredValue item when String.equal name item.value_name
      && IdentPath.equal scope_path item.scope_path -> Some item.origin_id
      | ItemTree.ExtensionConstructor item when String.equal name item.constructor_name
      && IdentPath.equal scope_path item.scope_path -> Some item.origin_id
      | _ -> None
    )

let exception_origin_id = fun (semantic_tree: SemanticTree.file) ~scope_path ~name ->
  ItemTree.items semantic_tree.item_tree |> List.find_map
    (
      function
      | ItemTree.Exception item when String.equal name item.exception_name
      && IdentPath.equal scope_path item.scope_path -> Some item.origin_id
      | _ -> None
    )

let alias_target_path = fun ~alias_name ~module_path path ->
  let alias_prefix = IdentPath.of_name alias_name in
  let suffix = IdentPath.strip_prefix ~prefix:alias_prefix path |> Option.unwrap_or ~default:path in
  IdentPath.append_path module_path suffix

let definition_target_of_binding_ref = fun analysis (binding_ref: Check_result.binding_ref) ->
  match binding_ref.provenance with
  | Check_result.Lowered_pattern pat_id -> (
      match analysis.semantic_tree with
      | None -> None
      | Some semantic_tree -> Option.and_then
        (SemanticTree.find_pattern semantic_tree pat_id)
        (fun pattern -> definition_site_of_origin_id analysis pattern.origin_id)
      |> Option.map (fun site -> ModuleTypings.Site site)
    )
  | Check_result.Declared_value { name; scope_path } -> (
      match analysis.semantic_tree with
      | None -> None
      | Some semantic_tree -> Option.and_then
        (declared_value_origin_id semantic_tree ~scope_path ~name)
        (definition_site_of_origin_id analysis)
      |> Option.map (fun site -> ModuleTypings.Site site)
    )
  | Check_result.Exception { name; scope_path } -> (
      match analysis.semantic_tree with
      | None -> None
      | Some semantic_tree -> Option.and_then
        (exception_origin_id semantic_tree ~scope_path ~name)
        (definition_site_of_origin_id analysis)
      |> Option.map (fun site -> ModuleTypings.Site site)
    )
  | Check_result.Ambient ->
      if IdentPath.is_bare binding_ref.path then
        None
      else
        Some (ModuleTypings.Export binding_ref.path)
  | Check_result.Included { module_path } ->
      Some (ModuleTypings.Export (IdentPath.append_path module_path binding_ref.path))
  | Check_result.Module_alias { alias_name; module_path } ->
      Some (ModuleTypings.Export (alias_target_path ~alias_name ~module_path binding_ref.path))
  | Check_result.Prelude
  | Check_result.Type_constructor _ ->
      None

let export_definitions = fun analysis ->
  analysis.export_bindings
  |> List.filter_map
    (fun (binding_ref: Check_result.binding_ref) ->
      definition_target_of_binding_ref analysis binding_ref
      |> Option.map
        (fun target ->
          (
            { export_name = IdentPath.to_string binding_ref.path; target }: ModuleTypings.value_definition
          )))

let analyze = fun ~config (source: Source.t) ->
  let parsed = source.parse_result in
  let cst = source.cst in
  let semantic_tree = Lower.lower_source_file ~source cst in
  let inferred = Infer.infer_file ~config ~source semantic_tree in
  let type_index =
    if config.capture_traces then
      let traced_exprs = inferred.expr_traces
      |> List.map
        (fun (trace: Check_result.expr_trace) ->
          {
            TypeIndex.expr_id = trace.expr_id;
            origin_id = trace.origin_id;
            inferred_type = trace.inferred_type
          }) in
      TypeIndex.of_traced_exprs ~origin_map:semantic_tree.origin_map traced_exprs
    else
      TypeIndex.empty
  in
  let diagnostics = semantic_tree.diagnostics @ inferred.diagnostics in
  let file_summary =
    if diagnostics = [] then
      FileSummary.trusted ~source_id:source.source_id ~type_decls:inferred.type_decls inferred.exports
    else
      FileSummary.errored ~source_id:source.source_id ~type_decls:inferred.type_decls inferred.exports
  in
  {
    source;
    parse_diagnostics = parsed.Parser.diagnostics;
    cst;
    semantic_tree = Some semantic_tree;
    lowering_diagnostics = semantic_tree.diagnostics;
    typing_diagnostics = inferred.diagnostics;
    ambient_type_decls = config.ambient_type_decls;
    file_summary;
    export_bindings = inferred.export_bindings;
    type_index;
    item_traces = inferred.item_traces;
    expr_traces = inferred.expr_traces;
  }
