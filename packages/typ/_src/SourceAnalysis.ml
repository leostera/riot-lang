open Std
open Analysis
open Diagnostics
open Infer
open Lower
open Model
module Typ_diagnostic = Diagnostic
open Syn

type completeness = FileSummary.completeness =
  | Complete
  | Partial

type t = {
  source: Source.t;
  parse_diagnostics: Syn.Diagnostic.t list;
  semantic_tree: SemanticTree.file option;
  lowering_diagnostics: Typ_diagnostic.t list;
  typing_diagnostics: Typ_diagnostic.t list;
  completeness: completeness;
  file_summary: FileSummary.t;
  value_definitions: ModuleTypings.value_definition list;
  type_index: TypeIndex.t;
  item_traces: Check_result.item_trace list;
  expr_traces: Check_result.expr_trace list;
}

let exports = fun analysis -> FileSummary.exports analysis.file_summary

let completeness_of_file_summary = fun summary -> FileSummary.completeness summary

let definition_site_of_origin_id = fun ~(source:Source.t) ~semantic_tree origin_id ->
  match semantic_tree with
  | None -> None
  | Some (semantic_tree: SemanticTree.file) -> OriginMap.find semantic_tree.origin_map origin_id
  |> Option.map
    (fun (origin: OriginMap.origin) ->
      ({ origin = source.origin; span = origin.span }: ModuleTypings.definition_site))

let declared_value_origin_id = fun (semantic_tree: SemanticTree.file) ~scope_path ~name ->
  ItemTree.items semantic_tree.item_tree |> List.find_map
    (
      function
      | ItemTree.DeclaredValue item when String.equal name item.value_name
      && SurfacePath.equal scope_path item.scope_path -> Some item.origin_id
      | ItemTree.ExtensionConstructor item when String.equal name item.constructor_name
      && SurfacePath.equal scope_path item.scope_path -> Some item.origin_id
      | _ -> None
    )

let exception_origin_id = fun (semantic_tree: SemanticTree.file) ~scope_path ~name ->
  ItemTree.items semantic_tree.item_tree |> List.find_map
    (
      function
      | ItemTree.Exception item when String.equal name item.exception_name
      && SurfacePath.equal scope_path item.scope_path -> Some item.origin_id
      | _ -> None
    )

let alias_target_path = fun ~alias_name ~module_path path ->
  let alias_prefix = SurfacePath.of_name alias_name in
  let suffix = SurfacePath.strip_prefix ~prefix:alias_prefix path |> Option.unwrap_or ~default:path in
  SurfacePath.append_path module_path suffix

let definition_target_of_binding_ref_in_tree = fun ~(source:Source.t) ~semantic_tree (
  binding_ref: Check_result.binding_ref
) ->
  match binding_ref.provenance with
  | Check_result.LoweredPattern pat_id -> (
      match semantic_tree with
      | None -> None
      | Some semantic_tree -> Option.and_then
        (SemanticTree.find_pattern semantic_tree pat_id)
        (fun pattern ->
          definition_site_of_origin_id ~source ~semantic_tree:(Some semantic_tree) pattern.origin_id)
      |> Option.map (fun site -> ModuleTypings.Site site)
    )
  | Check_result.DeclaredValue { name; scope_path } -> (
      match semantic_tree with
      | None -> None
      | Some semantic_tree -> Option.and_then
        (declared_value_origin_id semantic_tree ~scope_path ~name)
        (definition_site_of_origin_id ~source ~semantic_tree:(Some semantic_tree))
      |> Option.map (fun site -> ModuleTypings.Site site)
    )
  | Check_result.Exception { name; scope_path } -> (
      match semantic_tree with
      | None -> None
      | Some semantic_tree -> Option.and_then
        (exception_origin_id semantic_tree ~scope_path ~name)
        (definition_site_of_origin_id ~source ~semantic_tree:(Some semantic_tree))
      |> Option.map (fun site -> ModuleTypings.Site site)
    )
  | Check_result.Ambient ->
      if SurfacePath.is_bare binding_ref.surface_path then
        None
      else
        Some (ModuleTypings.Export binding_ref.surface_path)
  | Check_result.Included { module_path } ->
      Some (ModuleTypings.Export (SurfacePath.append_path module_path binding_ref.surface_path))
  | Check_result.ModuleAlias { alias_name; module_path } ->
      Some (ModuleTypings.Export (alias_target_path ~alias_name ~module_path binding_ref.surface_path))
  | Check_result.Prelude
  | Check_result.TypeConstructor _ ->
      None

let definition_target_of_binding_ref = fun analysis binding_ref ->
  definition_target_of_binding_ref_in_tree
    ~source:analysis.source
    ~semantic_tree:analysis.semantic_tree
    binding_ref

let export_definitions_of_bindings = fun ~(source:Source.t) ~semantic_tree export_bindings ->
  export_bindings
  |> List.filter_map
    (fun (binding_ref: Check_result.binding_ref) ->
      definition_target_of_binding_ref_in_tree ~source ~semantic_tree binding_ref
      |> Option.map
        (fun target ->
          ({ export_name = binding_ref.surface_path; target }: ModuleTypings.value_definition)))

let export_definitions = fun analysis -> analysis.value_definitions

let has_error_diagnostics = fun diagnostics ->
  List.exists (fun diagnostic -> Typ_diagnostic.severity diagnostic = Typ_diagnostic.Error) diagnostics

let analyze = fun ?(imported_world = ImportedWorld.empty ()) ~config (source: Source.t) ->
  let parsed = source.parse_result in
  let semantic_tree = Lower.lower_source_file ~source source.cst in
  let inferred = Infer.infer_file ~imported_world ~config ~source semantic_tree in
  let value_definitions = export_definitions_of_bindings
    ~source
    ~semantic_tree:(Some semantic_tree)
    inferred.export_bindings in
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
  let has_errors =
    not (Parser.(parsed.diagnostics) = [])
    || has_error_diagnostics semantic_tree.diagnostics
    || has_error_diagnostics inferred.diagnostics in
  let item_traces, expr_traces =
    if config.capture_traces then
      (inferred.item_traces, inferred.expr_traces)
    else
      ([], [])
  in
  let file_summary =
    if not has_errors then
      FileSummary.complete ~source_id:source.source_id ~type_decls:inferred.type_decls inferred.exports
    else
      FileSummary.partial
        ~source_id:source.source_id
        ~type_decls:inferred.type_decls
        ~exports:inferred.exports
        ()
  in
  {
    source;
    parse_diagnostics =
      Parser.(parsed.diagnostics);
    semantic_tree =
      if config.capture_traces then
        Some semantic_tree
      else
        None;
    lowering_diagnostics = semantic_tree.diagnostics;
    typing_diagnostics = inferred.diagnostics;
    completeness = completeness_of_file_summary file_summary;
    file_summary;
    value_definitions;
    type_index;
    item_traces;
    expr_traces;
  }
