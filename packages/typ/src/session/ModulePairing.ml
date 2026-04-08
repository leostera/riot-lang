open Std
open Diagnostics
open Model

type t = {
  module_typings: ModuleTypings.t;
  analyses_by_source: (SourceId.t * SourceAnalysis.t) list;
  signature_mismatches: Diagnostic.signature_mismatch list;
}

type source_kind =
  | Interface
  | Implementation
  | Other

let source_kind = fun (source: Source.t) ->
  match source.cst with
  | Syn.Cst.Interface _ -> Interface
  | Syn.Cst.Implementation _ -> Implementation

let prefer_source = fun current candidate ->
  match (source_kind current, source_kind candidate) with
  | (Interface, Interface)
  | (Implementation, Implementation)
  | (Other, Other) -> false
  | _, Interface -> true
  | Interface, _ -> false
  | Other, Implementation -> true
  | Implementation, Other -> false

let select_source = fun sources desired_kind ->
  let rec loop selected = function
    | [] -> selected
    | ((source, analysis) as candidate) :: tail ->
        if not (source_kind source = desired_kind) then
          loop selected tail
        else
          (
            match selected with
            | None -> loop (Some candidate) tail
            | Some (existing, _) when prefer_source existing source -> loop (Some candidate) tail
            | Some _ -> loop selected tail
          )
  in
  loop None sources

let source_span = fun (source: Source.t) ->
  Syn.Cst.syntax_node_of_source_file source.cst |> Syn.Cst.token_body_span

let qualified_name = fun scope_path name ->
  IdentPath.append_name scope_path name

let type_decl_key = fun (type_decl: FileSummary.type_decl) ->
  qualified_name type_decl.scope_path type_decl.declaration.type_name

let signature_visible_types = fun ~ambient_type_decls ~interface_decls ~implementation_decls ->
  VisibleTypes.of_type_decls (ambient_type_decls @ interface_decls @ implementation_decls)

let canonical_scheme_string = fun visible_types scheme ->
  VisibleTypes.canonicalize_scheme visible_types scheme |> TypePrinter.scheme_to_string

let canonical_type_string = fun visible_types ty ->
  VisibleTypes.canonicalize_type visible_types ty |> TypePrinter.type_to_string

let poly_variant_tag_signature_string = fun visible_types (tag: TypeDecl.poly_variant_tag) ->
  match tag.payload_type with
  | Some payload_type -> "`" ^ tag.name ^ " of " ^ canonical_type_string visible_types payload_type
  | None -> "`" ^ tag.name

let manifest_signature_string = fun visible_types -> function
  | TypeDecl.Alias manifest_type ->
      "alias(" ^ canonical_type_string visible_types manifest_type ^ ")"
  | TypeDecl.PolyVariant { bound; tags; inherited } ->
      let bound =
        match bound with
        | TypeDecl.Exact -> "exact"
        | TypeDecl.UpperBound -> "upper"
        | TypeDecl.LowerBound -> "lower"
      in
      let tags =
        tags
        |> List.map (poly_variant_tag_signature_string visible_types)
        |> String.concat " | "
      in
      let inherited =
        inherited
        |> List.map (canonical_type_string visible_types)
        |> String.concat " & "
      in
      "poly_variant(" ^ bound ^ "; tags=[" ^ tags ^ "]; inherited=[" ^ inherited ^ "])"

let type_decl_signature_string = fun visible_types (type_decl: FileSummary.type_decl) ->
  let decl = type_decl.declaration in
  let key = type_decl_key type_decl |> IdentPath.to_string in
  let param_variances =
    decl.param_variances
    |> List.map TypeDecl.variance_to_string
    |> String.concat ","
  in
  let constructors =
    decl.constructors
    |> List.map
      (fun (constructor: TypeDecl.constructor) ->
        let inline_record =
          match constructor.inline_record_labels with
          | Some labels ->
              "{"
              ^ (
                  labels
                  |> List.map
                    (fun (label: TypeDecl.label) ->
                      label.name ^ ":" ^ canonical_type_string visible_types label.field_type)
                  |> String.concat ";"
                )
              ^ "}"
          | None -> ""
        in
        constructor.name ^ inline_record ^ ":" ^ canonical_scheme_string visible_types constructor.scheme)
    |> String.concat ";"
  in
  let labels =
    decl.labels
    |> List.map
      (fun (label: TypeDecl.label) ->
        label.name
        ^ ":"
        ^ (if label.mutable_ then "mutable " else "")
        ^ canonical_type_string visible_types label.field_type)
    |> String.concat ";"
  in
  let manifest =
    match decl.manifest with
    | Some (TypeDecl.Alias manifest_type)
      when String.equal key (canonical_type_string visible_types manifest_type) ->
        "none"
    | Some manifest -> manifest_signature_string visible_types manifest
    | None -> "none"
  in
  decl.type_name
  ^ "{arity="
  ^ Int.to_string (List.length decl.param_ids)
  ^ ";variances=["
  ^ param_variances
  ^ "];constructors=["
  ^ constructors
  ^ "];labels=["
  ^ labels
  ^ "];manifest="
  ^ manifest
  ^ "}"

let is_abstract_type_decl = fun (decl: TypeDecl.t) ->
  List.is_empty decl.constructors && List.is_empty decl.labels && Option.is_none decl.manifest

let mark_analysis_errored = fun (analysis: SourceAnalysis.t) diagnostics ->
  if List.is_empty diagnostics then
    analysis
  else
    let file_summary =
      match analysis.file_summary.FileSummary.export_result with
      | FileSummary.TrustedExport { exports }
      | FileSummary.ErroredExport { exports } -> FileSummary.errored
        ~source_id:analysis.source.source_id
        ~type_decls:(FileSummary.type_decls analysis.file_summary)
        exports
      | FileSummary.NoExport -> FileSummary.missing
        ~source_id:analysis.source.source_id
        ~type_decls:(FileSummary.type_decls analysis.file_summary)
        ()
    in
    { analysis with typing_diagnostics = analysis.typing_diagnostics @ diagnostics; file_summary }

let module_typings_of_summary = fun ~module_name ~source_hash ~value_definitions summary ->
  match summary.FileSummary.export_result with
  | FileSummary.TrustedExport { exports } -> ModuleTypings.trusted
    ~module_name
    ~source_hash
    ~type_decls:summary.type_decls
    ~value_definitions
    exports
  | FileSummary.ErroredExport { exports } -> ModuleTypings.errored
    ~module_name
    ~source_hash
    ~type_decls:summary.type_decls
    ~value_definitions
    exports
  | FileSummary.NoExport -> ModuleTypings.missing
    ~module_name
    ~source_hash
    ~type_decls:summary.type_decls
    ~value_definitions
    ()

let with_module_view = fun module_typings (analysis: SourceAnalysis.t) ->
  {
    analysis with file_summary = ModuleTypings.to_file_summary
      ~source_id:analysis.source.source_id
      module_typings
  }

let find_declared_value_span = fun (analysis: SourceAnalysis.t) export_name ->
  match analysis.semantic_tree with
  | None -> None
  | Some semantic_tree ->
      semantic_tree.item_tree |> ItemTree.items |> List.find_map
        (
          function
          | ItemTree.DeclaredValue item when String.equal export_name
            (qualified_name item.scope_path item.value_name |> IdentPath.to_string) -> OriginMap.find
            semantic_tree.origin_map
            item.origin_id
          |> Option.map (fun (origin: OriginMap.origin) -> origin.span)
          | ItemTree.ExtensionConstructor item when String.equal export_name
            (qualified_name item.scope_path item.constructor_name |> IdentPath.to_string) -> OriginMap.find
            semantic_tree.origin_map
            item.origin_id
          |> Option.map (fun (origin: OriginMap.origin) -> origin.span)
          | _ -> None
        )

let find_type_decl_span = fun (analysis: SourceAnalysis.t) type_name ->
  match analysis.semantic_tree with
  | None -> None
  | Some semantic_tree ->
      semantic_tree.item_tree |> ItemTree.items |> List.find_map
        (
          function
          | ItemTree.Type item when String.equal type_name
            (qualified_name item.scope_path item.declaration.type_name |> IdentPath.to_string) -> OriginMap.find
            semantic_tree.origin_map
            item.origin_id
          |> Option.map (fun (origin: OriginMap.origin) -> origin.span)
          | _ -> None
        )

let interface_span_for_mismatch = fun (source, analysis) mismatch ->
  match mismatch with
  | Diagnostic.MissingValue { name }
  | Diagnostic.ValueTypeMismatch { name; _ } -> (
      match find_declared_value_span analysis name with
      | Some span -> span
      | None -> source_span source
    )
  | Diagnostic.MissingTypeDeclaration { name }
  | Diagnostic.TypeDeclarationMismatch { name; _ } -> (
      match find_type_decl_span analysis name with
      | Some span -> span
      | None -> source_span source
    )

let implementation_span_for_mismatch = fun (source, _analysis) _mismatch -> source_span source

let interface_diagnostic = fun interface_pair implementation_pair mismatch ->
  Diagnostic.SignatureInclusionError {
    mismatch_span = interface_span_for_mismatch interface_pair mismatch;
    counterpart_span = Some (implementation_span_for_mismatch implementation_pair mismatch);
    mismatch
  }

let implementation_diagnostic = fun interface_pair implementation_pair mismatch ->
  Diagnostic.SignatureInclusionError {
    mismatch_span = implementation_span_for_mismatch implementation_pair mismatch;
    counterpart_span = Some (interface_span_for_mismatch interface_pair mismatch);
    mismatch
  }

let list_for_all2 = fun predicate left right ->
  let rec loop left right =
    match (left, right) with
    | [], [] -> true
    | left :: left_tail, right :: right_tail ->
        predicate left right && loop left_tail right_tail
    | _ -> false
  in
  loop left right

let rigid_type_equal =
  let rec equal_type left right =
    let left = TypeRepr.prune left in
    let right = TypeRepr.prune right in
    match (TypeRepr.view left, TypeRepr.view right) with
    | TypeRepr.Int, TypeRepr.Int
    | TypeRepr.Float, TypeRepr.Float
    | TypeRepr.Bool, TypeRepr.Bool
    | TypeRepr.String, TypeRepr.String
    | TypeRepr.Char, TypeRepr.Char
    | TypeRepr.Unit, TypeRepr.Unit -> true
    | TypeRepr.Hole left_hole, TypeRepr.Hole right_hole ->
        Int.equal left_hole right_hole
    | TypeRepr.Option left_element, TypeRepr.Option right_element
    | TypeRepr.Array left_element, TypeRepr.Array right_element
    | TypeRepr.List left_element, TypeRepr.List right_element
    | TypeRepr.Seq left_element, TypeRepr.Seq right_element ->
        equal_type left_element right_element
    | TypeRepr.Result (left_ok, left_error), TypeRepr.Result (right_ok, right_error) ->
        equal_type left_ok right_ok && equal_type left_error right_error
    | TypeRepr.Named { head=left_head; arguments=left_arguments }, TypeRepr.Named
      { head=right_head; arguments=right_arguments } ->
        TypeConstructorId.equal left_head.type_constructor_id right_head.type_constructor_id
        && list_for_all2 equal_type left_arguments right_arguments
    | TypeRepr.PolyVariant { bound=left_bound; tags=left_tags; inherited=left_inherited }, TypeRepr.PolyVariant
      { bound=right_bound; tags=right_tags; inherited=right_inherited } ->
        left_bound = right_bound
        && list_for_all2 equal_poly_variant_tag left_tags right_tags
        && list_for_all2 equal_type left_inherited right_inherited
    | TypeRepr.Tuple left_members, TypeRepr.Tuple right_members ->
        list_for_all2 equal_type left_members right_members
    | TypeRepr.Arrow { label=left_label; lhs=left_lhs; rhs=left_rhs }, TypeRepr.Arrow
      { label=right_label; lhs=right_lhs; rhs=right_rhs } ->
        left_label = right_label
        && equal_type left_lhs right_lhs
        && equal_type left_rhs right_rhs
    | TypeRepr.Var { id=left_id; link=None; _ }, TypeRepr.Var { id=right_id; link=None; _ } ->
        Int.equal left_id right_id
    | TypeRepr.Var { link=Some left_link; _ }, _ ->
        equal_type left_link right
    | _, TypeRepr.Var { link=Some right_link; _ } ->
        equal_type left right_link
    | _ -> false
  and equal_poly_variant_tag left right =
    String.equal left.name right.name
    &&
    match (left.payload_type, right.payload_type) with
    | None, None -> true
    | Some left_payload, Some right_payload -> equal_type left_payload right_payload
    | _ -> false
  in
  equal_type

let scheme_includes = fun ~visible_types actual_scheme expected_scheme ->
  let actual_scheme = VisibleTypes.canonicalize_scheme visible_types actual_scheme in
  let expected_scheme = VisibleTypes.canonicalize_scheme visible_types expected_scheme in
  let actual_quantified, actual_body = TypeScheme.to_explicit actual_scheme in
  let _expected_quantified, expected_body = TypeScheme.to_explicit expected_scheme in
  let flexible_actual_vars = Collections.HashSet.of_list actual_quantified in
  let bindings = Collections.HashMap.with_capacity 8 in
  let rec includes_type actual expected =
    let actual = TypeRepr.prune actual in
    let expected = TypeRepr.prune expected in
    match TypeRepr.view actual with
    | TypeRepr.Var { id; link=None; _ } when Collections.HashSet.contains flexible_actual_vars id ->
        (
          match Collections.HashMap.get bindings id with
          | Some bound -> rigid_type_equal bound expected
          | None ->
              let _ = Collections.HashMap.insert bindings id expected in
              true
        )
    | TypeRepr.Var { link=Some actual_link; _ } ->
        includes_type actual_link expected
    | _ ->
        match (TypeRepr.view actual, TypeRepr.view expected) with
        | TypeRepr.Int, TypeRepr.Int
        | TypeRepr.Float, TypeRepr.Float
        | TypeRepr.Bool, TypeRepr.Bool
        | TypeRepr.String, TypeRepr.String
        | TypeRepr.Char, TypeRepr.Char
        | TypeRepr.Unit, TypeRepr.Unit -> true
        | TypeRepr.Hole actual_hole, TypeRepr.Hole expected_hole ->
            Int.equal actual_hole expected_hole
        | TypeRepr.Option actual_element, TypeRepr.Option expected_element
        | TypeRepr.Array actual_element, TypeRepr.Array expected_element
        | TypeRepr.List actual_element, TypeRepr.List expected_element
        | TypeRepr.Seq actual_element, TypeRepr.Seq expected_element ->
            includes_type actual_element expected_element
        | TypeRepr.Result (actual_ok, actual_error), TypeRepr.Result (expected_ok, expected_error) ->
            includes_type actual_ok expected_ok && includes_type actual_error expected_error
        | TypeRepr.Named { head=actual_head; arguments=actual_arguments }, TypeRepr.Named
          { head=expected_head; arguments=expected_arguments } ->
            TypeConstructorId.equal
              actual_head.type_constructor_id
              expected_head.type_constructor_id
            && list_for_all2 includes_type actual_arguments expected_arguments
        | TypeRepr.PolyVariant { bound=actual_bound; tags=actual_tags; inherited=actual_inherited }, TypeRepr.PolyVariant
          { bound=expected_bound; tags=expected_tags; inherited=expected_inherited } ->
            actual_bound = expected_bound
            && list_for_all2 includes_poly_variant_tag actual_tags expected_tags
            && list_for_all2 includes_type actual_inherited expected_inherited
        | TypeRepr.Tuple actual_members, TypeRepr.Tuple expected_members ->
            list_for_all2 includes_type actual_members expected_members
        | TypeRepr.Arrow { label=actual_label; lhs=actual_lhs; rhs=actual_rhs }, TypeRepr.Arrow
          { label=expected_label; lhs=expected_lhs; rhs=expected_rhs } ->
            actual_label = expected_label
            && includes_type actual_lhs expected_lhs
            && includes_type actual_rhs expected_rhs
        | TypeRepr.Var { id=actual_id; link=None; _ }, TypeRepr.Var { id=expected_id; link=None; _ } ->
            Int.equal actual_id expected_id
        | _, TypeRepr.Var { link=Some expected_link; _ } ->
            includes_type actual expected_link
        | _ -> false
  and includes_poly_variant_tag actual expected =
    String.equal actual.name expected.name
    &&
    match (actual.payload_type, expected.payload_type) with
    | None, None -> true
    | Some actual_payload, Some expected_payload -> includes_type actual_payload expected_payload
    | _ -> false
  in
  includes_type actual_body expected_body

let value_mismatches = fun ~visible_types interface_exports implementation_exports ->
  interface_exports |> List.filter_map
    (fun (name, expected_scheme) ->
      match List.assoc_opt name implementation_exports with
      | None -> Some (Diagnostic.MissingValue { name })
      | Some actual_scheme ->
          let expected = canonical_scheme_string visible_types expected_scheme in
          let actual = canonical_scheme_string visible_types actual_scheme in
          if scheme_includes ~visible_types actual_scheme expected_scheme then
            None
          else
            Some (Diagnostic.ValueTypeMismatch { name; expected; actual }))

let type_decl_matches = fun ~visible_types interface_decl implementation_decl ->
  let interface_arity = List.length interface_decl.FileSummary.declaration.TypeDecl.param_ids in
  let implementation_arity = List.length implementation_decl.FileSummary.declaration.TypeDecl.param_ids in
  if not (interface_arity = implementation_arity) then
    false
  else if is_abstract_type_decl interface_decl.FileSummary.declaration then
    true
  else
    String.equal
      (type_decl_signature_string visible_types interface_decl)
      (type_decl_signature_string visible_types implementation_decl)

let type_decl_mismatches = fun ~visible_types interface_decls implementation_decls ->
  interface_decls |> List.filter_map
    (fun (interface_decl: FileSummary.type_decl) ->
      let key = type_decl_key interface_decl in
      match
        implementation_decls |> List.find_opt
          (fun (implementation_decl: FileSummary.type_decl) ->
            IdentPath.equal key (type_decl_key implementation_decl))
      with
      | None -> Some (Diagnostic.MissingTypeDeclaration { name = IdentPath.to_string key })
      | Some implementation_decl ->
          if
            type_decl_matches
              ~visible_types
              interface_decl
              implementation_decl
          then
            None
          else
            Some (Diagnostic.TypeDeclarationMismatch {
              name = IdentPath.to_string key;
              expected = type_decl_signature_string visible_types interface_decl;
              actual = type_decl_signature_string visible_types implementation_decl
            }))

let interface_shaped_export_result = fun interface_summary implementation_summary ->
  let exports = FileSummary.exports interface_summary in
  match (
    interface_summary.FileSummary.export_result,
    implementation_summary.FileSummary.export_result
  ) with
  | (FileSummary.NoExport, _)
  | (_, FileSummary.NoExport) -> FileSummary.NoExport
  | FileSummary.TrustedExport _, FileSummary.TrustedExport _ -> FileSummary.TrustedExport { exports }
  | _ -> FileSummary.ErroredExport { exports }

let should_check_signature_inclusion = fun interface_summary implementation_summary ->
  match (
    interface_summary.FileSummary.export_result,
    implementation_summary.FileSummary.export_result
  ) with
  | (FileSummary.TrustedExport _, FileSummary.TrustedExport _) -> true
  | _ -> false

let analyses_by_source = fun sources ->
  sources
  |> List.map (fun (_source, (analysis: SourceAnalysis.t)) -> (analysis.source.source_id, analysis))

let extra_analyses = fun excluded_ids sources ->
  sources |> List.filter_map
    (fun (_source, (analysis: SourceAnalysis.t)) ->
      if List.exists (SourceId.equal analysis.source.source_id) excluded_ids then
        None
      else
        Some (analysis.source.source_id, analysis))

let paired_module_view = fun ~module_name interface_pair implementation_pair ->
  let (_interface_source, (interface_analysis: SourceAnalysis.t)) = interface_pair in
  let (_implementation_source, (implementation_analysis: SourceAnalysis.t)) = implementation_pair in
  let export_result = interface_shaped_export_result
    interface_analysis.file_summary
    implementation_analysis.file_summary in
  let type_decls =
    match export_result with
    | FileSummary.NoExport -> []
    | _ -> FileSummary.type_decls interface_analysis.file_summary
  in
  let value_definitions =
    match export_result with
    | FileSummary.NoExport -> []
    | _ -> SourceAnalysis.export_definitions interface_analysis
  in
  let source_hash = ModuleTypings.synthetic_source_hash
    ~module_name
    ~export_result
    ~type_decls
    ~value_definitions
    () in
  let module_typings = module_typings_of_summary
    ~module_name
    ~source_hash
    ~value_definitions
    { FileSummary.source_id = interface_analysis.source.source_id; export_result; type_decls } in
  let interface_analysis = with_module_view module_typings interface_analysis in
  let implementation_analysis = with_module_view module_typings implementation_analysis in
  {
    module_typings;
    analyses_by_source = [
      (interface_analysis.source.source_id, interface_analysis);
      (implementation_analysis.source.source_id, implementation_analysis);
    ]
    @ extra_analyses
      [ interface_analysis.source.source_id; implementation_analysis.source.source_id ]
      [ interface_pair; implementation_pair ];
    signature_mismatches = [];
  }

let pair_interface_and_implementation = fun ~module_name interface_pair implementation_pair ->
  let (_interface_source, (interface_analysis: SourceAnalysis.t)) = interface_pair in
  let (_implementation_source, (implementation_analysis: SourceAnalysis.t)) = implementation_pair in
  if not (should_check_signature_inclusion interface_analysis.file_summary implementation_analysis.file_summary)
  then
    paired_module_view ~module_name interface_pair implementation_pair
  else
    let ambient_type_decls = interface_analysis.ambient_type_decls @ implementation_analysis.ambient_type_decls in
    let visible_types = signature_visible_types
      ~ambient_type_decls
      ~interface_decls:(FileSummary.type_decls interface_analysis.file_summary)
      ~implementation_decls:(FileSummary.type_decls implementation_analysis.file_summary) in
    let mismatches = value_mismatches
      ~visible_types
      (FileSummary.exports interface_analysis.file_summary)
      (FileSummary.exports implementation_analysis.file_summary)
    @ type_decl_mismatches
      ~visible_types
      (FileSummary.type_decls interface_analysis.file_summary)
      (FileSummary.type_decls implementation_analysis.file_summary) in
    if List.is_empty mismatches then
      paired_module_view ~module_name interface_pair implementation_pair
    else
    let interface_diagnostics = mismatches
    |> List.map (interface_diagnostic interface_pair implementation_pair) in
    let implementation_diagnostics = mismatches
    |> List.map (implementation_diagnostic interface_pair implementation_pair) in
    let module_typings = ModuleTypings.missing
      ~module_name
      ~source_hash:(ModuleTypings.synthetic_source_hash
        ~module_name
        ~export_result:FileSummary.NoExport
        ~type_decls:[]
        ())
      () in
    let interface_analysis = mark_analysis_errored interface_analysis interface_diagnostics
    |> with_module_view module_typings in
    let implementation_analysis = mark_analysis_errored implementation_analysis implementation_diagnostics
    |> with_module_view module_typings in
    {
      module_typings;
      analyses_by_source = [
        (interface_analysis.source.source_id, interface_analysis);
        (implementation_analysis.source.source_id, implementation_analysis);
      ]
      @ extra_analyses
        [ interface_analysis.source.source_id; implementation_analysis.source.source_id ]
        [ interface_pair; implementation_pair ];
      signature_mismatches = mismatches;
    }

let singleton_module_typings = fun ~module_name (source, (analysis: SourceAnalysis.t)) ->
  module_typings_of_summary
    ~module_name
    ~source_hash:(Source.input_hash source)
    ~value_definitions:(SourceAnalysis.export_definitions analysis)
    analysis.file_summary

let of_sources = fun ~module_name sources ->
  match (select_source sources Interface, select_source sources Implementation) with
  | Some interface_pair, Some implementation_pair ->
      pair_interface_and_implementation ~module_name interface_pair implementation_pair
  | Some interface_pair, None ->
      let (source, analysis) = interface_pair in
      {
        module_typings = singleton_module_typings ~module_name (source, analysis);
        analyses_by_source = analyses_by_source sources;
        signature_mismatches = [];
      }
  | None, Some implementation_pair ->
      let (source, analysis) = implementation_pair in
      {
        module_typings = singleton_module_typings ~module_name (source, analysis);
        analyses_by_source = analyses_by_source sources;
        signature_mismatches = [];
      }
  | None, None ->
      let source_hash = ModuleTypings.synthetic_source_hash
        ~module_name
        ~export_result:FileSummary.NoExport
        ~type_decls:[]
        () in
      {
        module_typings = ModuleTypings.missing ~module_name ~source_hash ();
        analyses_by_source = analyses_by_source sources;
        signature_mismatches = [];
      }
