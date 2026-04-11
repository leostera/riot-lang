open Std
open Typ
open Typ.Analysis
open Typ.Diagnostics
open Typ.Model
open Typ.Session

let export_names = fun export ->
  match export with
  | Some (FileSummary.TrustedExport { exports })
  | Some (FileSummary.ErroredExport { exports }) -> List.map
    (fun (name, _scheme) -> SurfacePath.to_string name)
    exports
  | Some FileSummary.NoExport
  | None -> []

let export_scheme = fun snapshot source_id name ->
  match Query.export_of snapshot source_id with
  | Some (FileSummary.TrustedExport { exports })
  | Some (FileSummary.ErroredExport { exports }) ->
      exports |> List.find_map
        (fun (candidate_name, scheme) ->
          if SurfacePath.equal (SurfacePath.of_string name) candidate_name then
            Some scheme
          else
            None) |> Option.map TypePrinter.scheme_to_string
  | Some FileSummary.NoExport
  | None -> None

let lookup_export = fun name exports ->
  exports |> List.find_map
    (fun (candidate_name, value) ->
      if SurfacePath.equal candidate_name (SurfacePath.of_string name) then
        Some value
      else
        None)

let inferred_type_at = fun snapshot source_id offset ->
  Query.type_at snapshot source_id (Position.make ~offset) |> function
  | Some ty -> Some (TypePrinter.type_to_string ty)
  | None -> None

let definition_at = fun snapshot source_id offset ->
  Query.definition_at snapshot source_id (Position.make ~offset)

let source_origin_label = fun origin ->
  match origin with
  | Source.Path path -> Path.to_string path
  | Source.Label label -> label

let definition_covers_offset = fun (definition: ModuleTypings.definition_site) offset ->
  Position.is_within_span (Position.make ~offset) definition.span

let offset_of_substring = fun text needle ->
  let text_length = String.length text in
  let needle_length = String.length needle in
  let max_start = text_length - needle_length in
  let rec loop start =
    if start > max_start then
      None
    else if String.sub text start needle_length = needle then
      Some start
    else
      loop (start + 1)
  in
  if needle_length = 0 then
    Some 0
  else if needle_length > text_length then
    None
  else
    loop 0

let has_unbound_name = fun snapshot source_id ->
  Query.diagnostics snapshot source_id |> List.exists
    (
      function
      | Query.Typing (Diagnostic.UnboundName _) -> true
      | _ -> false
    )

let has_unsupported_syntax = fun snapshot source_id ->
  Query.diagnostics snapshot source_id |> List.exists
    (
      function
      | Query.Lowering (Diagnostic.UnsupportedSyntax _) -> true
      | _ -> false
    )

let has_signature_error = fun snapshot source_id ->
  Query.diagnostics snapshot source_id |> List.exists
    (
      function
      | Query.Typing (Diagnostic.SignatureInclusionError _) -> true
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

let typing_diagnostic_summaries = fun snapshot source_id ->
  Query.diagnostics snapshot source_id |> List.filter_map
    (
      function
      | Query.Typing diagnostic -> Some (
        Diagnostic.code diagnostic,
        Diagnostic.severity diagnostic |> Diagnostic.severity_to_string,
        Diagnostic.message diagnostic
      )
      | Query.Parse _
      | Query.Lowering _ -> None
    )

let show_option = fun value ->
  match value with
  | Some value -> format Format.[ str "Some("; str value; str ")" ]
  | None -> "None"

let test_local_module_aliases_include_public_wrapper_spellings = fun _ctx ->
  let aliases = LocalModules.local_module_aliases_of_internal_name
    (LocalModules.InternalName.of_string "Kernel_new__Net__Tcp_listener")
  |> List.map LocalModules.AmbientName.to_string in
  Test.assert_equal
    ~expected:[ "Net.TcpListener"; "TcpListener"; "Net.Tcp_listener"; "Tcp_listener" ]
    ~actual:aliases;
  Ok ()

let test_contextual_local_module_depth_prefers_deeper_shared_prefix = fun _ctx ->
  let current_module_name = LocalModules.InternalName.of_string "Kernel_new__Net__Tcp_listener__Unix" in
  let required_module_name = LocalModules.RequiredName.of_string "Addr" in
  let candidate_module_name = LocalModules.InternalName.of_string "Kernel_new__Net__Tcp_listener__Addr" in
  let depth = LocalModules.contextual_match_depth ~current_module_name ~required_module_name ~candidate_module_name in
  Test.assert_equal ~expected:(Some 3) ~actual:depth;
  Ok ()

let test_contextual_local_module_depth_keeps_single_segment_suffixes = fun _ctx ->
  let current_module_name = LocalModules.InternalName.of_string "Kernel_new__Net__Ip_addr" in
  let required_module_name = LocalModules.RequiredName.of_string "Unix" in
  let direct_candidate = LocalModules.InternalName.of_string "Kernel_new__Net__Ip_addr__Unix" in
  let cousin_candidate = LocalModules.InternalName.of_string "Kernel_new__Net__Addr__Unix" in
  let direct_depth = LocalModules.contextual_match_depth
    ~current_module_name
    ~required_module_name
    ~candidate_module_name:direct_candidate in
  let cousin_depth = LocalModules.contextual_match_depth
    ~current_module_name
    ~required_module_name
    ~candidate_module_name:cousin_candidate in
  Test.assert_equal ~expected:(Some 3) ~actual:direct_depth;
  Test.assert_equal ~expected:(Some 2) ~actual:cousin_depth;
  Ok ()

let test_local_module_implicit_opens_skip_enclosing_alias_wrappers = fun _ctx ->
  let current_module_name = LocalModules.InternalName.of_string "Kernel_new__Fs__Read_dir" in
  let include_root_alias = LocalModules.should_include_implicit_open
    ~current_module_name
    ~module_name:"Kernel_new__Aliases" in
  let include_fs_alias = LocalModules.should_include_implicit_open
    ~current_module_name
    ~module_name:"Kernel_new__Fs__Aliases" in
  let include_colors_alias = LocalModules.should_include_implicit_open
    ~current_module_name
    ~module_name:"Colors__Aliases" in
  Test.assert_equal ~expected:false ~actual:include_root_alias;
  Test.assert_equal ~expected:false ~actual:include_fs_alias;
  Test.assert_equal ~expected:true ~actual:include_colors_alias;
  Ok ()

let trace_debug = fun snapshot source_id ->
  match Query.analysis_of_source snapshot source_id with
  | None -> []
  | Some analysis ->
      let item_lines = analysis.item_traces
      |> List.map
        (fun (trace: Check_result.item_trace) ->
          format
            Format.[
              str "item ";
              str (ItemArenaId.to_string trace.item_id);
              str " -> [";
              str (String.concat ", " (List.map fst trace.exports_after));
              str "]";
            ]) in
      let expr_lines = analysis.expr_traces
      |> List.map
        (fun (trace: Check_result.expr_trace) ->
          format
            Format.[
              str "expr ";
              str (ExprArenaId.to_string trace.expr_id);
              str " -> [";
              str (String.concat ", " (List.map fst trace.env_before));
              str "]";
            ]) in
      item_lines @ expr_lines

let module_typings_jsons = fun snapshot ->
  Snapshot.module_typings snapshot |> List.map ModuleTypings.Json.to_json

let typ_event_name = fun ({ Event.kind; _ }: Event.t) ->
  match kind with
  | Event.PrepareSnapshotStarted _ -> "typ_prepare_snapshot_start"
  | Event.HydrateModuleTypingsStarted _ -> "typ_hydrate_module_typings_start"
  | Event.HydrateModuleTypingsFinished _ -> "typ_hydrate_module_typings_finish"
  | Event.PrepareSnapshotFailed _ -> "typ_prepare_snapshot_failed"
  | Event.PrepareSnapshotFinished _ -> "typ_prepare_snapshot_finish"
  | Event.SnapshotMaterializationStarted _ -> "typ_snapshot_materialization_start"
  | Event.SnapshotMaterializationFinished _ -> "typ_snapshot_materialization_finish"
  | Event.SourceAnalysisStarted _ -> "typ_source_analysis_start"
  | Event.SourceAnalysisFinished _ -> "typ_source_analysis_finish"
  | Event.ModulePairingStarted _ -> "typ_module_pairing_start"
  | Event.ModulePairingFinished _ -> "typ_module_pairing_finish"
  | Event.ModuleTypingsCollectionStarted _ -> "typ_module_typings_collection_start"
  | Event.ModuleTypingsCollectionFinished _ -> "typ_module_typings_collection_finish"

let typ_event_instants_are_monotonic = fun (events: Event.t list) ->
  let rec loop previous remaining_events =
    match remaining_events with
    | [] -> true
    | ({ Event.instant_us; _ }: Event.t) :: rest -> instant_us >= previous && loop instant_us rest
  in
  loop 0 events

let typ_events_json = fun events -> Data.Json.Array (events |> List.map Event.to_json) |> Data.Json.to_string

let exported_type_names = fun snapshot source_id ->
  match Query.module_typings_of snapshot source_id with
  | None -> []
  | Some typings ->
      ModuleTypings.type_decls typings |> List.map
        (fun (type_decl: FileSummary.type_decl) ->
          if SurfacePath.is_empty type_decl.scope_path then
            type_decl.declaration.type_name
          else
            SurfacePath.append_name type_decl.scope_path type_decl.declaration.type_name
            |> SurfacePath.to_string)

let module_typings_export_names = fun typings ->
  ModuleTypings.exports typings |> List.map (fun (name, _scheme) -> SurfacePath.to_string name)

let file_summary_export_names = fun snapshot source_id ->
  match Query.file_summary_of snapshot source_id with
  | None -> []
  | Some summary -> FileSummary.exports summary
  |> List.map (fun (name, _scheme) -> SurfacePath.to_string name)

let prepare_snapshot_or_error = fun session ~roots ->
  match Session.prepare_snapshot session ~roots with
  | Ok snapshot -> Ok snapshot
  | Error missing -> Error (format
    Format.[
      str "unexpected missing requirements: ";
      str (Data.Json.to_string (Session.MissingRequirements.to_json missing));
    ])

let with_typ_store = fun f ->
  Fs.with_tempdir ~prefix:"typ-store"
    (fun tmpdir ->
      let contentstore = Contentstore.create
        ~root:Path.(tmpdir / Path.v "cache")
        ~policy:Contentstore.Policy.default
        () in
      let store = Store.create contentstore () in
      f store) |> Result.unwrap_or ~default:(Error "tempdir creation failed")

let qualify_exports = fun module_name exports ->
  let module_path = SurfacePath.of_name module_name in
  List.map (fun (name, scheme) -> (SurfacePath.append_path module_path name, scheme)) exports

let qualify_type_decls = fun module_name type_decls ->
  List.map
    (fun (type_decl: FileSummary.type_decl) ->
      {
        FileSummary.scope_path = SurfacePath.prepend_name module_name type_decl.scope_path;
        declaration = type_decl.declaration
      })
    type_decls

let expect_cst = fun ~filename parse_result ->
  match Syn.build_cst parse_result with
  | Ok cst -> cst
  | Error (Syn.Parse_diagnostics diagnostics) -> panic
    (format
      Format.[
        str "expected successful CST for ";
        str filename;
        str " but parser reported diagnostics: ";
        str (String.concat "; " (List.map Syn.Diagnostic.to_string diagnostics));
      ])
  | Error (Syn.Cst_builder_error error) -> panic
    (format
      Format.[
        str "expected successful CST for ";
        str filename;
        str " but CST build failed: ";
        str error.message;
      ])

let create_source = fun session ~kind ~origin ~text ->
  let filename =
    match origin with
    | Source.Path path -> path
    | Source.Label label -> Path.v label
  in
  let parse_result = Syn.parse ~filename text in
  let cst = expect_cst ~filename:(Path.to_string filename) parse_result in
  let implicit_opens = [] in
  Session.create_source
    session
    ~kind
    ~module_name:(Source.infer_module_name origin)
    ~implicit_opens
    ~origin
    ~source_hash:(Source.hash ~implicit_opens ~cst)
    ~parse_result
    ~cst

let update_source_text = fun session source_id ~kind ~origin ~text ->
  let filename =
    match origin with
    | Source.Path path -> path
    | Source.Label label -> Path.v label
  in
  let parse_result = Syn.parse ~filename text in
  let cst = expect_cst ~filename:(Path.to_string filename) parse_result in
  let implicit_opens = [] in
  Session.update_source
    session
    source_id
    ~source_hash:(Source.hash ~implicit_opens ~cst)
    ~parse_result
    ~cst

let make_source_with_implicit_opens = fun ~implicit_opens ~source_id ~kind ~origin ~revision ~text ->
  let filename =
    match origin with
    | Source.Path path -> path
    | Source.Label label -> Path.v label
  in
  let parse_result = Syn.parse ~filename text in
  let cst = expect_cst ~filename:(Path.to_string filename) parse_result in
  Source.make_prepared
    ~source_id
    ~kind
    ~module_name:(Source.infer_module_name origin)
    ~implicit_opens
    ~origin
    ~revision
    ~source_hash:(Source.hash ~implicit_opens ~cst)
    ~parse_result
    ~cst

let make_source = fun ~source_id ~kind ~origin ~revision ~text ->
  make_source_with_implicit_opens ~implicit_opens:[] ~source_id ~kind ~origin ~revision ~text

let prepared_source = fun ~filename ~text ->
  let source_id = SourceId.of_int 0 in
  let origin = Source.Label filename in
  let kind = Source.File in
  let parse_result = Syn.parse ~filename:(Path.v filename) text in
  let cst = expect_cst ~filename parse_result in
  let implicit_opens = [] in
  Source.make_prepared
    ~source_id
    ~kind
    ~module_name:(Source.infer_module_name origin)
    ~implicit_opens
    ~origin
    ~revision:0
    ~source_hash:(Source.hash ~implicit_opens ~cst)
    ~parse_result
    ~cst

let imported_world_of_loaded_modules = fun loaded_modules (source: Source.t) ->
  let loaded_modules_index = LoadedModules.of_list loaded_modules in
  let required_names =
    match Syn.Deps.of_parse_result source.parse_result with
    | Ok deps -> Syn.Deps.modules deps
    | Error _ -> []
  in
  let visible_modules =
    required_names
    |> List.map LocalModules.RequiredName.of_string
    |> List.filter_map
      (fun required_name ->
        if LoadedModules.contains loaded_modules_index ~required_name then
          Some (
            SurfacePath.of_string (LocalModules.RequiredName.to_string required_name),
            PackageEnv.ModuleId.Loaded required_name
          )
        else
          None)
    |> List.sort_uniq compare
  in
  let implicit_open_modules =
    source.implicit_opens
    |> List.map SurfacePath.to_string
    |> List.map LocalModules.RequiredName.of_string
    |> List.filter_map
      (fun required_name ->
        if LoadedModules.contains loaded_modules_index ~required_name then
          Some (
            SurfacePath.of_string (LocalModules.RequiredName.to_string required_name),
            PackageEnv.ModuleId.Loaded required_name
          )
        else
          None)
    |> List.sort_uniq compare
  in
  ImportedWorld.create
    ~package_env:(PackageEnv.of_loaded_modules loaded_modules_index)
    ~scope_view:(ScopeView.create ~visible_modules ~implicit_open_modules)

let prepared_check_source = fun ~source_id ~filename ~internal_module_name ~local_module_name ~public_module_name ~text ->
  let path = Path.v filename in
  let origin = Source.Label filename in
  let kind = Source.File in
  let parse_result = Syn.parse ~filename:path text in
  let cst = expect_cst ~filename parse_result in
  let implicit_opens = [] in
  let source = Source.make_prepared
    ~source_id
    ~kind
    ~module_name:internal_module_name
    ~implicit_opens
    ~origin
    ~revision:0
    ~source_hash:(Source.hash ~implicit_opens ~cst)
    ~parse_result
    ~cst in
  {
    Check.display_path = path;
    internal_module_name = LocalModules.InternalName.of_string internal_module_name;
    local_module_name = LocalModules.AmbientName.of_string local_module_name;
    public_module_name = public_module_name |> Option.map LocalModules.AmbientName.of_string;
    source;
  }

let check_source_text = fun ~filename text ->
  let parse_result = Syn.parse ~filename text in
  let cst = expect_cst ~filename:(Path.to_string filename) parse_result in
  let origin = Source.Path filename in
  let implicit_opens = [] in
  let source = Source.make_prepared
    ~source_id:(SourceId.of_int 0)
    ~kind:Source.File
    ~module_name:(Source.infer_module_name origin)
    ~implicit_opens
    ~origin
    ~revision:0
    ~source_hash:(Source.hash ~implicit_opens ~cst)
    ~parse_result
    ~cst in
  Typ.check ~config:Config.default ~source

let scheme_has_named_path =
  let rec type_has_named_path ty =
    match TypeRepr.view ty with
    | TypeRepr.Option item
    | TypeRepr.Array item
    | TypeRepr.List item
    | TypeRepr.Seq item -> type_has_named_path item
    | TypeRepr.Result (left, right) -> type_has_named_path left || type_has_named_path right
    | TypeRepr.Named { arguments; _ }
    | TypeRepr.Tuple arguments -> List.exists type_has_named_path arguments
    | TypeRepr.PolyVariant { tags; inherited; _ } ->
        List.exists
          (fun (tag: TypeRepr.poly_variant_tag) ->
            match tag.payload_type with
            | Some payload_type -> type_has_named_path payload_type
            | None -> false)
          tags || List.exists type_has_named_path inherited
    | TypeRepr.Arrow { lhs; rhs; _ } -> type_has_named_path lhs || type_has_named_path rhs
    | TypeRepr.Package signature -> List.exists
      (fun (value: TypeRepr.package_value) -> type_has_named_path (TypeScheme.body value.scheme))
      signature.values
    | TypeRepr.Int
    | TypeRepr.Float
    | TypeRepr.Bool
    | TypeRepr.String
    | TypeRepr.Char
    | TypeRepr.Unit
    | TypeRepr.Var _
    | TypeRepr.Hole _ -> false
  in
  fun scheme ->
    let (_quantified, body) = TypeScheme.to_explicit scheme in
    type_has_named_path body

let test_source_id_stays_stable_across_updates = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "stable.ml")
    ~text:"let x = 1" in
  let snapshot_before = Session.snapshot session in
  let session = update_source_text
    session
    source_id
    ~kind:Source.File
    ~origin:(Source.Label "stable.ml")
    ~text:"let y = 2" in
  let snapshot_after = Session.snapshot session in
  let before_names = export_names (Query.export_of snapshot_before source_id) in
  let after_names = export_names (Query.export_of snapshot_after source_id) in
  Test.assert_equal ~expected:[ "x" ] ~actual:before_names;
  Test.assert_equal ~expected:[ "y" ] ~actual:after_names;
  Ok ()

let test_snapshots_remain_immutable_after_updates = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "immutable.ml")
    ~text:"let x = 1" in
  let snapshot_before = Session.snapshot session in
  let session = update_source_text
    session
    source_id
    ~kind:Source.File
    ~origin:(Source.Label "immutable.ml")
    ~text:"let x = true" in
  let snapshot_after = Session.snapshot session in
  let before_type = inferred_type_at snapshot_before source_id 8 in
  let after_type = inferred_type_at snapshot_after source_id 8 in
  Test.assert_equal ~expected:(Some "int") ~actual:before_type;
  Test.assert_equal ~expected:(Some "bool") ~actual:after_type;
  Ok ()

let test_type_at_uses_smallest_indexed_expression = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = "let id x = x\nlet answer = id 42\n" in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "type_at.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let callee_type = inferred_type_at snapshot source_id 26 in
  let argument_type = inferred_type_at snapshot source_id 29 in
  Test.assert_equal ~expected:(Some "int -> int") ~actual:callee_type;
  Test.assert_equal ~expected:(Some "int") ~actual:argument_type;
  Ok ()

let test_definition_at_uses_local_pattern_origin = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = "let id x = x\nlet answer = id 42\n" in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "definition_local.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let use_offset = offset_of_substring source "id 42" |> Option.expect ~msg:"expected local use" in
  match definition_at snapshot source_id use_offset with
  | None -> Error "expected local definition"
  | Some definition ->
      Test.assert_equal
        ~expected:"definition_local.ml"
        ~actual:(source_origin_label definition.origin);
      let expected_offset = offset_of_substring source "id x" |> Option.expect ~msg:"expected local binder" in
      Test.assert_equal ~expected:true ~actual:(definition_covers_offset definition expected_offset);
      Ok ()

let test_definition_at_uses_exported_module_typings = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let colors_source = "let to_string value = value\n" in
  let (session, _colors_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:colors_source in
  let client_source = "open Colors\nlet label = to_string \"ok\"\n" in
  let (session, client_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "client.ml")
    ~text:client_source in
  let snapshot = Session.snapshot session in
  let use_offset = offset_of_substring client_source "to_string \"ok\"" |> Option.expect ~msg:"expected opened use" in
  match definition_at snapshot client_source_id use_offset with
  | None -> Error "expected opened-module definition"
  | Some definition ->
      Test.assert_equal ~expected:"colors.ml" ~actual:(source_origin_label definition.origin);
      let expected_offset = offset_of_substring colors_source "to_string value"
      |> Option.expect ~msg:"expected export binder" in
      Test.assert_equal ~expected:true ~actual:(definition_covers_offset definition expected_offset);
      Ok ()

let test_definition_at_prefers_interface_export_origin = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let intf_source = "val answer : int\n" in
  let (session, _intf_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.mli")
    ~text:intf_source in
  let (session, _impl_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"let answer = 42\n" in
  let client_source = "let value = Colors.answer\n" in
  let (session, client_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "client.ml")
    ~text:client_source in
  let snapshot = Session.snapshot session in
  let use_offset = offset_of_substring client_source "answer" |> Option.expect ~msg:"expected qualified use" in
  match definition_at snapshot client_source_id use_offset with
  | None -> Error "expected interface-backed definition"
  | Some definition ->
      Test.assert_equal ~expected:"colors.mli" ~actual:(source_origin_label definition.origin);
      let expected_offset = offset_of_substring intf_source "answer" |> Option.expect ~msg:"expected interface declaration" in
      Test.assert_equal ~expected:true ~actual:(definition_covers_offset definition expected_offset);
      Ok ()

let test_definition_at_follows_include_reexports = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let base_source = "let value = 1\n" in
  let (session, _base_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "base.ml")
    ~text:base_source in
  let (session, _wrapper_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "wrapper.ml")
    ~text:"include Base\n" in
  let client_source = "let current = Wrapper.value\n" in
  let (session, client_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "client.ml")
    ~text:client_source in
  let snapshot = Session.snapshot session in
  let use_offset = offset_of_substring client_source "value" |> Option.expect ~msg:"expected reexported use" in
  match definition_at snapshot client_source_id use_offset with
  | None -> Error "expected include-backed definition"
  | Some definition ->
      Test.assert_equal ~expected:"base.ml" ~actual:(source_origin_label definition.origin);
      let expected_offset = offset_of_substring base_source "value = 1" |> Option.expect ~msg:"expected base binder" in
      Test.assert_equal ~expected:true ~actual:(definition_covers_offset definition expected_offset);
      Ok ()

let test_snapshot_without_traces_still_reports_diagnostics_and_module_typings = fun _ctx ->
  let config = Config.default |> Config.with_capture_traces ~capture_traces:false in
  let session = Session.empty ~config in
  let source = "let id x = x\nlet broken = missing\n" in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "no_traces.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  let analysis = Query.analysis_of_source snapshot source_id |> Option.expect ~msg:"missing analysis" in
  let module_typings = Query.module_typings_of snapshot source_id in
  let missing_offset = offset_of_substring source "missing" |> Option.expect ~msg:"missing offset" in
  let inferred = inferred_type_at snapshot source_id missing_offset in
  Test.assert_equal ~expected:[] ~actual:analysis.expr_traces;
  Test.assert_equal ~expected:[] ~actual:analysis.item_traces;
  Test.assert_equal ~expected:(Data.Json.Array []) ~actual:(TypeIndex.to_json analysis.type_index);
  Test.assert_equal ~expected:None ~actual:inferred;
  if
    List.exists (fun diagnostic -> Option.is_some (offset_of_substring diagnostic "unbound name")) diagnostics
  then
    match module_typings with
    | Some _ -> Ok ()
    | None -> Error "expected module typings even when traces are disabled"
  else
    Error (format
      Format.[
        str "expected unbound-name diagnostics, got:\n";
        str (String.concat "\n" diagnostics);
      ])

let test_snapshot_exposes_implicit_file_modules = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, colors_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"module RGB = struct let blend x y = x end\nlet to_string value = value\n" in
  let (session, demo_source_id) = create_source
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
  if demo_has_unbound_name then
    Error (String.concat "\n" (demo_diagnostics @ trace_debug snapshot demo_source_id))
  else if not (color_exports = [ "to_string"; "RGB.blend" ]) then
    Error (format
      Format.[ str "unexpected colors exports: "; str (String.concat ", " color_exports); ])
  else if not (midpoint_type = Some "int -> int -> int") then
    Error (format Format.[ str "unexpected midpoint type: "; str (show_option midpoint_type); ])
  else if not (label_type = Some "string -> string") then
    Error (format Format.[ str "unexpected label type: "; str (show_option label_type); ])
  else
    Ok ()

let test_prepare_snapshot_uses_implicit_opened_alias_modules_with_internal_names = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let helper_text = "let twice x = x + x\n" in
  let helper_parse_result = Syn.parse ~filename:(Path.v "helper.ml") helper_text in
  let helper_cst = expect_cst ~filename:"helper.ml" helper_parse_result in
  let (session, _helper_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~module_name:"Colors__Helper"
    ~implicit_opens:[]
    ~origin:(Source.Label "helper.ml")
    ~source_hash:(Source.hash ~implicit_opens:[] ~cst:helper_cst)
    ~parse_result:helper_parse_result
    ~cst:helper_cst in
  let aliases_text = "module Helper = Colors__Helper\n" in
  let aliases_parse_result = Syn.parse ~filename:(Path.v "Colors__Aliases.ml-gen") aliases_text in
  let aliases_cst = expect_cst ~filename:"Colors__Aliases.ml-gen" aliases_parse_result in
  let (session, _aliases_source_id) = Session.create_source
    session
    ~kind:Source.Generated
    ~module_name:"Colors__Aliases"
    ~implicit_opens:[]
    ~origin:(Source.Label "Colors__Aliases.ml-gen")
    ~source_hash:(Source.hash ~implicit_opens:[] ~cst:aliases_cst)
    ~parse_result:aliases_parse_result
    ~cst:aliases_cst in
  let colors_text = "let answer = Helper.twice 21\n" in
  let colors_parse_result = Syn.parse ~filename:(Path.v "colors.ml") colors_text in
  let colors_cst = expect_cst ~filename:"colors.ml" colors_parse_result in
  let implicit_opens = [ SurfacePath.of_string "Colors__Aliases" ] in
  let (session, colors_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~module_name:"Colors"
    ~implicit_opens
    ~origin:(Source.Label "colors.ml")
    ~source_hash:(Source.hash ~implicit_opens ~cst:colors_cst)
    ~parse_result:colors_parse_result
    ~cst:colors_cst in
  match prepare_snapshot_or_error session ~roots:[ colors_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot colors_source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        let answer_type = export_scheme snapshot colors_source_id "answer" in
        if not (answer_type = Some "int") then
          Error (format Format.[ str "unexpected answer type: "; str (show_option answer_type); ])
        else
          Ok ()

let test_prepare_snapshot_resolves_internal_module_dependencies_by_local_alias = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let int_text = "let hash value = value\n" in
  let int_parse_result = Syn.parse ~filename:(Path.v "int.ml") int_text in
  let int_cst = expect_cst ~filename:"int.ml" int_parse_result in
  let (session, int_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~module_name:"Pkg__Int"
    ~implicit_opens:[]
    ~origin:(Source.Label "int.ml")
    ~source_hash:(Source.hash ~implicit_opens:[] ~cst:int_cst)
    ~parse_result:int_parse_result
    ~cst:int_cst in
  let session = Session.register_source_alias session int_source_id ~module_name:"Int" in
  let token_text = "let value = Int.hash 1\n" in
  let token_parse_result = Syn.parse ~filename:(Path.v "token.ml") token_text in
  let token_cst = expect_cst ~filename:"token.ml" token_parse_result in
  let (session, token_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~module_name:"Pkg__Token"
    ~implicit_opens:[]
    ~origin:(Source.Label "token.ml")
    ~source_hash:(Source.hash ~implicit_opens:[] ~cst:token_cst)
    ~parse_result:token_parse_result
    ~cst:token_cst in
  match prepare_snapshot_or_error session ~roots:[ token_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot token_source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        let value_type = export_scheme snapshot token_source_id "value" in
        if value_type = Some "int" then
          Ok ()
        else
          Error (format Format.[ str "unexpected value type: "; str (show_option value_type); ])

let test_prepare_snapshot_prefers_internal_local_alias_dependencies_over_loaded_modules = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let unix_text = {ocaml|
    type t = string

    let of_string value = value

    let to_string value = value
  |ocaml}
  in
  let unix_parse_result = Syn.parse ~filename:(Path.v "unix.ml") unix_text in
  let unix_cst = expect_cst ~filename:"unix.ml" unix_parse_result in
  let (session, unix_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~module_name:"Ip_addr__Unix"
    ~implicit_opens:[]
    ~origin:(Source.Label "unix.ml")
    ~source_hash:(Source.hash ~implicit_opens:[] ~cst:unix_cst)
    ~parse_result:unix_parse_result
    ~cst:unix_cst in
  let (session, impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "ip_addr.ml")
    ~text:{ocaml|
      include Unix
    |ocaml}
  in
  let (session, intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "ip_addr.mli")
    ~text:{ocaml|
      type t

      val of_string : string -> t

      val to_string : t -> string
    |ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ impl_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let impl_diagnostics = diagnostic_strings snapshot impl_source_id in
      let intf_diagnostics = diagnostic_strings snapshot intf_source_id in
      if not (List.is_empty impl_diagnostics) then
        Error (String.concat "\n" impl_diagnostics)
      else if not (List.is_empty intf_diagnostics) then
        Error (String.concat "\n" intf_diagnostics)
      else
        let local_unix_typings = Session.Snapshot.find_module_typings_by_name snapshot "Ip_addr__Unix" in
        let of_string_type = export_scheme snapshot impl_source_id "of_string" in
        let to_string_type = export_scheme snapshot impl_source_id "to_string" in
        if has_signature_error snapshot impl_source_id then
          Error "expected implementation signature inclusion to succeed"
        else if has_signature_error snapshot intf_source_id then
          Error "expected interface signature inclusion to succeed"
        else if Option.is_none local_unix_typings then
          Error "expected internal Unix sibling to be included in the rooted snapshot"
        else if not (of_string_type = Some "string -> t") then
          Error (format
            Format.[ str "unexpected of_string type: "; str (show_option of_string_type); ])
        else if not (to_string_type = Some "t -> string") then
          Error (format
            Format.[ str "unexpected to_string type: "; str (show_option to_string_type); ])
        else
          Ok ()

let test_prepare_snapshot_uses_internal_local_alias_dependencies_transitively = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let unix_text = {ocaml|
    type t = string

    let of_string value = value

    let to_string value = value
  |ocaml}
  in
  let unix_parse_result = Syn.parse ~filename:(Path.v "unix.ml") unix_text in
  let unix_cst = expect_cst ~filename:"unix.ml" unix_parse_result in
  let (session, _unix_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~module_name:"Ip_addr__Unix"
    ~implicit_opens:[]
    ~origin:(Source.Label "unix.ml")
    ~source_hash:(Source.hash ~implicit_opens:[] ~cst:unix_cst)
    ~parse_result:unix_parse_result
    ~cst:unix_cst in
  let (session, _ip_addr_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "ip_addr.ml")
    ~text:{ocaml|
      include Unix
    |ocaml}
  in
  let (session, _ip_addr_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "ip_addr.mli")
    ~text:{ocaml|
      type t

      val of_string : string -> t

      val to_string : t -> string
    |ocaml}
  in
  let (session, _socket_addr_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "socket_addr.ml")
    ~text:{ocaml|
      let render value = Ip_addr.to_string value
    |ocaml}
  in
  let (session, _socket_addr_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "socket_addr.mli")
    ~text:{ocaml|
      val render : Ip_addr.t -> string
    |ocaml}
  in
  let (session, app_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "app.ml")
    ~text:{ocaml|
      let render value = Socket_addr.render value
    |ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ app_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot app_source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        let render_type = export_scheme snapshot app_source_id "render" in
        if not (render_type = Some "Ip_addr.t -> string") then
          Error (format Format.[ str "unexpected render type: "; str (show_option render_type); ])
        else
          Ok ()

let test_prepare_snapshot_internal_local_alias_dependencies_ignore_source_order = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _ip_addr_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "ip_addr.ml")
    ~text:{ocaml|
      include Unix
    |ocaml}
  in
  let (session, _ip_addr_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "ip_addr.mli")
    ~text:{ocaml|
      type t

      val of_string : string -> t

      val to_string : t -> string
    |ocaml}
  in
  let (session, _socket_addr_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "socket_addr.ml")
    ~text:{ocaml|
      let render value = Ip_addr.to_string value
    |ocaml}
  in
  let (session, _socket_addr_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "socket_addr.mli")
    ~text:{ocaml|
      val render : Ip_addr.t -> string
    |ocaml}
  in
  let (session, app_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "app.ml")
    ~text:{ocaml|
      let render value = Socket_addr.render value
    |ocaml}
  in
  let unix_text = {ocaml|
    type t = string

    let of_string value = value

    let to_string value = value
  |ocaml}
  in
  let unix_parse_result = Syn.parse ~filename:(Path.v "unix.ml") unix_text in
  let unix_cst = expect_cst ~filename:"unix.ml" unix_parse_result in
  let (session, _unix_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~module_name:"Ip_addr__Unix"
    ~implicit_opens:[]
    ~origin:(Source.Label "unix.ml")
    ~source_hash:(Source.hash ~implicit_opens:[] ~cst:unix_cst)
    ~parse_result:unix_parse_result
    ~cst:unix_cst in
  match prepare_snapshot_or_error session ~roots:[ app_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot app_source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        let render_type = export_scheme snapshot app_source_id "render" in
        if not (render_type = Some "Ip_addr.t -> string") then
          Error (format Format.[ str "unexpected render type: "; str (show_option render_type); ])
        else
          Ok ()

let test_prepare_snapshot_nested_internal_local_alias_dependencies_typecheck = fun _ctx ->
  let create_named_source session ~module_name ~filename ~text =
    let parse_result = Syn.parse ~filename:(Path.v filename) text in
    let cst = expect_cst ~filename parse_result in
    Session.create_source
      session
      ~kind:Source.File
      ~module_name
      ~implicit_opens:[]
      ~origin:(Source.Label filename)
      ~source_hash:(Source.hash ~implicit_opens:[] ~cst)
      ~parse_result
      ~cst
  in
  let session = Session.empty ~config:Config.default in
  let (session, _ip_addr_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Ip_addr" ~filename:"ip_addr.ml"
    ~text:{ocaml|
      include Unix
    |ocaml}
  in
  let (session, _ip_addr_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Ip_addr" ~filename:"ip_addr.mli"
    ~text:{ocaml|
      type t

      val of_string : string -> t

      val to_string : t -> string
    |ocaml}
  in
  let (session, _socket_addr_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Socket_addr" ~filename:"socket_addr.ml"
    ~text:{ocaml|
      let render value = Ip_addr.to_string value
    |ocaml}
  in
  let (session, _socket_addr_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Socket_addr" ~filename:"socket_addr.mli"
    ~text:{ocaml|
      val render : Ip_addr.t -> string
    |ocaml}
  in
  let (session, app_source_id) = create_named_source session ~module_name:"Kernel_new" ~filename:"kernel_new.ml"
    ~text:{ocaml|
      let render value = Socket_addr.render value
    |ocaml}
  in
  let (session, _unix_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Ip_addr__Unix" ~filename:"unix.ml"
    ~text:{ocaml|
      type t = string

      let of_string value = value

      let to_string value = value
    |ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ app_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot app_source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        let render_type = export_scheme snapshot app_source_id "render" in
        if not (render_type = Some "Ip_addr.t -> string") then
          Error (format Format.[ str "unexpected render type: "; str (show_option render_type); ])
        else
          Ok ()

let test_prepare_snapshot_nested_unix_submodule_sees_sibling_ip_addr_exports = fun _ctx ->
  let module_name_suffix_aliases module_name =
    let segments = module_name
    |> String.split_on_char '.'
    |> List.filter (fun segment -> not (String.equal segment "")) in
    let rec loop aliases = function
      | [] -> List.rev aliases
      | _ :: rest as current -> loop (String.concat "." current :: aliases) rest
    in
    loop [] segments |> List.sort_uniq String.compare
  in
  let create_named_source session ~module_name ~filename ~text =
    let parse_result = Syn.parse ~filename:(Path.v filename) text in
    let cst = expect_cst ~filename parse_result in
    Session.create_source
      session
      ~kind:Source.File
      ~module_name
      ~implicit_opens:[]
      ~origin:(Source.Label filename)
      ~source_hash:(Source.hash ~implicit_opens:[] ~cst)
      ~parse_result
      ~cst
  in
  let register_local_aliases session source_id local_module_name = module_name_suffix_aliases local_module_name
  |> List.fold_left
    (fun session module_name -> Session.register_source_alias session source_id ~module_name)
    session in
  let session = Session.empty ~config:Config.default in
  let (session, net_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net" ~filename:"net.mli"
    ~text:{ocaml|
        module IpAddr = Ip_addr

        module SocketAddr = Socket_addr

        module TcpStream = Tcp_stream
      |ocaml}
  in
  let (session, net_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net" ~filename:"net.ml"
    ~text:{ocaml|
        module IpAddr = Ip_addr

        module SocketAddr = Socket_addr

        module TcpStream = Tcp_stream
      |ocaml}
  in
  let (session, ip_addr_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Ip_addr" ~filename:"ip_addr.mli"
    ~text:{ocaml|
        type t

        val of_string : string -> t

        val to_string : t -> string
      |ocaml}
  in
  let session = register_local_aliases session ip_addr_intf_source_id "Net.Ip_addr" in
  let (session, unix_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Ip_addr__Unix" ~filename:"unix.ml"
    ~text:{ocaml|
        type t = string

        let of_string value = value

        let to_string value = value
      |ocaml}
  in
  let session = register_local_aliases session unix_source_id "Net.Ip_addr.Unix" in
  let (session, tcp_listener_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Tcp_listener__Unix" ~filename:"tcp_listener_unix.ml"
    ~text:{ocaml|
        let parse host = Ip_addr.of_string host

        let render addr = Ip_addr.to_string addr
      |ocaml}
  in
  let session = register_local_aliases session tcp_listener_impl_source_id "Net.Tcp_listener.Unix" in
  let (session, tcp_listener_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Tcp_listener__Unix" ~filename:"tcp_listener_unix.mli"
    ~text:{ocaml|
        val parse : string -> Ip_addr.t

        val render : Ip_addr.t -> string
      |ocaml}
  in
  let session = register_local_aliases session tcp_listener_intf_source_id "Net.Tcp_listener.Unix" in
  match prepare_snapshot_or_error session ~roots:[ tcp_listener_impl_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot tcp_listener_impl_source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        let parse_type = export_scheme snapshot tcp_listener_impl_source_id "parse" in
        let render_type = export_scheme snapshot tcp_listener_impl_source_id "render" in
        if not (parse_type = Some "string -> Ip_addr.t") then
          Error (format Format.[ str "unexpected parse type: "; str (show_option parse_type); ])
        else if not (render_type = Some "Ip_addr.t -> string") then
          Error (format Format.[ str "unexpected render type: "; str (show_option render_type); ])
        else
          Ok ()

let test_prepare_snapshot_wrapper_module_reexports_unix_exports_to_sibling_modules = fun _ctx ->
  let module_name_suffix_aliases module_name =
    let segments = module_name
    |> String.split_on_char '.'
    |> List.filter (fun segment -> not (String.equal segment "")) in
    let rec loop aliases = function
      | [] -> List.rev aliases
      | _ :: rest as current -> loop (String.concat "." current :: aliases) rest
    in
    loop [] segments |> List.sort_uniq String.compare
  in
  let create_named_source session ~module_name ~filename ~text =
    let parse_result = Syn.parse ~filename:(Path.v filename) text in
    let cst = expect_cst ~filename parse_result in
    Session.create_source
      session
      ~kind:Source.File
      ~module_name
      ~implicit_opens:[]
      ~origin:(Source.Label filename)
      ~source_hash:(Source.hash ~implicit_opens:[] ~cst)
      ~parse_result
      ~cst
  in
  let register_local_aliases session source_id local_module_name = module_name_suffix_aliases local_module_name
  |> List.fold_left
    (fun session module_name -> Session.register_source_alias session source_id ~module_name)
    session in
  let session = Session.empty ~config:Config.default in
  let (session, ip_addr_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Ip_addr" ~filename:"ip_addr.ml"
    ~text:{ocaml|
        include Unix
      |ocaml}
  in
  let session = register_local_aliases session ip_addr_impl_source_id "Net.Ip_addr" in
  let (session, ip_addr_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Ip_addr" ~filename:"ip_addr.mli"
    ~text:{ocaml|
        type t
        type error =
          | InvalidText of { value: string }

        val v4_loopback : t
        val v6_loopback : t
        val of_string : string -> t
        val to_string : t -> string
      |ocaml}
  in
  let session = register_local_aliases session ip_addr_intf_source_id "Net.Ip_addr" in
  let (session, ip_addr_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Ip_addr" ~filename:"ip_addr.ml"
    ~text:{ocaml|
        include Unix
      |ocaml}
  in
  let session = register_local_aliases session ip_addr_impl_source_id "Net.Ip_addr" in
  let (session, ip_addr_unix_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Ip_addr__Unix" ~filename:"ip_addr_unix.ml"
    ~text:{ocaml|
        type t = string
        type error =
          | InvalidText of { value: string }

        let v4_loopback = "127.0.0.1"
        let v6_loopback = "::1"

        let of_string value = value
        let to_string value = value
      |ocaml}
  in
  let session = register_local_aliases session ip_addr_unix_source_id "Net.Ip_addr.Unix" in
  let (session, socket_addr_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Socket_addr" ~filename:"socket_addr.mli"
    ~text:{ocaml|
        type t
        val loopback_v4 : port:int -> t
        val loopback_v6 : port:int -> t
        val to_string : t -> string
      |ocaml}
  in
  let session = register_local_aliases session socket_addr_intf_source_id "Net.Socket_addr" in
  let (session, socket_addr_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Socket_addr" ~filename:"socket_addr.ml"
    ~text:{ocaml|
        type t = {
          ip : Ip_addr.t;
          port : int;
        }

        let loopback_v4 ~port = { ip = Ip_addr.v4_loopback; port }

        let loopback_v6 ~port = { ip = Ip_addr.v6_loopback; port }

        let to_string value = Ip_addr.to_string value.ip
      |ocaml}
  in
  let session = register_local_aliases session socket_addr_impl_source_id "Net.Socket_addr" in
  let roots = [
    ip_addr_intf_source_id;
    ip_addr_impl_source_id;
    ip_addr_unix_source_id;
    socket_addr_intf_source_id;
    socket_addr_impl_source_id;
  ] in
  match prepare_snapshot_or_error session ~roots with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot socket_addr_impl_source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        Ok ()

let test_prepare_snapshot_wrapper_module_preserves_same_path_nominal_value_types = fun _ctx ->
  let module_name_suffix_aliases module_name =
    let segments = module_name
    |> String.split_on_char '.'
    |> List.filter (fun segment -> not (String.equal segment "")) in
    let rec loop aliases = function
      | [] -> List.rev aliases
      | _ :: rest as current -> loop (String.concat "." current :: aliases) rest
    in
    loop [] segments |> List.sort_uniq String.compare
  in
  let create_named_source session ~module_name ~filename ~text =
    let parse_result = Syn.parse ~filename:(Path.v filename) text in
    let cst = expect_cst ~filename parse_result in
    Session.create_source
      session
      ~kind:Source.File
      ~module_name
      ~implicit_opens:[]
      ~origin:(Source.Label filename)
      ~source_hash:(Source.hash ~implicit_opens:[] ~cst)
      ~parse_result
      ~cst
  in
  let register_local_aliases session source_id local_module_name = module_name_suffix_aliases local_module_name
  |> List.fold_left
    (fun session module_name -> Session.register_source_alias session source_id ~module_name)
    session in
  let session = Session.empty ~config:Config.default in
  let (session, foo_intf_source_id) = create_named_source session ~module_name:"Foo" ~filename:"foo.mli"
    ~text:{ocaml|
        type t
        type error = InvalidText of string

        val use : t -> error -> unit
      |ocaml}
  in
  let (session, foo_impl_source_id) = create_named_source session ~module_name:"Foo" ~filename:"foo.ml"
    ~text:{ocaml|
        include Unix
      |ocaml}
  in
  let (session, foo_unix_intf_source_id) = create_named_source session ~module_name:"Foo__Unix" ~filename:"foo_unix.mli"
    ~text:{ocaml|
        type t = string
        type error = InvalidText of string

        val use : t -> error -> unit
      |ocaml}
  in
  let session = register_local_aliases session foo_unix_intf_source_id "Foo.Unix" in
  let (session, foo_unix_impl_source_id) = create_named_source session ~module_name:"Foo__Unix" ~filename:"foo_unix.ml"
    ~text:{ocaml|
        type t = string
        type error = InvalidText of string

        let use _ _ = ()
      |ocaml}
  in
  let session = register_local_aliases session foo_unix_impl_source_id "Foo.Unix" in
  match prepare_snapshot_or_error
    session
    ~roots:[
      foo_intf_source_id;
      foo_impl_source_id;
      foo_unix_intf_source_id;
      foo_unix_impl_source_id
    ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let foo_intf_diagnostics = diagnostic_strings snapshot foo_intf_source_id in
      let foo_impl_diagnostics = diagnostic_strings snapshot foo_impl_source_id in
      let foo_exports =
        match Session.Snapshot.find_module_typings_by_name snapshot "Foo" with
        | Some typings -> module_typings_export_names typings |> String.concat ", "
        | None -> "<missing>"
      in
      if not (List.is_empty foo_intf_diagnostics) then
        Error (format
          Format.[
            str "foo.mli diagnostics:\n";
            str (String.concat "\n" foo_intf_diagnostics);
            str "\nfoo exports: ";
            str foo_exports;
          ])
      else if not (List.is_empty foo_impl_diagnostics) then
        Error (format
          Format.[
            str "foo.ml diagnostics:\n";
            str (String.concat "\n" foo_impl_diagnostics);
            str "\nfoo exports: ";
            str foo_exports;
          ])
      else
        Ok ()

let test_prepare_snapshot_wrapper_module_preserves_local_result_error_surface = fun _ctx ->
  let module_name_suffix_aliases module_name =
    let segments = module_name
    |> String.split_on_char '.'
    |> List.filter (fun segment -> not (String.equal segment "")) in
    let rec loop aliases = function
      | [] -> List.rev aliases
      | _ :: rest as current -> loop (String.concat "." current :: aliases) rest
    in
    loop [] segments |> List.sort_uniq String.compare
  in
  let create_named_source session ~module_name ~filename ~text =
    let parse_result = Syn.parse ~filename:(Path.v filename) text in
    let cst = expect_cst ~filename parse_result in
    Session.create_source
      session
      ~kind:Source.File
      ~module_name
      ~implicit_opens:[]
      ~origin:(Source.Label filename)
      ~source_hash:(Source.hash ~implicit_opens:[] ~cst)
      ~parse_result
      ~cst
  in
  let register_local_aliases session source_id local_module_name = module_name_suffix_aliases local_module_name
  |> List.fold_left
    (fun session module_name -> Session.register_source_alias session source_id ~module_name)
    session in
  let session = Session.empty ~config:Config.default in
  let (session, result_intf_source_id) = create_named_source session ~module_name:"Result" ~filename:"result.mli"
    ~text:{ocaml|
        type ('ok, 'error) t =
          | Ok of 'ok
          | Error of 'error
      |ocaml}
  in
  let (session, result_impl_source_id) = create_named_source session ~module_name:"Result" ~filename:"result.ml"
    ~text:{ocaml|
        type ('ok, 'error) t =
          | Ok of 'ok
          | Error of 'error
      |ocaml}
  in
  let (session, ip_addr_intf_source_id) = create_named_source session ~module_name:"Ip_addr" ~filename:"ip_addr.mli"
    ~text:{ocaml|
        type t
        type error =
          | InvalidText of { value: string }

        val error_to_string : error -> string
        val v4_loopback : t
        val v6_loopback : t
        val of_string : string -> (t, error) Result.t
        val to_string : t -> string
        val compare : t -> t -> int
        val equal : t -> t -> bool
      |ocaml}
  in
  let (session, ip_addr_impl_source_id) = create_named_source session ~module_name:"Ip_addr" ~filename:"ip_addr.ml"
    ~text:{ocaml|
        include Unix
      |ocaml}
  in
  let (session, ip_addr_unix_intf_source_id) = create_named_source session ~module_name:"Ip_addr__Unix" ~filename:"ip_addr_unix.mli"
    ~text:{ocaml|
        type t
        type error =
          | InvalidText of { value: string }

        val error_to_string : error -> string
        val v4_loopback : t
        val v6_loopback : t
        val of_string : string -> (t, error) Result.t
        val to_string : t -> string
        val compare : t -> t -> int
        val equal : t -> t -> bool
      |ocaml}
  in
  let session = register_local_aliases session ip_addr_unix_intf_source_id "Ip_addr.Unix" in
  let (session, ip_addr_unix_impl_source_id) = create_named_source session ~module_name:"Ip_addr__Unix" ~filename:"ip_addr_unix.ml"
    ~text:{ocaml|
        type t = string

        type error =
          | InvalidText of { value: string }

        let error_to_string _ = ""
        let v4_loopback = "127.0.0.1"
        let v6_loopback = "::1"
        let of_string value = Result.Ok value
        let to_string value = value
        let compare = String.compare
        let equal = String.equal
      |ocaml}
  in
  let session = register_local_aliases session ip_addr_unix_impl_source_id "Ip_addr.Unix" in
  match prepare_snapshot_or_error
    session
    ~roots:[
      result_intf_source_id;
      result_impl_source_id;
      ip_addr_intf_source_id;
      ip_addr_impl_source_id;
      ip_addr_unix_intf_source_id;
      ip_addr_unix_impl_source_id;
    ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let ip_addr_intf_diagnostics = diagnostic_strings snapshot ip_addr_intf_source_id in
      let ip_addr_impl_diagnostics = diagnostic_strings snapshot ip_addr_impl_source_id in
      let ip_addr_exports =
        match Session.Snapshot.find_module_typings_by_name snapshot "Ip_addr" with
        | Some typings -> module_typings_export_names typings |> String.concat ", "
        | None -> "<missing>"
      in
      if not (List.is_empty ip_addr_intf_diagnostics) then
        Error (format
          Format.[
            str "ip_addr.mli diagnostics:\n";
            str (String.concat "\n" ip_addr_intf_diagnostics);
            str "\nip_addr exports: ";
            str ip_addr_exports;
          ])
      else if not (List.is_empty ip_addr_impl_diagnostics) then
        Error (format
          Format.[
            str "ip_addr.ml diagnostics:\n";
            str (String.concat "\n" ip_addr_impl_diagnostics);
            str "\nip_addr exports: ";
            str ip_addr_exports;
          ])
      else
        Ok ()

let test_prepare_snapshot_nested_module_alias_canonicalizes_sibling_error_types = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _tcp_listener_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "tcp_listener.mli")
    ~text:{ocaml|
        type error =
          | System of int

        val error_to_string : error -> string
      |ocaml}
  in
  let (session, _tcp_listener_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "tcp_listener.ml")
    ~text:{ocaml|
        type error =
          | System of int

        let error_to_string _ = ""
      |ocaml}
  in
  let (session, _net_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "net.mli")
    ~text:{ocaml|
        module TcpListener = Tcp_listener
      |ocaml}
  in
  let (session, _net_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "net.ml")
    ~text:{ocaml|
        module TcpListener = Tcp_listener
      |ocaml}
  in
  let (session, error_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "error.mli")
    ~text:{ocaml|
        type t =
          | NetTcpListener of Net.TcpListener.error

        val of_net_tcp_listener : Net.TcpListener.error -> t

        val detail_to_string : t -> string
      |ocaml}
  in
  let (session, error_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "error.ml")
    ~text:{ocaml|
        type t =
          | NetTcpListener of Net.TcpListener.error

        let of_net_tcp_listener = fun error -> NetTcpListener error

        let detail_to_string = fun value ->
          match value with
          | NetTcpListener error -> Net.TcpListener.error_to_string error
      |ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ error_intf_source_id; error_impl_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let error_intf_diagnostics = diagnostic_strings snapshot error_intf_source_id in
      let error_impl_diagnostics = diagnostic_strings snapshot error_impl_source_id in
      let net_error_to_string_type =
        match Session.Snapshot.find_module_typings_by_name snapshot "Net" with
        | Some typings -> ModuleTypings.exports typings
        |> lookup_export "TcpListener.error_to_string"
        |> Option.map TypePrinter.scheme_to_string
        | None -> None
      in
      let net_type_names =
        match Session.Snapshot.find_module_typings_by_name snapshot "Net" with
        | Some typings -> ModuleTypings.type_decls typings
        |> List.map
          (fun (type_decl: FileSummary.type_decl) ->
            SurfacePath.append_name type_decl.scope_path type_decl.declaration.type_name
            |> SurfacePath.to_string)
        | None -> []
      in
      if not (List.is_empty error_intf_diagnostics) then
        Error (format
          Format.[
            str (String.concat "\n" error_intf_diagnostics);
            str "\nNet.TcpListener.error_to_string = ";
            str (show_option net_error_to_string_type);
            str "\nNet type decls = ";
            str (String.concat ", " net_type_names);
          ])
      else if not (List.is_empty error_impl_diagnostics) then
        Error (format
          Format.[
            str (String.concat "\n" error_impl_diagnostics);
            str "\nNet.TcpListener.error_to_string = ";
            str (show_option net_error_to_string_type);
            str "\nNet type decls = ";
            str (String.concat ", " net_type_names);
          ])
      else
        Ok ()

let test_prepare_snapshot_kernel_named_wrapper_alias_preserves_nested_error_types = fun _ctx ->
  let module_name_suffix_aliases module_name =
    let segments = module_name
    |> String.split_on_char '.'
    |> List.filter (fun segment -> not (String.equal segment "")) in
    let rec loop aliases = function
      | [] -> List.rev aliases
      | _ :: rest as current -> loop (String.concat "." current :: aliases) rest
    in
    loop [] segments |> List.sort_uniq String.compare
  in
  let create_named_source session ~module_name ~filename ~text =
    let parse_result = Syn.parse ~filename:(Path.v filename) text in
    let cst = expect_cst ~filename parse_result in
    Session.create_source
      session
      ~kind:Source.File
      ~module_name
      ~implicit_opens:[]
      ~origin:(Source.Label filename)
      ~source_hash:(Source.hash ~implicit_opens:[] ~cst)
      ~parse_result
      ~cst
  in
  let register_local_aliases session source_id local_module_name = module_name_suffix_aliases local_module_name
  |> List.fold_left
    (fun session module_name -> Session.register_source_alias session source_id ~module_name)
    session in
  let session = Session.empty ~config:Config.default in
  let (session, read_dir_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Fs__Read_dir" ~filename:"read_dir.mli"
    ~text:{ocaml|
        type error =
          | File of int

        val error_to_string : error -> string
      |ocaml}
  in
  let session = register_local_aliases session read_dir_intf_source_id "Fs.ReadDir" in
  let (session, read_dir_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Fs__Read_dir" ~filename:"read_dir.ml"
    ~text:{ocaml|
        type error =
          | File of int

        let error_to_string _ = ""
      |ocaml}
  in
  let session = register_local_aliases session read_dir_impl_source_id "Fs.ReadDir" in
  let (session, fs_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Fs" ~filename:"fs.mli"
    ~text:{ocaml|
        module ReadDir = Read_dir
      |ocaml}
  in
  let session = register_local_aliases session fs_intf_source_id "Fs" in
  let (session, fs_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Fs" ~filename:"fs.ml"
    ~text:{ocaml|
        module ReadDir = Read_dir
      |ocaml}
  in
  let session = register_local_aliases session fs_impl_source_id "Fs" in
  let (session, error_source_id) = create_named_source session ~module_name:"Kernel_new__Error" ~filename:"error.ml"
    ~text:{ocaml|
        type t =
          | FsReadDir of Fs.ReadDir.error

        let of_fs_read_dir = fun error -> FsReadDir error

        let detail_to_string = fun value ->
          match value with
          | FsReadDir error -> Fs.ReadDir.error_to_string error
      |ocaml}
  in
  match prepare_snapshot_or_error
    session
    ~roots:[
      read_dir_intf_source_id;
      read_dir_impl_source_id;
      fs_intf_source_id;
      fs_impl_source_id;
      error_source_id;
    ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot error_source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        let of_fs_read_dir_type = export_scheme snapshot error_source_id "of_fs_read_dir" in
        if of_fs_read_dir_type = Some "Fs.ReadDir.error -> t" then
          Ok ()
        else
          Error ("unexpected of_fs_read_dir type: " ^ show_option of_fs_read_dir_type)

let test_prepare_snapshot_multiple_wrapper_aliases_preserve_nested_error_types = fun _ctx ->
  let module_name_suffix_aliases module_name =
    let segments = module_name
    |> String.split_on_char '.'
    |> List.filter (fun segment -> not (String.equal segment "")) in
    let rec loop aliases = function
      | [] -> List.rev aliases
      | _ :: rest as current -> loop (String.concat "." current :: aliases) rest
    in
    loop [] segments |> List.sort_uniq String.compare
  in
  let create_named_source session ~module_name ~filename ~text =
    let parse_result = Syn.parse ~filename:(Path.v filename) text in
    let cst = expect_cst ~filename parse_result in
    Session.create_source
      session
      ~kind:Source.File
      ~module_name
      ~implicit_opens:[]
      ~origin:(Source.Label filename)
      ~source_hash:(Source.hash ~implicit_opens:[] ~cst)
      ~parse_result
      ~cst
  in
  let register_local_aliases session source_id local_module_name = module_name_suffix_aliases local_module_name
  |> List.fold_left
    (fun session module_name -> Session.register_source_alias session source_id ~module_name)
    session in
  let session = Session.empty ~config:Config.default in
  let (session, read_dir_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Fs__Read_dir" ~filename:"read_dir.mli"
    ~text:{ocaml|
        type error =
          | File of int
      |ocaml}
  in
  let session = register_local_aliases session read_dir_intf_source_id "Fs.ReadDir" in
  let (session, read_dir_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Fs__Read_dir" ~filename:"read_dir.ml"
    ~text:{ocaml|
        type error =
          | File of int
      |ocaml}
  in
  let session = register_local_aliases session read_dir_impl_source_id "Fs.ReadDir" in
  let (session, fs_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Fs" ~filename:"fs.mli"
    ~text:{ocaml|
        module ReadDir = Read_dir
      |ocaml}
  in
  let session = register_local_aliases session fs_intf_source_id "Fs" in
  let (session, fs_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Fs" ~filename:"fs.ml"
    ~text:{ocaml|
        module ReadDir = Read_dir
      |ocaml}
  in
  let session = register_local_aliases session fs_impl_source_id "Fs" in
  let (session, tcp_listener_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Tcp_listener" ~filename:"tcp_listener.mli"
    ~text:{ocaml|
        type error =
          | System of int
      |ocaml}
  in
  let session = register_local_aliases session tcp_listener_intf_source_id "Net.TcpListener" in
  let (session, tcp_listener_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Tcp_listener" ~filename:"tcp_listener.ml"
    ~text:{ocaml|
        type error =
          | System of int
      |ocaml}
  in
  let session = register_local_aliases session tcp_listener_impl_source_id "Net.TcpListener" in
  let (session, net_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net" ~filename:"net.mli"
    ~text:{ocaml|
        module TcpListener = Tcp_listener
      |ocaml}
  in
  let session = register_local_aliases session net_intf_source_id "Net" in
  let (session, net_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net" ~filename:"net.ml"
    ~text:{ocaml|
        module TcpListener = Tcp_listener
      |ocaml}
  in
  let session = register_local_aliases session net_impl_source_id "Net" in
  let (session, system_time_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Time__System_time" ~filename:"system_time.mli"
    ~text:{ocaml|
        type error =
          | System of int
      |ocaml}
  in
  let session = register_local_aliases session system_time_intf_source_id "Time.SystemTime" in
  let (session, system_time_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Time__System_time" ~filename:"system_time.ml"
    ~text:{ocaml|
        type error =
          | System of int
      |ocaml}
  in
  let session = register_local_aliases session system_time_impl_source_id "Time.SystemTime" in
  let (session, time_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Time" ~filename:"time.mli"
    ~text:{ocaml|
        module SystemTime = System_time
      |ocaml}
  in
  let session = register_local_aliases session time_intf_source_id "Time" in
  let (session, time_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Time" ~filename:"time.ml"
    ~text:{ocaml|
        module SystemTime = System_time
      |ocaml}
  in
  let session = register_local_aliases session time_impl_source_id "Time" in
  let (session, error_source_id) = create_named_source session ~module_name:"Kernel_new__Error" ~filename:"error.ml"
    ~text:{ocaml|
        type t =
          | FsReadDir of Fs.ReadDir.error
          | NetTcpListener of Net.TcpListener.error
          | TimeSystemTime of Time.SystemTime.error

        let of_fs_read_dir = fun error -> FsReadDir error

        let of_net_tcp_listener = fun error -> NetTcpListener error

        let of_time_system_time = fun error -> TimeSystemTime error
      |ocaml}
  in
  match
    prepare_snapshot_or_error session
      ~roots:[
        read_dir_intf_source_id;
        read_dir_impl_source_id;
        fs_intf_source_id;
        fs_impl_source_id;
        tcp_listener_intf_source_id;
        tcp_listener_impl_source_id;
        net_intf_source_id;
        net_impl_source_id;
        system_time_intf_source_id;
        system_time_impl_source_id;
        time_intf_source_id;
        time_impl_source_id;
        error_source_id;
      ]
  with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot error_source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        let of_fs_read_dir_type = export_scheme snapshot error_source_id "of_fs_read_dir" in
        let of_net_tcp_listener_type = export_scheme snapshot error_source_id "of_net_tcp_listener" in
        let of_time_system_time_type = export_scheme snapshot error_source_id "of_time_system_time" in
        if
          of_fs_read_dir_type = Some "Fs.ReadDir.error -> t"
          && of_net_tcp_listener_type = Some "Net.TcpListener.error -> t"
          && of_time_system_time_type = Some "Time.SystemTime.error -> t"
        then
          Ok ()
        else
          Error (format
            Format.[
              str "unexpected error helper exports: of_fs_read_dir=";
              str (show_option of_fs_read_dir_type);
              str ", of_net_tcp_listener=";
              str (show_option of_net_tcp_listener_type);
              str ", of_time_system_time=";
              str (show_option of_time_system_time_type);
            ])

let test_prepare_snapshot_nested_include_wrapper_alias_preserves_error_types = fun _ctx ->
  let module_name_suffix_aliases module_name =
    let segments = module_name
    |> String.split_on_char '.'
    |> List.filter (fun segment -> not (String.equal segment "")) in
    let rec loop aliases = function
      | [] -> List.rev aliases
      | _ :: rest as current -> loop (String.concat "." current :: aliases) rest
    in
    loop [] segments |> List.sort_uniq String.compare
  in
  let create_named_source session ~module_name ~filename ~text =
    let parse_result = Syn.parse ~filename:(Path.v filename) text in
    let cst = expect_cst ~filename parse_result in
    Session.create_source
      session
      ~kind:Source.File
      ~module_name
      ~implicit_opens:[]
      ~origin:(Source.Label filename)
      ~source_hash:(Source.hash ~implicit_opens:[] ~cst)
      ~parse_result
      ~cst
  in
  let register_local_aliases session source_id local_module_name = module_name_suffix_aliases local_module_name
  |> List.fold_left
    (fun session module_name -> Session.register_source_alias session source_id ~module_name)
    session in
  let session = Session.empty ~config:Config.default in
  let (session, unix_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Tcp_listener__Unix" ~filename:"tcp_listener_unix.mli"
    ~text:{ocaml|
        type error =
          | System of int

        val error_to_string : error -> string
      |ocaml}
  in
  let session = register_local_aliases session unix_intf_source_id "Net.TcpListener.Unix" in
  let (session, unix_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Tcp_listener__Unix" ~filename:"tcp_listener_unix.ml"
    ~text:{ocaml|
        type error =
          | System of int

        let error_to_string _ = ""
      |ocaml}
  in
  let session = register_local_aliases session unix_impl_source_id "Net.TcpListener.Unix" in
  let (session, tcp_listener_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Tcp_listener" ~filename:"tcp_listener.mli"
    ~text:{ocaml|
        type error =
          | System of int

        val error_to_string : error -> string
      |ocaml}
  in
  let session = register_local_aliases session tcp_listener_intf_source_id "Net.TcpListener" in
  let (session, tcp_listener_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Tcp_listener" ~filename:"tcp_listener.ml"
    ~text:{ocaml|
        include Unix
      |ocaml}
  in
  let session = register_local_aliases session tcp_listener_impl_source_id "Net.TcpListener" in
  let (session, net_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net" ~filename:"net.mli"
    ~text:{ocaml|
        module TcpListener = Tcp_listener
      |ocaml}
  in
  let session = register_local_aliases session net_intf_source_id "Net" in
  let (session, net_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net" ~filename:"net.ml"
    ~text:{ocaml|
        module TcpListener = Tcp_listener
      |ocaml}
  in
  let session = register_local_aliases session net_impl_source_id "Net" in
  let (session, error_source_id) = create_named_source session ~module_name:"Kernel_new__Error" ~filename:"error.ml"
    ~text:{ocaml|
        type t =
          | NetTcpListener of Net.TcpListener.error

        let of_net_tcp_listener = fun error -> NetTcpListener error

        let detail_to_string = fun value ->
          match value with
          | NetTcpListener error -> Net.TcpListener.error_to_string error
      |ocaml}
  in
  match prepare_snapshot_or_error
    session
    ~roots:[
      unix_intf_source_id;
      unix_impl_source_id;
      tcp_listener_intf_source_id;
      tcp_listener_impl_source_id;
      net_intf_source_id;
      net_impl_source_id;
      error_source_id;
    ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot error_source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        let of_net_tcp_listener_type = export_scheme snapshot error_source_id "of_net_tcp_listener" in
        if of_net_tcp_listener_type = Some "Net.TcpListener.error -> t" then
          Ok ()
        else
          Error ("unexpected of_net_tcp_listener type: " ^ show_option of_net_tcp_listener_type)

let test_prepare_snapshot_planner_aliases_preserve_nested_constructor_owners = fun _ctx ->
  let create_named_source session ~kind ~module_name ~filename ~implicit_opens ~text =
    let parse_result = Syn.parse ~filename:(Path.v filename) text in
    let cst = expect_cst ~filename parse_result in
    Session.create_source
      session
      ~kind
      ~module_name
      ~implicit_opens
      ~origin:(Source.Label filename)
      ~source_hash:(Source.hash ~implicit_opens ~cst)
      ~parse_result
      ~cst
  in
  let session = Session.empty ~config:Config.default in
  let (session, _read_dir_intf_source_id) = create_named_source session ~kind:Source.File ~module_name:"Kernel_new__Fs__Read_dir" ~filename:"read_dir.mli" ~implicit_opens:[]
    ~text:{ocaml|
        type error =
          | File of int
      |ocaml}
  in
  let (session, _read_dir_impl_source_id) = create_named_source session ~kind:Source.File ~module_name:"Kernel_new__Fs__Read_dir" ~filename:"read_dir.ml" ~implicit_opens:[]
    ~text:{ocaml|
        type error =
          | File of int
      |ocaml}
  in
  let (session, _fs_aliases_source_id) = create_named_source session ~kind:Source.Generated ~module_name:"Kernel_new__Fs__Aliases" ~filename:"Kernel_new__Fs__Aliases.ml-gen" ~implicit_opens:[]
    ~text:{ocaml|
        module Read_dir = Kernel_new__Fs__Read_dir

        module Super = struct
          module Read_dir = Kernel_new__Fs__Read_dir
        end
      |ocaml}
  in
  let fs_implicit_opens = [ SurfacePath.of_string "Kernel_new__Fs__Aliases" ] in
  let (session, _fs_intf_source_id) = create_named_source session ~kind:Source.File ~module_name:"Kernel_new__Fs" ~filename:"fs.mli" ~implicit_opens:fs_implicit_opens
    ~text:{ocaml|
        module ReadDir = Read_dir
      |ocaml}
  in
  let (session, _fs_impl_source_id) = create_named_source session ~kind:Source.File ~module_name:"Kernel_new__Fs" ~filename:"fs.ml" ~implicit_opens:fs_implicit_opens
    ~text:{ocaml|
        module ReadDir = Read_dir
      |ocaml}
  in
  let (session, _root_aliases_source_id) = create_named_source session ~kind:Source.Generated ~module_name:"Kernel_new__Aliases" ~filename:"Kernel_new__Aliases.ml-gen" ~implicit_opens:[]
    ~text:{ocaml|
        module Fs = Kernel_new__Fs

        module Super = struct
          module Fs = Kernel_new__Fs
        end
      |ocaml}
  in
  let error_implicit_opens = [ SurfacePath.of_string "Kernel_new__Aliases" ] in
  let (session, error_source_id) = create_named_source session ~kind:Source.File ~module_name:"Kernel_new__Error" ~filename:"error.ml" ~implicit_opens:error_implicit_opens
    ~text:{ocaml|
        type t =
          | FsReadDir of Fs.ReadDir.error

        let system = fun value ->
          match value with
          | FsReadDir (Fs.ReadDir.File _code) -> true
      |ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ error_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot error_source_id in
      if List.is_empty diagnostics then
        Ok ()
      else
        let fs_type_names =
          match Session.Snapshot.find_module_typings_by_name snapshot "Kernel_new__Fs" with
          | Some typings -> ModuleTypings.type_decls typings
          |> List.map
            (fun (type_decl: FileSummary.type_decl) ->
              SurfacePath.append_name type_decl.scope_path type_decl.declaration.type_name
              |> SurfacePath.to_string)
          | None -> []
        in
        let fs_export_names =
          match Session.Snapshot.find_module_typings_by_name snapshot "Kernel_new__Fs" with
          | Some typings -> module_typings_export_names typings
          | None -> []
        in
        let error_semantic_tree =
          match Query.analysis_of_source snapshot error_source_id with
          | Some { SourceAnalysis.semantic_tree=Some semantic_tree; _ } -> SemanticTree.to_string semantic_tree
          | _ -> "<no semantic tree>"
        in
        Error (format
          Format.[
            str (String.concat "\n" diagnostics);
            str "\nKernel_new__Fs type decls: ";
            str (String.concat ", " fs_type_names);
            str "\nKernel_new__Fs exports: ";
            str (String.concat ", " fs_export_names);
            str "\nerror semantic tree:\n";
            str error_semantic_tree;
          ])

let test_prepare_snapshot_partial_wrapper_preserves_nested_module_exports = fun _ctx ->
  let create_named_source session ~module_name ~filename ~text =
    let parse_result = Syn.parse ~filename:(Path.v filename) text in
    let cst = expect_cst ~filename parse_result in
    Session.create_source
      session
      ~kind:Source.File
      ~module_name
      ~implicit_opens:[]
      ~origin:(Source.Label filename)
      ~source_hash:(Source.hash ~implicit_opens:[] ~cst)
      ~parse_result
      ~cst
  in
  let session = Session.empty ~config:Config.default in
  let (session, _non_zero_int_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Async__Non_zero_int" ~filename:"non_zero_int.mli"
    ~text:{ocaml|
        type t = int
      |ocaml}
  in
  let (session, _non_zero_int_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Async__Non_zero_int" ~filename:"non_zero_int.ml"
    ~text:{ocaml|
        type t = int
      |ocaml}
  in
  let (session, _interest_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Async__Interest" ~filename:"interest.mli"
    ~text:{ocaml|
        type t = Non_zero_int.t

        val is_readable : t -> bool

        val is_writable : t -> bool
      |ocaml}
  in
  let (session, _interest_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Async__Interest" ~filename:"interest.ml"
    ~text:{ocaml|
        type t = Non_zero_int.t

        let is_readable value =
          let _ = value in
          true

        let is_writable value =
          let _ = value in
          true
      |ocaml}
  in
  let (session, _adapter_unix_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Async__Adapter__Unix" ~filename:"adapter_unix.mli"
    ~text:{ocaml|
        type error =
          | Oops

        module Selector : sig
          type t

          val make : unit -> t

          val close : t -> unit

          val select : t -> unit

          val register : t -> interest:Interest.t -> unit

          val reregister : t -> interest:Interest.t -> unit
        end
      |ocaml}
  in
  let (session, _adapter_unix_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Async__Adapter__Unix" ~filename:"adapter_unix.ml"
    ~text:{ocaml|
        type selector = int

        type error =
          | Oops

        module Selector = struct
          type t = selector

          let make () = 0

          let close _ = ()

          let select _ = ()

          let register _ ~interest =
            let _ = Interest.is_readable interest in
            ()

          let reregister _ ~interest =
            let _ = Interest.is_writable interest in
            ()
        end
      |ocaml}
  in
  let (session, _adapter_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Async__Adapter" ~filename:"adapter.mli"
    ~text:{ocaml|
        type error =
          | Oops

        module Selector : sig
          type t

          val make : unit -> t

          val close : t -> unit

          val select : t -> unit

          val register : t -> interest:Interest.t -> unit

          val reregister : t -> interest:Interest.t -> unit
        end
      |ocaml}
  in
  let (session, _adapter_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Async__Adapter" ~filename:"adapter.ml"
    ~text:{ocaml|
        include Unix
      |ocaml}
  in
  let (session, poll_source_id) = create_named_source session ~module_name:"Kernel_new__Async__Poll" ~filename:"poll.ml"
    ~text:{ocaml|
        type t = {
          selector : Adapter.Selector.t;
        }

        let make () = { selector = Adapter.Selector.make () }

        let close value = Adapter.Selector.close value.selector

        let poll value = Adapter.Selector.select value.selector
      |ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ poll_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot poll_source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        let make_type = export_scheme snapshot poll_source_id "make" in
        let close_type = export_scheme snapshot poll_source_id "close" in
        let poll_type = export_scheme snapshot poll_source_id "poll" in
        if not (make_type = Some "unit -> t") then
          Error ("unexpected make type: " ^ show_option make_type)
        else if not (close_type = Some "t -> unit") then
          Error ("unexpected close type: " ^ show_option close_type)
        else if not (poll_type = Some "t -> unit") then
          Error ("unexpected poll type: " ^ show_option poll_type)
        else
          Ok ()

let test_prepare_snapshot_paired_module_alias_preserves_interface_shaped_sibling_types = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _non_zero_int_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "non_zero_int.mli")
    ~text:{ocaml|
        type t = int
      |ocaml}
  in
  let (session, _non_zero_int_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "non_zero_int.ml")
    ~text:{ocaml|
        type t = int
      |ocaml}
  in
  let (session, _interest_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "interest.mli")
    ~text:{ocaml|
        type t

        val readable : t

        val is_readable : t -> bool
      |ocaml}
  in
  let (session, _interest_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "interest.ml")
    ~text:{ocaml|
        type t = Non_zero_int.t

        let readable = 1

        let is_readable value =
          let _ = value in
          true
      |ocaml}
  in
  let (session, async_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "async.mli")
    ~text:{ocaml|
        module Interest : sig
          type t

          val readable : t

          val is_readable : t -> bool
        end
      |ocaml}
  in
  let (session, async_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "async.ml")
    ~text:{ocaml|
        module Interest = Interest
      |ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ async_intf_source_id; async_impl_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let async_intf_diagnostics = diagnostic_strings snapshot async_intf_source_id in
      let async_impl_diagnostics = diagnostic_strings snapshot async_impl_source_id in
      if not (List.is_empty async_intf_diagnostics) then
        Error (String.concat "\n" async_intf_diagnostics)
      else if not (List.is_empty async_impl_diagnostics) then
        Error (String.concat "\n" async_impl_diagnostics)
      else
        let readable_type = export_scheme snapshot async_impl_source_id "Interest.readable" in
        let is_readable_type = export_scheme snapshot async_impl_source_id "Interest.is_readable" in
        if readable_type = Some "Interest.t" && is_readable_type = Some "Interest.t -> bool" then
          Ok ()
        else
          Error (format
            Format.[
              str "unexpected async interest exports: readable=";
              str (show_option readable_type);
              str ", is_readable=";
              str (show_option is_readable_type);
            ])

let test_prepare_snapshot_query_order_preserves_in_progress_wrapper_exports = fun _ctx ->
  let create_named_source session ~module_name ~filename ~text =
    let parse_result = Syn.parse ~filename:(Path.v filename) text in
    let cst = expect_cst ~filename parse_result in
    Session.create_source
      session
      ~kind:Source.File
      ~module_name
      ~implicit_opens:[]
      ~origin:(Source.Label filename)
      ~source_hash:(Source.hash ~implicit_opens:[] ~cst)
      ~parse_result
      ~cst
  in
  let session = Session.empty ~config:Config.default in
  let (session, net_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net" ~filename:"net.mli"
    ~text:{ocaml|
        module IpAddr = Ip_addr

        module SocketAddr = Socket_addr
      |ocaml}
  in
  let (session, net_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net" ~filename:"net.ml"
    ~text:{ocaml|
        module IpAddr = Ip_addr

        module SocketAddr = Socket_addr
      |ocaml}
  in
  let (session, _ip_addr_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Ip_addr" ~filename:"ip_addr.mli"
    ~text:{ocaml|
        type t

        val v4_loopback : t

        val v6_loopback : t

        val to_string : t -> string
      |ocaml}
  in
  let (session, _ip_addr_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Ip_addr" ~filename:"ip_addr.ml"
    ~text:{ocaml|
        include Unix
      |ocaml}
  in
  let (session, _ip_addr_unix_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Ip_addr__Unix" ~filename:"ip_addr_unix.mli"
    ~text:{ocaml|
        type t

        val v4_loopback : t

        val v6_loopback : t

        val to_string : t -> string
      |ocaml}
  in
  let (session, _ip_addr_unix_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Ip_addr__Unix" ~filename:"ip_addr_unix.ml"
    ~text:{ocaml|
        type t = string

        let v4_loopback = "127.0.0.1"

        let v6_loopback = "::1"

        let to_string value = value
      |ocaml}
  in
  let (session, _socket_addr_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Socket_addr" ~filename:"socket_addr.mli"
    ~text:{ocaml|
        type t

        val loopback_v4 : port:int -> t

        val loopback_v6 : port:int -> t

        val to_string : t -> string
      |ocaml}
  in
  let (session, socket_addr_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Socket_addr" ~filename:"socket_addr.ml"
    ~text:{ocaml|
        type t = {
          ip : Ip_addr.t;
          port : int;
        }

        let loopback_v4 ~port = { ip = Ip_addr.v4_loopback; port }

        let loopback_v6 ~port = { ip = Ip_addr.v6_loopback; port }

        let to_string value = Ip_addr.to_string value.ip
      |ocaml}
  in
  let roots = [ net_intf_source_id; net_impl_source_id; socket_addr_impl_source_id; ] in
  match prepare_snapshot_or_error session ~roots with
  | Error _ as err -> err
  | Ok snapshot ->
      let _ = Query.analysis_of_source snapshot net_intf_source_id in
      let _ = Query.analysis_of_source snapshot net_impl_source_id in
      let diagnostics = diagnostic_strings snapshot socket_addr_impl_source_id in
      if List.is_empty diagnostics then
        Ok ()
      else
        Error (String.concat "\n" diagnostics)

let test_prepare_snapshot_net_wrapper_graph_preserves_ip_addr_exports = fun _ctx ->
  let module_name_suffix_aliases module_name =
    let segments = module_name
    |> String.split_on_char '.'
    |> List.filter (fun segment -> not (String.equal segment "")) in
    let rec loop aliases = function
      | [] -> List.rev aliases
      | _ :: rest as current -> loop (String.concat "." current :: aliases) rest
    in
    loop [] segments |> List.sort_uniq String.compare
  in
  let create_named_source session ~module_name ~filename ~text =
    let parse_result = Syn.parse ~filename:(Path.v filename) text in
    let cst = expect_cst ~filename parse_result in
    Session.create_source
      session
      ~kind:Source.File
      ~module_name
      ~implicit_opens:[]
      ~origin:(Source.Label filename)
      ~source_hash:(Source.hash ~implicit_opens:[] ~cst)
      ~parse_result
      ~cst
  in
  let register_local_aliases session source_id local_module_name = module_name_suffix_aliases local_module_name
  |> List.fold_left
    (fun session module_name -> Session.register_source_alias session source_id ~module_name)
    session in
  let session = Session.empty ~config:Config.default in
  let (session, net_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net" ~filename:"net.mli"
    ~text:{ocaml|
        module IpAddr = Ip_addr

        module SocketAddr = Socket_addr

        module TcpStream = Tcp_stream
      |ocaml}
  in
  let (session, net_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net" ~filename:"net.ml"
    ~text:{ocaml|
        module IpAddr = Ip_addr

        module SocketAddr = Socket_addr

        module TcpStream = Tcp_stream
      |ocaml}
  in
  let (session, ip_addr_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Ip_addr" ~filename:"ip_addr.mli"
    ~text:{ocaml|
        type t

        val v4_loopback : t
        val of_string : string -> t
        val to_string : t -> string
      |ocaml}
  in
  let session = register_local_aliases session ip_addr_intf_source_id "Net.Ip_addr" in
  let (session, ip_addr_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Ip_addr" ~filename:"ip_addr.ml"
    ~text:{ocaml|
        include Unix
      |ocaml}
  in
  let session = register_local_aliases session ip_addr_impl_source_id "Net.Ip_addr" in
  let (session, ip_addr_unix_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Ip_addr__Unix" ~filename:"ip_addr_unix.ml"
    ~text:{ocaml|
        type t = string

        let v4_loopback = "127.0.0.1"
        let of_string value = value
        let to_string value = value
      |ocaml}
  in
  let session = register_local_aliases session ip_addr_unix_source_id "Net.Ip_addr.Unix" in
  let (session, socket_addr_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Socket_addr" ~filename:"socket_addr.mli"
    ~text:{ocaml|
        type t

        val of_parts : ip:Ip_addr.t -> port:int -> t
        val loopback_v4 : port:int -> t
        val to_string : t -> string
      |ocaml}
  in
  let session = register_local_aliases session socket_addr_intf_source_id "Net.Socket_addr" in
  let (session, socket_addr_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Socket_addr" ~filename:"socket_addr.ml"
    ~text:{ocaml|
        type t = {
          ip : Ip_addr.t;
          port : int;
        }

        let of_parts ~ip ~port = { ip; port }

        let loopback_v4 ~port = { ip = Ip_addr.v4_loopback; port }

        let to_string value = Ip_addr.to_string value.ip
      |ocaml}
  in
  let session = register_local_aliases session socket_addr_impl_source_id "Net.Socket_addr" in
  let (session, tcp_stream_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Tcp_stream" ~filename:"tcp_stream.mli"
    ~text:{ocaml|
        type t

        val local_addr_text : t -> string
      |ocaml}
  in
  let session = register_local_aliases session tcp_stream_intf_source_id "Net.Tcp_stream" in
  let (session, tcp_stream_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Tcp_stream" ~filename:"tcp_stream.ml"
    ~text:{ocaml|
        include Unix
      |ocaml}
  in
  let session = register_local_aliases session tcp_stream_impl_source_id "Net.Tcp_stream" in
  let (session, tcp_stream_unix_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Tcp_stream__Unix" ~filename:"tcp_stream_unix.ml"
    ~text:{ocaml|
        type t = int

        let socket_addr_of_pair (ip, port) =
          let ip = Ip_addr.of_string ip in
          Socket_addr.of_parts ~ip ~port

        let local_addr_text stream =
          let addr = socket_addr_of_pair ("127.0.0.1", stream) in
          Socket_addr.to_string addr
      |ocaml}
  in
  let session = register_local_aliases session tcp_stream_unix_source_id "Net.Tcp_stream.Unix" in
  let roots = [
    ip_addr_intf_source_id;
    ip_addr_impl_source_id;
    ip_addr_unix_source_id;
    socket_addr_intf_source_id;
    socket_addr_impl_source_id;
    tcp_stream_intf_source_id;
    tcp_stream_impl_source_id;
    tcp_stream_unix_source_id;
  ] in
  match prepare_snapshot_or_error session ~roots with
  | Error _ as err -> err
  | Ok snapshot ->
      let socket_addr_diagnostics = diagnostic_strings snapshot socket_addr_impl_source_id in
      let tcp_stream_diagnostics = diagnostic_strings snapshot tcp_stream_unix_source_id in
      let ip_addr_exports =
        match Session.Snapshot.find_module_typings_by_name snapshot "Kernel_new__Net__Ip_addr" with
        | Some typings -> module_typings_export_names typings |> String.concat ", "
        | None -> "<missing>"
      in
      let ip_addr_impl_exports = export_names (Query.export_of snapshot ip_addr_impl_source_id)
      |> String.concat ", " in
      let ip_addr_intf_exports = export_names (Query.export_of snapshot ip_addr_intf_source_id)
      |> String.concat ", " in
      let ip_addr_impl_diagnostics = diagnostic_strings snapshot ip_addr_impl_source_id in
      let ip_addr_intf_diagnostics = diagnostic_strings snapshot ip_addr_intf_source_id in
      if not (List.is_empty socket_addr_diagnostics) then
        Error (
          format
            Format.[
              str "socket_addr diagnostics:\n";
              str (String.concat "\n" socket_addr_diagnostics);
              str "\nip_addr exports: ";
              str ip_addr_exports;
              str "\nip_addr impl exports: ";
              str ip_addr_impl_exports;
              str "\nip_addr intf exports: ";
              str ip_addr_intf_exports;
              str "\nip_addr impl diagnostics:\n";
              str (String.concat "\n" ip_addr_impl_diagnostics);
              str "\nip_addr intf diagnostics:\n";
              str (String.concat "\n" ip_addr_intf_diagnostics);
            ]
        )
      else if not (List.is_empty tcp_stream_diagnostics) then
        Error (
          format
            Format.[
              str "tcp_stream_unix diagnostics:\n";
              str (String.concat "\n" tcp_stream_diagnostics);
              str "\nip_addr exports: ";
              str ip_addr_exports;
              str "\nip_addr impl exports: ";
              str ip_addr_impl_exports;
              str "\nip_addr intf exports: ";
              str ip_addr_intf_exports;
              str "\nip_addr impl diagnostics:\n";
              str (String.concat "\n" ip_addr_impl_diagnostics);
              str "\nip_addr intf diagnostics:\n";
              str (String.concat "\n" ip_addr_intf_diagnostics);
            ]
        )
      else
        Ok ()

let test_prepare_snapshot_net_wrapper_graph_preserves_ip_addr_exports_with_paired_unix = fun _ctx ->
  let module_name_suffix_aliases module_name =
    let segments = module_name
    |> String.split_on_char '.'
    |> List.filter (fun segment -> not (String.equal segment "")) in
    let rec loop aliases = function
      | [] -> List.rev aliases
      | _ :: rest as current -> loop (String.concat "." current :: aliases) rest
    in
    loop [] segments |> List.sort_uniq String.compare
  in
  let create_named_source session ~module_name ~filename ~text =
    let parse_result = Syn.parse ~filename:(Path.v filename) text in
    let cst = expect_cst ~filename parse_result in
    Session.create_source
      session
      ~kind:Source.File
      ~module_name
      ~implicit_opens:[]
      ~origin:(Source.Label filename)
      ~source_hash:(Source.hash ~implicit_opens:[] ~cst)
      ~parse_result
      ~cst
  in
  let register_local_aliases session source_id local_module_name = module_name_suffix_aliases local_module_name
  |> List.fold_left
    (fun session module_name -> Session.register_source_alias session source_id ~module_name)
    session in
  let session = Session.empty ~config:Config.default in
  let (session, net_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net" ~filename:"net.mli"
    ~text:{ocaml|
        module IpAddr = Ip_addr

        module SocketAddr = Socket_addr

        module TcpStream = Tcp_stream
      |ocaml}
  in
  let (session, net_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net" ~filename:"net.ml"
    ~text:{ocaml|
        module IpAddr = Ip_addr

        module SocketAddr = Socket_addr

        module TcpStream = Tcp_stream
      |ocaml}
  in
  let (session, ip_addr_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Ip_addr" ~filename:"ip_addr.mli"
    ~text:{ocaml|
        type t

        val v4_loopback : t
        val of_string : string -> t
        val to_string : t -> string
      |ocaml}
  in
  let session = register_local_aliases session ip_addr_intf_source_id "Net.Ip_addr" in
  let (session, ip_addr_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Ip_addr" ~filename:"ip_addr.ml"
    ~text:{ocaml|
        include Unix
      |ocaml}
  in
  let session = register_local_aliases session ip_addr_impl_source_id "Net.Ip_addr" in
  let (session, ip_addr_unix_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Ip_addr__Unix" ~filename:"unix.mli"
    ~text:{ocaml|
        type t

        val v4_loopback : t
        val of_string : string -> t
        val to_string : t -> string
      |ocaml}
  in
  let session = register_local_aliases session ip_addr_unix_intf_source_id "Net.Ip_addr.Unix" in
  let (session, ip_addr_unix_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Ip_addr__Unix" ~filename:"unix.ml"
    ~text:{ocaml|
        type t = string

        let v4_loopback = "127.0.0.1"
        let of_string value = value
        let to_string value = value
      |ocaml}
  in
  let session = register_local_aliases session ip_addr_unix_impl_source_id "Net.Ip_addr.Unix" in
  let (session, socket_addr_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Socket_addr" ~filename:"socket_addr.mli"
    ~text:{ocaml|
        type t

        val of_parts : ip:Ip_addr.t -> port:int -> t
        val loopback_v4 : port:int -> t
        val to_string : t -> string
      |ocaml}
  in
  let session = register_local_aliases session socket_addr_intf_source_id "Net.Socket_addr" in
  let (session, socket_addr_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Socket_addr" ~filename:"socket_addr.ml"
    ~text:{ocaml|
        type t = {
          ip : Ip_addr.t;
          port : int;
        }

        let of_parts ~ip ~port = { ip; port }

        let loopback_v4 ~port = { ip = Ip_addr.v4_loopback; port }

        let to_string value = Ip_addr.to_string value.ip
      |ocaml}
  in
  let session = register_local_aliases session socket_addr_impl_source_id "Net.Socket_addr" in
  let (session, tcp_stream_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Tcp_stream" ~filename:"tcp_stream.mli"
    ~text:{ocaml|
        type t

        val local_addr_text : t -> string
      |ocaml}
  in
  let session = register_local_aliases session tcp_stream_intf_source_id "Net.Tcp_stream" in
  let (session, tcp_stream_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Tcp_stream" ~filename:"tcp_stream.ml"
    ~text:{ocaml|
        include Unix
      |ocaml}
  in
  let session = register_local_aliases session tcp_stream_impl_source_id "Net.Tcp_stream" in
  let (session, tcp_stream_unix_source_id) = create_named_source session ~module_name:"Kernel_new__Net__Tcp_stream__Unix" ~filename:"unix.ml"
    ~text:{ocaml|
        type t = int

        let socket_addr_of_pair (ip, port) =
          let ip = Ip_addr.of_string ip in
          Socket_addr.of_parts ~ip ~port

        let local_addr_text stream =
          let addr = socket_addr_of_pair ("127.0.0.1", stream) in
          Socket_addr.to_string addr
      |ocaml}
  in
  let session = register_local_aliases session tcp_stream_unix_source_id "Net.Tcp_stream.Unix" in
  let roots = [
    net_intf_source_id;
    net_impl_source_id;
    ip_addr_intf_source_id;
    ip_addr_impl_source_id;
    ip_addr_unix_intf_source_id;
    ip_addr_unix_impl_source_id;
    socket_addr_intf_source_id;
    socket_addr_impl_source_id;
    tcp_stream_intf_source_id;
    tcp_stream_impl_source_id;
    tcp_stream_unix_source_id;
  ]
  in
  match prepare_snapshot_or_error session ~roots with
  | Error _ as err -> err
  | Ok snapshot ->
      let _ = Session.Snapshot.module_typings snapshot in
      let socket_addr_diagnostics = diagnostic_strings snapshot socket_addr_impl_source_id in
      let tcp_stream_diagnostics = diagnostic_strings snapshot tcp_stream_unix_source_id in
      if not (List.is_empty socket_addr_diagnostics) then
        Error (String.concat "\n" socket_addr_diagnostics)
      else if not (List.is_empty tcp_stream_diagnostics) then
        Error (String.concat "\n" tcp_stream_diagnostics)
      else
        Ok ()

let test_implicit_opens_do_not_leak_into_module_exports = fun _ctx ->
  let config = Config.default |> Config.with_capture_traces ~capture_traces:true in
  let session = Session.empty ~config in
  let helper_text = "let twice x = x + x\n" in
  let helper_parse_result = Syn.parse ~filename:(Path.v "helper.ml") helper_text in
  let helper_cst = expect_cst ~filename:"helper.ml" helper_parse_result in
  let aliases_text = {ocaml|
    module Helper = Colors__Helper

    module Super = struct
      module Helper = Colors__Helper
    end
  |ocaml}
  in
  let aliases_parse_result = Syn.parse ~filename:(Path.v "Colors__Aliases.ml-gen") aliases_text in
  let aliases_cst = expect_cst ~filename:"Colors__Aliases.ml-gen" aliases_parse_result in
  let (session, _aliases_source_id) = Session.create_source
    session
    ~kind:Source.Generated
    ~module_name:"Colors__Aliases"
    ~implicit_opens:[]
    ~origin:(Source.Label "Colors__Aliases.ml-gen")
    ~source_hash:(Source.hash ~implicit_opens:[] ~cst:aliases_cst)
    ~parse_result:aliases_parse_result
    ~cst:aliases_cst in
  let implicit_opens = [ SurfacePath.of_string "Colors__Aliases" ] in
  let (session, helper_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~module_name:"Colors__Helper"
    ~implicit_opens
    ~origin:(Source.Label "helper.ml")
    ~source_hash:(Source.hash ~implicit_opens ~cst:helper_cst)
    ~parse_result:helper_parse_result
    ~cst:helper_cst in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot helper_source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let exports = export_names (Query.export_of snapshot helper_source_id) in
    if not (exports = [ "twice" ]) then
      Error (format
        Format.[
          str "unexpected helper exports: ";
          str (String.concat ", " exports);
          str "\n";
          str (String.concat "\n" (trace_debug snapshot helper_source_id));
        ])
    else
      Ok ()

let test_snapshot_exports_interface_declarations = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
    type color
    val id : 'a -> 'a
    module Local : sig
      type t
      val id : t -> t
    end
    module Uses_outer : sig
      val paint : color -> color
    end
  |ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "iface.mli")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let id_type = export_scheme snapshot source_id "id" in
    let local_id_type = export_scheme snapshot source_id "Local.id" in
    let paint_type = export_scheme snapshot source_id "Uses_outer.paint" in
    let type_names = exported_type_names snapshot source_id in
    Test.assert_equal ~expected:(Some "'a. 'a -> 'a") ~actual:id_type;
    Test.assert_equal ~expected:(Some "Local.t -> Local.t") ~actual:local_id_type;
    Test.assert_equal ~expected:(Some "color -> color") ~actual:paint_type;
    Test.assert_equal ~expected:[ "color"; "Local.t" ] ~actual:type_names;
    Ok ()

let test_snapshot_exports_interface_externals = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = "external strlen : string -> int = \"caml_strlen\"\n" in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "externals.mli")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let strlen_type = export_scheme snapshot source_id "strlen" in
    Test.assert_equal ~expected:(Some "string -> int") ~actual:strlen_type;
    Ok ()

let test_snapshot_collects_module_typings = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "alpha.ml")
    ~text:"let id x = x\n" in
  let (session, _) = create_source session ~kind:Source.File ~origin:(Source.Label "beta.ml") ~text:"let broken = missing\n" in
  let snapshot = Session.snapshot session in
  let summaries = module_typings_jsons snapshot in
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
  Test.assert_equal ~expected:[ "trusted_export"; "errored_export" ] ~actual:tags;
  Ok ()

let test_snapshot_module_typings_are_canonical_per_module = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _impl_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"let answer = 42\n" in
  let (session, _intf_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.mli")
    ~text:"val answer : int\n" in
  let snapshot = Session.snapshot session in
  let module_names =
    module_typings_jsons snapshot
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
  Test.assert_equal ~expected:[ "Colors" ] ~actual:module_names;
  Ok ()

let test_query_module_typings_of_uses_canonical_root_typings = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, impl_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"let answer = 42\n" in
  let (session, intf_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.mli")
    ~text:"val answer : int\n" in
  let snapshot = Session.snapshot session in
  let canonical_json =
    match Snapshot.module_typings snapshot with
    | [ typings ] -> ModuleTypings.Json.to_json typings |> Data.Json.to_string
    | typings -> panic
      (format
        Format.[
          str "expected one canonical module typings value but got ";
          int (List.length typings);
        ])
  in
  let typings_json source_id =
    match Query.module_typings_of snapshot source_id with
    | Some typings -> ModuleTypings.Json.to_json typings |> Data.Json.to_string
    | None -> panic
      (format Format.[ str "expected module typings for "; str (SourceId.to_string source_id); ])
  in
  let impl_json = typings_json impl_source_id in
  let intf_json = typings_json intf_source_id in
  Test.assert_equal ~expected:canonical_json ~actual:impl_json;
  Test.assert_equal ~expected:canonical_json ~actual:intf_json;
  Ok ()

let test_paired_modules_export_interface_shaped_module_typings = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, impl_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"let answer = 42\nlet hidden = true\n" in
  let (session, intf_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.mli")
    ~text:"val answer : int\n" in
  let snapshot = Session.snapshot session in
  let export_names_for source_id =
    match Query.module_typings_of snapshot source_id with
    | None -> []
    | Some typings -> module_typings_export_names typings
  in
  Test.assert_equal ~expected:[ "answer" ] ~actual:(export_names_for impl_source_id);
  Test.assert_equal ~expected:[ "answer" ] ~actual:(export_names_for intf_source_id);
  Ok ()

let test_paired_modules_export_interface_shaped_file_summaries = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, impl_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"let answer = 42\nlet hidden = true\n" in
  let (session, intf_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.mli")
    ~text:"val answer : int\n" in
  let snapshot = Session.snapshot session in
  Test.assert_equal
    ~expected:[ "answer" ]
    ~actual:(export_names (Query.export_of snapshot impl_source_id));
  Test.assert_equal
    ~expected:[ "answer" ]
    ~actual:(export_names (Query.export_of snapshot intf_source_id));
  Test.assert_equal
    ~expected:[ "answer" ]
    ~actual:(file_summary_export_names snapshot impl_source_id);
  Test.assert_equal
    ~expected:[ "answer" ]
    ~actual:(file_summary_export_names snapshot intf_source_id);
  Ok ()

let test_paired_modules_report_signature_inclusion_mismatches = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, impl_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"let answer = true\n" in
  let (session, intf_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.mli")
    ~text:"val answer : int\n" in
  let snapshot = Session.snapshot session in
  let has_signature_error source_id =
    Query.diagnostics snapshot source_id
    |> List.exists
      (
        function
        | Query.Typing (Diagnostic.SignatureInclusionError _) -> true
        | _ -> false
      )
  in
  let impl_typings = Query.module_typings_of snapshot impl_source_id |> Option.expect ~msg:"missing impl typings" in
  let intf_typings = Query.module_typings_of snapshot intf_source_id |> Option.expect ~msg:"missing interface typings" in
  Test.assert_equal ~expected:true ~actual:(has_signature_error impl_source_id);
  Test.assert_equal ~expected:true ~actual:(has_signature_error intf_source_id);
  Test.assert_equal ~expected:[] ~actual:(module_typings_export_names impl_typings);
  Test.assert_equal ~expected:[] ~actual:(module_typings_export_names intf_typings);
  Test.assert_equal ~expected:[] ~actual:(export_names (Query.export_of snapshot impl_source_id));
  Test.assert_equal ~expected:[] ~actual:(export_names (Query.export_of snapshot intf_source_id));
  Test.assert_equal ~expected:[] ~actual:(file_summary_export_names snapshot impl_source_id);
  Test.assert_equal ~expected:[] ~actual:(file_summary_export_names snapshot intf_source_id);
  Ok ()

let test_paired_modules_skip_signature_inclusion_for_errored_implementation = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, impl_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"let answer = missing\n" in
  let (session, intf_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.mli")
    ~text:"val answer : int\n" in
  let snapshot = Session.snapshot session in
  let has_signature_error source_id =
    Query.diagnostics snapshot source_id
    |> List.exists
      (
        function
        | Query.Typing (Diagnostic.SignatureInclusionError _) -> true
        | _ -> false
      )
  in
  let impl_typings = Query.module_typings_of snapshot impl_source_id |> Option.expect ~msg:"missing impl typings" in
  let intf_typings = Query.module_typings_of snapshot intf_source_id |> Option.expect ~msg:"missing interface typings" in
  Test.assert_equal ~expected:true ~actual:(has_unbound_name snapshot impl_source_id);
  Test.assert_equal ~expected:false ~actual:(has_signature_error impl_source_id);
  Test.assert_equal ~expected:false ~actual:(has_signature_error intf_source_id);
  Test.assert_equal ~expected:[ "answer" ] ~actual:(module_typings_export_names impl_typings);
  Test.assert_equal ~expected:[ "answer" ] ~actual:(module_typings_export_names intf_typings);
  Test.assert_equal ~expected:(Some "int") ~actual:(export_scheme snapshot impl_source_id "answer");
  Test.assert_equal ~expected:(Some "int") ~actual:(export_scheme snapshot intf_source_id "answer");
  Test.assert_equal
    ~expected:[ "answer" ]
    ~actual:(file_summary_export_names snapshot impl_source_id);
  Test.assert_equal
    ~expected:[ "answer" ]
    ~actual:(file_summary_export_names snapshot intf_source_id);
  Ok ()

let test_paired_modules_skip_signature_inclusion_for_unsupported_interface_types = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, impl_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "color.ml")
    ~text:"type t = Color\nlet to_escape_seq ~mode:_ _ = \"\"\n" in
  let (session, intf_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "color.mli")
    ~text:"type t\nval to_escape_seq : mode:[> `bg | `fg ] -> t -> string\n" in
  let snapshot = Session.snapshot session in
  let has_signature_error source_id =
    Query.diagnostics snapshot source_id
    |> List.exists
      (
        function
        | Query.Typing (Diagnostic.SignatureInclusionError _) -> true
        | _ -> false
      )
  in
  let impl_typings = Query.module_typings_of snapshot impl_source_id |> Option.expect ~msg:"missing impl typings" in
  let intf_typings = Query.module_typings_of snapshot intf_source_id |> Option.expect ~msg:"missing interface typings" in
  Test.assert_equal ~expected:false ~actual:(has_signature_error impl_source_id);
  Test.assert_equal ~expected:false ~actual:(has_signature_error intf_source_id);
  Test.assert_equal ~expected:false ~actual:(has_unsupported_syntax snapshot impl_source_id);
  Test.assert_equal ~expected:true ~actual:(has_unsupported_syntax snapshot intf_source_id);
  Test.assert_equal ~expected:[ "to_escape_seq" ] ~actual:(module_typings_export_names impl_typings);
  Test.assert_equal ~expected:[ "to_escape_seq" ] ~actual:(module_typings_export_names intf_typings);
  Test.assert_equal
    ~expected:[ "to_escape_seq" ]
    ~actual:(file_summary_export_names snapshot impl_source_id);
  Test.assert_equal
    ~expected:[ "to_escape_seq" ]
    ~actual:(file_summary_export_names snapshot intf_source_id);
  Ok ()

let test_paired_modules_accept_manifest_alias_specialization = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "hash.ml")
    ~text:{ocaml|
      type t = bytes

      let of_bytes bytes = bytes

      let to_bytes hash = hash
    |ocaml}
  in
  let (session, intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "hash.mli")
    ~text:{ocaml|
      type t

      val of_bytes : bytes -> t

      val to_bytes : t -> bytes
    |ocaml}
  in
  let snapshot = Session.snapshot session in
  let impl_typings = Query.module_typings_of snapshot impl_source_id |> Option.expect ~msg:"missing impl typings" in
  let intf_typings = Query.module_typings_of snapshot intf_source_id |> Option.expect ~msg:"missing interface typings" in
  Test.assert_equal ~expected:false ~actual:(has_signature_error snapshot impl_source_id);
  Test.assert_equal ~expected:false ~actual:(has_signature_error snapshot intf_source_id);
  Test.assert_equal
    ~expected:[ "of_bytes"; "to_bytes" ]
    ~actual:(module_typings_export_names impl_typings);
  Test.assert_equal
    ~expected:[ "of_bytes"; "to_bytes" ]
    ~actual:(module_typings_export_names intf_typings);
  Test.assert_equal
    ~expected:[ "of_bytes"; "to_bytes" ]
    ~actual:(file_summary_export_names snapshot impl_source_id);
  Test.assert_equal
    ~expected:[ "of_bytes"; "to_bytes" ]
    ~actual:(file_summary_export_names snapshot intf_source_id);
  Ok ()

let test_paired_modules_canonicalize_builtin_aliases_in_signature_inclusion = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, string_alias_source_id) = create_source seed_session ~kind:Source.File ~origin:(Source.Label "string_alias.mli")
    ~text:{ocaml|
      type t = string
    |ocaml}
  in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_string_alias =
    match Query.module_typings_of seed_snapshot string_alias_source_id with
    | Some typings -> typings
    | None -> panic "expected string_alias module typings"
  in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ loaded_string_alias ] in
  let session = Session.empty ~config in
  let (session, impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "rng.ml")
    ~text:{ocaml|
      let normalize (value: string) = value

      let default () = ""
    |ocaml}
  in
  let (session, intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "rng.mli")
    ~text:{ocaml|
      val normalize : String_alias.t -> String_alias.t

      val default : unit -> String_alias.t
    |ocaml}
  in
  let snapshot = Session.snapshot session in
  let impl_typings = Query.module_typings_of snapshot impl_source_id |> Option.expect ~msg:"missing impl typings" in
  let intf_typings = Query.module_typings_of snapshot intf_source_id |> Option.expect ~msg:"missing interface typings" in
  Test.assert_equal ~expected:false ~actual:(has_signature_error snapshot impl_source_id);
  Test.assert_equal ~expected:false ~actual:(has_signature_error snapshot intf_source_id);
  Test.assert_equal
    ~expected:[ "default"; "normalize" ]
    ~actual:(module_typings_export_names impl_typings);
  Test.assert_equal
    ~expected:[ "default"; "normalize" ]
    ~actual:(module_typings_export_names intf_typings);
  Test.assert_equal
    ~expected:[ "default"; "normalize" ]
    ~actual:(file_summary_export_names snapshot impl_source_id);
  Test.assert_equal
    ~expected:[ "default"; "normalize" ]
    ~actual:(file_summary_export_names snapshot intf_source_id);
  Test.assert_equal
    ~expected:(Some "string -> string")
    ~actual:(export_scheme snapshot impl_source_id "normalize");
  Test.assert_equal
    ~expected:(Some "unit -> string")
    ~actual:(export_scheme snapshot intf_source_id "default");
  Ok ()

let test_paired_modules_accept_manifest_alias_value_usage_in_signature_inclusion = fun _ctx ->
  let create_named_source session ~module_name ~filename ~text =
    let parse_result = Syn.parse ~filename:(Path.v filename) text in
    let cst = expect_cst ~filename parse_result in
    Session.create_source
      session
      ~kind:Source.File
      ~module_name
      ~implicit_opens:[]
      ~origin:(Source.Label filename)
      ~source_hash:(Source.hash ~implicit_opens:[] ~cst)
      ~parse_result
      ~cst
  in
  let session = Session.empty ~config:Config.default in
  let (session, _non_zero_int_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Async__Non_zero_int" ~filename:"non_zero_int.mli"
    ~text:{ocaml|
        type t = int
      |ocaml}
  in
  let (session, _non_zero_int_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Async__Non_zero_int" ~filename:"non_zero_int.ml"
    ~text:{ocaml|
        type t = int
      |ocaml}
  in
  let (session, interest_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Async__Interest" ~filename:"interest.ml"
    ~text:{ocaml|
        type t = Non_zero_int.t

        let readable = 0b0001

        let add = fun left right -> left lor right

        let is_readable = fun value -> value land readable != 0
      |ocaml}
  in
  let (session, interest_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Async__Interest" ~filename:"interest.mli"
    ~text:{ocaml|
        type t = Non_zero_int.t

        val readable: t

        val add: t -> t -> t

        val is_readable: t -> bool
      |ocaml}
  in
  let snapshot = Session.snapshot session in
  Test.assert_equal ~expected:false ~actual:(has_signature_error snapshot interest_impl_source_id);
  Test.assert_equal ~expected:false ~actual:(has_signature_error snapshot interest_intf_source_id);
  let actual_impl_readable = export_scheme snapshot interest_impl_source_id "readable" in
  let actual_impl_add = export_scheme snapshot interest_impl_source_id "add" in
  let actual_impl_is_readable = export_scheme snapshot interest_impl_source_id "is_readable" in
  let actual_intf_readable = export_scheme snapshot interest_intf_source_id "readable" in
  let actual_intf_add = export_scheme snapshot interest_intf_source_id "add" in
  let actual_intf_is_readable = export_scheme snapshot interest_intf_source_id "is_readable" in
  if
    actual_impl_readable = Some "Non_zero_int.t"
    && actual_impl_add = Some "Non_zero_int.t -> Non_zero_int.t -> Non_zero_int.t"
    && actual_impl_is_readable = Some "Non_zero_int.t -> bool"
    && actual_intf_readable = Some "Non_zero_int.t"
    && actual_intf_add = Some "Non_zero_int.t -> Non_zero_int.t -> Non_zero_int.t"
    && actual_intf_is_readable = Some "Non_zero_int.t -> bool"
  then
    Ok ()
  else
    Error (
      format
        Format.[
          str "unexpected interest exports: impl.readable=";
          str (show_option actual_impl_readable);
          str ", impl.add=";
          str (show_option actual_impl_add);
          str ", impl.is_readable=";
          str (show_option actual_impl_is_readable);
          str ", intf.readable=";
          str (show_option actual_intf_readable);
          str ", intf.add=";
          str (show_option actual_intf_add);
          str ", intf.is_readable=";
          str (show_option actual_intf_is_readable);
        ]
    )

let test_manifest_option_aliases_canonicalize_during_inference = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, option_source_id) = create_source seed_session ~kind:Source.File ~origin:(Source.Label "option.ml")
    ~text:{ocaml|
      type 'value t = 'value option =
        | None
        | Some of 'value

      let map = fun fn ->
        function
        | Some value -> Some (fn value)
        | None -> None
    |ocaml}
  in
  let (seed_session, path_source_id) = create_source seed_session ~kind:Source.File ~origin:(Source.Label "path.ml")
    ~text:{ocaml|
      type t = string

      let v value = value
    |ocaml}
  in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_option =
    match Query.module_typings_of seed_snapshot option_source_id with
    | Some typings -> typings
    | None -> panic "expected option module typings"
  in
  let loaded_path =
    match Query.module_typings_of seed_snapshot path_source_id with
    | Some typings -> typings
    | None -> panic "expected path module typings"
  in
  let config = Config.default
  |> Config.with_loaded_modules ~loaded_modules:[ loaded_option; loaded_path ] in
  let session = Session.empty ~config in
  let (session, unix_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "unix.ml")
    ~text:{ocaml|
      let get = fun _ -> Option.Some "HOME"

      let home_dir = fun () ->
        Option.map Path.v (get "HOME")
    |ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ unix_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot unix_source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        let () = Test.assert_equal
          ~expected:false
          ~actual:(has_signature_error snapshot unix_source_id) in
        let () = Test.assert_equal
          ~expected:true
          ~actual:(Option.is_some (export_scheme snapshot unix_source_id "home_dir")) in
        Ok ()

let test_manifest_aliases_canonicalize_across_modules = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, list_alias_source_id) = create_source seed_session ~kind:Source.File ~origin:(Source.Label "list_alias.ml")
    ~text:{ocaml|
      type 'value t = 'value list
    |ocaml}
  in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_list_alias =
    match Query.module_typings_of seed_snapshot list_alias_source_id with
    | Some typings -> typings
    | None -> panic "expected list_alias module typings"
  in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ loaded_list_alias ] in
  let session = Session.empty ~config in
  let (session, use_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "use.ml")
    ~text:{ocaml|
      let wrap (values: int list) : int List_alias.t = values

      let unwrap (values: int List_alias.t) : int list = values
    |ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ use_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot use_source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        let () = Test.assert_equal
          ~expected:true
          ~actual:(Option.is_some (export_scheme snapshot use_source_id "wrap")) in
        let () = Test.assert_equal
          ~expected:true
          ~actual:(Option.is_some (export_scheme snapshot use_source_id "unwrap")) in
        Ok ()

let test_paired_modules_accept_option_manifest_aliases_in_signature_inclusion = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, option_source_id) = create_source seed_session ~kind:Source.File ~origin:(Source.Label "option.ml")
    ~text:{ocaml|
      type 'value t = 'value option =
        | None
        | Some of 'value
    |ocaml}
  in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_option =
    match Query.module_typings_of seed_snapshot option_source_id with
    | Some typings -> typings
    | None -> panic "expected option module typings"
  in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ loaded_option ] in
  let session = Session.empty ~config in
  let (session, impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "env.ml")
    ~text:{ocaml|
      let normalize (value: string option) : string option = value
    |ocaml}
  in
  let (session, intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "env.mli")
    ~text:{ocaml|
      val normalize : string Option.t -> string option
    |ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ impl_source_id; intf_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let impl_diagnostics = diagnostic_strings snapshot impl_source_id in
      let intf_diagnostics = diagnostic_strings snapshot intf_source_id in
      if not (List.is_empty impl_diagnostics && List.is_empty intf_diagnostics) then
        Error (String.concat "\n" (impl_diagnostics @ intf_diagnostics))
      else if has_signature_error snapshot impl_source_id then
        Error "unexpected signature error in implementation"
      else if has_signature_error snapshot intf_source_id then
        Error "unexpected signature error in interface"
      else
        let impl_scheme = export_scheme snapshot impl_source_id "normalize" in
        let intf_scheme = export_scheme snapshot intf_source_id "normalize" in
        if impl_scheme != Some "string option -> string option" then
          Error ("unexpected implementation normalize type: " ^ show_option impl_scheme)
        else if intf_scheme != Some "string option -> string option" then
          Error ("unexpected interface normalize type: " ^ show_option intf_scheme)
        else
          Ok ()

let test_paired_modules_allow_private_top_level_exception_helpers = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "global0.ml")
    ~text:{ocaml|
      exception Panic of string

      let panic _ =
        let rec loop () = loop () in
        loop ()
    |ocaml}
  in
  let (session, intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "global0.mli")
    ~text:{ocaml|
      val panic : string -> 'a
    |ocaml}
  in
  let snapshot = Session.snapshot session in
  let impl_diagnostics = diagnostic_strings snapshot impl_source_id in
  let intf_diagnostics = diagnostic_strings snapshot intf_source_id in
  if not (List.is_empty impl_diagnostics) then
    Error (String.concat "\n" impl_diagnostics)
  else if not (List.is_empty intf_diagnostics) then
    Error (String.concat "\n" intf_diagnostics)
  else
    let impl_exports = export_names (Query.export_of snapshot impl_source_id) in
    let intf_exports = export_names (Query.export_of snapshot intf_source_id) in
    Test.assert_equal ~expected:[ "panic" ] ~actual:impl_exports;
    Test.assert_equal ~expected:[ "panic" ] ~actual:intf_exports;
    Test.assert_equal ~expected:false ~actual:(has_signature_error snapshot impl_source_id);
    Test.assert_equal ~expected:false ~actual:(has_signature_error snapshot intf_source_id);
    Ok ()

let test_paired_modules_include_sibling_exports_during_signature_inclusion = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _ops_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "ops.ml")
    ~text:{ocaml|
      let abs x = x
    |ocaml}
  in
  let (session, impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "global0.ml")
    ~text:{ocaml|
      include Ops

      let answer = abs 1
    |ocaml}
  in
  let (session, intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "global0.mli")
    ~text:{ocaml|
      val abs : int -> int

      val answer : int
    |ocaml}
  in
  let snapshot = Session.snapshot session in
  let impl_diagnostics = diagnostic_strings snapshot impl_source_id in
  let intf_diagnostics = diagnostic_strings snapshot intf_source_id in
  if not (List.is_empty impl_diagnostics) then
    Error (String.concat "\n" impl_diagnostics)
  else if not (List.is_empty intf_diagnostics) then
    Error (String.concat "\n" intf_diagnostics)
  else
    let impl_typings = Query.module_typings_of snapshot impl_source_id |> Option.expect ~msg:"missing impl typings" in
    let intf_typings = Query.module_typings_of snapshot intf_source_id |> Option.expect ~msg:"missing interface typings" in
    Test.assert_equal ~expected:false ~actual:(has_signature_error snapshot impl_source_id);
    Test.assert_equal ~expected:false ~actual:(has_signature_error snapshot intf_source_id);
    Test.assert_equal
      ~expected:[ "abs"; "answer" ]
      ~actual:(module_typings_export_names impl_typings);
    Test.assert_equal
      ~expected:[ "abs"; "answer" ]
      ~actual:(module_typings_export_names intf_typings);
    Test.assert_equal
      ~expected:[ "abs"; "answer" ]
      ~actual:(file_summary_export_names snapshot impl_source_id);
    Test.assert_equal
      ~expected:[ "abs"; "answer" ]
      ~actual:(file_summary_export_names snapshot intf_source_id);
    Ok ()

let test_paired_modules_include_paired_sibling_exports_during_signature_inclusion = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _ops_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "ops.ml")
    ~text:{ocaml|
      let abs x = x

      let answer = abs 1
    |ocaml}
  in
  let (session, _ops_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "ops.mli")
    ~text:{ocaml|
      val abs : int -> int

      val answer : int
    |ocaml}
  in
  let (session, impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "global0.ml")
    ~text:{ocaml|
      include Ops

      let total = abs answer
    |ocaml}
  in
  let (session, intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "global0.mli")
    ~text:{ocaml|
      val abs : int -> int

      val answer : int

      val total : int
    |ocaml}
  in
  let snapshot = Session.snapshot session in
  let impl_diagnostics = diagnostic_strings snapshot impl_source_id in
  let intf_diagnostics = diagnostic_strings snapshot intf_source_id in
  if not (List.is_empty impl_diagnostics) then
    Error (String.concat "\n" impl_diagnostics)
  else if not (List.is_empty intf_diagnostics) then
    Error (String.concat "\n" intf_diagnostics)
  else
    let impl_typings = Query.module_typings_of snapshot impl_source_id |> Option.expect ~msg:"missing impl typings" in
    let intf_typings = Query.module_typings_of snapshot intf_source_id |> Option.expect ~msg:"missing interface typings" in
    Test.assert_equal ~expected:false ~actual:(has_signature_error snapshot impl_source_id);
    Test.assert_equal ~expected:false ~actual:(has_signature_error snapshot intf_source_id);
    Test.assert_equal
      ~expected:[ "abs"; "answer"; "total" ]
      ~actual:(module_typings_export_names impl_typings);
    Test.assert_equal
      ~expected:[ "abs"; "answer"; "total" ]
      ~actual:(module_typings_export_names intf_typings);
    Test.assert_equal
      ~expected:[ "abs"; "answer"; "total" ]
      ~actual:(file_summary_export_names snapshot impl_source_id);
    Test.assert_equal
      ~expected:[ "abs"; "answer"; "total" ]
      ~actual:(file_summary_export_names snapshot intf_source_id);
    Ok ()

let test_source_input_hash_ignores_source_id_and_revision = fun _ctx ->
  let source_a = make_source
    ~source_id:(SourceId.of_int 0)
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~revision:1
    ~text:"let id x = x\n" in
  let source_b = make_source
    ~source_id:(SourceId.of_int 99)
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~revision:42
    ~text:"let id x = x\n" in
  let source_c = make_source
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

let test_source_input_hash_ignores_comments_and_docstrings = fun _ctx ->
  let source_a = make_source
    ~source_id:(SourceId.of_int 0)
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~revision:1
    ~text:"let id x = x\n" in
  let source_b = make_source
    ~source_id:(SourceId.of_int 1)
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~revision:2
    ~text:"(** docs *)\n(* banner *)\nlet id (* note *) x = x\n" in
  let hash_a = Source.input_hash source_a |> Crypto.Digest.hex in
  let hash_b = Source.input_hash source_b |> Crypto.Digest.hex in
  Test.assert_equal ~expected:hash_a ~actual:hash_b;
  Ok ()

let test_source_input_hash_changes_with_implicit_opens = fun _ctx ->
  let source_without_opens = make_source
    ~source_id:(SourceId.of_int 0)
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~revision:1
    ~text:"let id x = x\n" in
  let source_with_opens = make_source_with_implicit_opens
    ~implicit_opens:[ SurfacePath.of_string "Colors__Aliases" ]
    ~source_id:(SourceId.of_int 1)
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~revision:2
    ~text:"let id x = x\n" in
  let hash_without_opens = Source.input_hash source_without_opens |> Crypto.Digest.hex in
  let hash_with_opens = Source.input_hash source_with_opens |> Crypto.Digest.hex in
  if String.equal hash_without_opens hash_with_opens then
    Error "expected source input hash to change when implicit opens change"
  else
    Ok ()

let test_snapshot_uses_loaded_module_typings = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, colors_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"module RGB = struct let blend x y = x end\nlet to_string value = value\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_colors =
    match Query.module_typings_of seed_snapshot colors_source_id with
    | Some typings -> typings
    | None -> panic "expected seed module typings"
  in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ loaded_colors ] in
  let session = Session.empty ~config in
  let (session, demo_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "blend_demo.ml")
    ~text:"open Colors\nlet midpoint = RGB.blend 1 2\nlet label = to_string \"ok\"\n" in
  let snapshot = Session.snapshot session in
  let demo_has_unbound_name = has_unbound_name snapshot demo_source_id in
  let midpoint_type = inferred_type_at snapshot demo_source_id 34 in
  let label_type = inferred_type_at snapshot demo_source_id 58 in
  let summary_modules =
    module_typings_jsons snapshot
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
  Test.assert_equal ~expected:[ "Blend_demo" ] ~actual:summary_modules;
  if demo_has_unbound_name then
    Error (String.concat "\n" (diagnostic_strings snapshot demo_source_id))
  else (
    Test.assert_equal ~expected:(Some "int -> int -> int") ~actual:midpoint_type;
    Test.assert_equal ~expected:(Some "string -> string") ~actual:label_type;
    Ok ()
  )

let test_prepare_snapshot_hydrates_module_typings_from_store = fun _ctx ->
  with_typ_store
    (fun store ->
      let seed_session = Session.empty ~config:Config.default in
      let (seed_session, colors_source_id) = create_source
        seed_session
        ~kind:Source.File
        ~origin:(Source.Label "colors.ml")
        ~text:"module RGB = struct let blend x y = x end\nlet to_string value = value\n" in
      let seed_snapshot = Session.snapshot seed_session in
      let loaded_colors =
        match Query.module_typings_of seed_snapshot colors_source_id with
        | Some typings -> typings
        | None -> panic "expected seed module typings"
      in
      let _ = Store.save_module_typings store loaded_colors |> Result.expect ~msg:"save_module_typings should succeed" in
      let config = Config.default |> Config.with_store ~store:(Some store) in
      let session = Session.empty ~config in
      let (session, demo_source_id) = create_source
        session
        ~kind:Source.File
        ~origin:(Source.Label "blend_demo.ml")
        ~text:"open Colors\nlet midpoint = RGB.blend 1 2\nlet label = to_string \"ok\"\n" in
      match Session.prepare_snapshot session ~roots:[ demo_source_id ] with
      | Error missing -> Error (format
        Format.[
          str "expected store-backed snapshot preparation to succeed, got ";
          str (Data.Json.to_string (Session.MissingRequirements.to_json missing));
        ])
      | Ok snapshot ->
          let diagnostics = diagnostic_strings snapshot demo_source_id in
          if not (List.is_empty diagnostics) then
            Error (String.concat "\n" diagnostics)
          else
            let midpoint_type = inferred_type_at snapshot demo_source_id 34 in
            let label_type = inferred_type_at snapshot demo_source_id 58 in
            Test.assert_equal ~expected:(Some "int -> int -> int") ~actual:midpoint_type;
            Test.assert_equal ~expected:(Some "string -> string") ~actual:label_type;
            Ok ())

let test_prepare_snapshot_keeps_store_read_only_during_query_forcing = fun _ctx ->
  with_typ_store
    (fun store ->
      let config = Config.default |> Config.with_store ~store:(Some store) in
      let session = Session.empty ~config in
      let (session, source_id) = create_source
        session
        ~kind:Source.File
        ~origin:(Source.Label "single.ml")
        ~text:"let value = 42\n" in
      match Session.prepare_snapshot session ~roots:[ source_id ] with
      | Error missing -> Error (format
        Format.[
          str "expected store-backed snapshot preparation to succeed, got ";
          str (Data.Json.to_string (Session.MissingRequirements.to_json missing));
        ])
      | Ok snapshot ->
          let diagnostics = diagnostic_strings snapshot source_id in
          let before = Store.load_module_typings store ~module_name:"Single" in
          let value_type = export_scheme snapshot source_id "value" in
          let _ = Session.Snapshot.find_module_typings_by_name snapshot "Single" in
          let rooted_typings = Session.Snapshot.module_typings snapshot in
          let after = Store.load_module_typings store ~module_name:"Single" in
          if Option.is_some before then
            Error "expected module typings store to start empty"
          else if not (List.is_empty diagnostics) then
            Error (String.concat "\n" diagnostics)
          else if value_type <> Some "int" then
            Error ("unexpected rooted export type: " ^ show_option value_type)
          else if List.map ModuleTypings.module_name rooted_typings <> [ "Single" ] then
            Error ("unexpected rooted module typings: "
            ^ String.concat ", " (List.map ModuleTypings.module_name rooted_typings))
          else if Option.is_some after then
            Error "expected rooted snapshot queries to leave canonical module persistence to the build path"
          else
            Ok ())

let test_fold_package_sources_persists_module_typings_from_authoritative_engine = fun _ctx ->
  with_typ_store
    (fun store ->
      let config = Config.default |> Config.with_store ~store:(Some store) in
      let source_a = prepared_check_source
        ~source_id:(SourceId.of_int 0)
        ~filename:"a.ml"
        ~internal_module_name:"A"
        ~local_module_name:"A"
        ~public_module_name:None
        ~text:"let answer = 42\n" in
      let source_b = prepared_check_source
        ~source_id:(SourceId.of_int 1)
        ~filename:"b.ml"
        ~internal_module_name:"B"
        ~local_module_name:"B"
        ~public_module_name:None
        ~text:"let use = A.answer\n" in
      let before_a = Store.load_module_typings store ~module_name:"A" in
      let before_b = Store.load_module_typings store ~module_name:"B" in
      match Check.fold_package_sources
        ~config
        ~ordered_sources:[ source_b; source_a ]
        ~init:[]
        ~f:(fun groups (group: Check.finished_group) -> group :: groups)
        () with
      | Error Check.MissingRequirements { module_name; requirements } ->
          Error (format
            Format.[
              str "unexpected missing requirements while checking ";
              str (LocalModules.InternalName.to_string module_name);
              str ": ";
              str (Data.Json.to_string (Session.MissingRequirements.to_json requirements));
            ])
      | Error Check.MissingModuleTypings { module_name } ->
          Error (format
            Format.[
              str "missing authoritative module typings for ";
              str (LocalModules.InternalName.to_string module_name);
            ])
      | Error Check.MissingAnalysis { module_name; path } ->
          Error (format
            Format.[
              str "missing checked analysis for ";
              str (LocalModules.InternalName.to_string module_name);
              str " at ";
              str (Path.to_string path);
            ])
      | Error Check.StoreFailure { module_name; reason } ->
          Error (format
            Format.[
              str "while persisting module typings for ";
              str (LocalModules.InternalName.to_string module_name);
              str ": ";
              str reason;
            ])
      | Error Check.PackageStoreFailure { package_name; reason } ->
          Error (format
            Format.[
              str "while persisting package bundle for ";
              str package_name;
              str ": ";
              str reason;
            ])
      | Ok result ->
          let groups = List.rev result.acc in
          let after_a = Store.load_module_typings store ~module_name:"A" in
          let after_b = Store.load_module_typings store ~module_name:"B" in
          if Option.is_some before_a || Option.is_some before_b then
            Error "expected module typings store to start empty"
          else
            match (after_a, after_b) with
            | (Some a_typings, Some b_typings) ->
                Test.assert_equal
                  ~expected:[ "A"; "B" ]
                  ~actual:(groups
                  |> List.map
                    (fun (group: Check.finished_group) -> LocalModules.InternalName.to_string group.module_name));
                Test.assert_equal
                  ~expected:[ "answer" ]
                  ~actual:(module_typings_export_names a_typings);
                Test.assert_equal ~expected:[ "use" ] ~actual:(module_typings_export_names b_typings);
                Ok ()
            | (None, Some _) ->
                Error "expected authoritative build checking to persist sibling dependency module typings"
            | (Some _, None) ->
                Error "expected authoritative build checking to persist rooted module typings"
            | (None, None) ->
                Error "expected authoritative build checking to persist module typings as modules finish")

let test_fold_package_sources_resolves_contextual_local_modules = fun _ctx ->
  let adapter = prepared_check_source
    ~source_id:(SourceId.of_int 0)
    ~filename:"async/adapter.ml"
    ~internal_module_name:"Kernel_new__Async__Adapter"
    ~local_module_name:"Async.Adapter"
    ~public_module_name:None
    ~text:"let id value = value\n" in
  let source = prepared_check_source
    ~source_id:(SourceId.of_int 1)
    ~filename:"async/source.ml"
    ~internal_module_name:"Kernel_new__Async__Source"
    ~local_module_name:"Async.Source"
    ~public_module_name:None
    ~text:"let run value = Adapter.id value\n" in
  let config = Config.default |> Config.with_capture_traces ~capture_traces:false in
  match Check.fold_package_sources
    ~config
    ~ordered_sources:[ source; adapter ]
    ~init:[]
    ~f:(fun groups (group: Check.finished_group) -> group :: groups)
    () with
  | Error Check.MissingRequirements { module_name; requirements } ->
      Error (format
        Format.[
          str "unexpected missing requirements while checking ";
          str (LocalModules.InternalName.to_string module_name);
          str ": ";
          str (Data.Json.to_string (Session.MissingRequirements.to_json requirements));
        ])
  | Error Check.MissingModuleTypings { module_name } ->
      Error (format
        Format.[
          str "missing module typings for ";
          str (LocalModules.InternalName.to_string module_name);
        ])
  | Error Check.MissingAnalysis { module_name; path } ->
      Error (format
        Format.[
          str "missing analysis for ";
          str (LocalModules.InternalName.to_string module_name);
          str " at ";
          str (Path.to_string path);
        ])
  | Error Check.StoreFailure { module_name; reason } ->
      Error (format
        Format.[
          str "store failure for ";
          str (LocalModules.InternalName.to_string module_name);
          str ": ";
          str reason;
        ])
  | Error Check.PackageStoreFailure { package_name; reason } ->
      Error (format
        Format.[ str "package store failure for "; str package_name; str ": "; str reason; ])
  | Ok result ->
      let groups = List.rev result.acc in
      let loaded_modules = result.loaded_modules in
      let module_order = groups
      |> List.map
        (fun (group: Check.finished_group) -> LocalModules.InternalName.to_string group.module_name) in
      let expected_order = [ "Kernel_new__Async__Adapter"; "Kernel_new__Async__Source" ] in
      let source_group =
        groups
        |> List.find_opt
          (fun (group: Check.finished_group) ->
            String.equal (LocalModules.InternalName.to_string group.module_name) "Kernel_new__Async__Source")
        |> Option.expect ~msg:"expected Async.Source group"
      in
      let source_analysis = source_group.checked_sources
      |> List.find_opt
        (fun (checked_source: Check.checked_source) -> Path.to_string checked_source.path = "async/source.ml")
      |> Option.map (fun (checked_source: Check.checked_source) -> checked_source.analysis)
      |> Option.expect ~msg:"expected Async.Source analysis" in
      let export_names = FileSummary.exports source_analysis.file_summary
      |> List.map (fun (name, _scheme) -> SurfacePath.to_string name) in
      Test.assert_equal ~expected:expected_order ~actual:module_order;
      Test.assert_equal
        ~expected:[]
        ~actual:(List.map Diagnostic.to_string source_analysis.typing_diagnostics);
      Test.assert_equal ~expected:[ "run" ] ~actual:export_names;
      Test.assert_equal
        ~expected:true
        ~actual:(LoadedModules.contains
          loaded_modules
          ~required_name:(LocalModules.RequiredName.of_string "Kernel_new__Async__Adapter"));
      Test.assert_equal
        ~expected:true
        ~actual:(LoadedModules.contains
          loaded_modules
          ~required_name:(LocalModules.RequiredName.of_string "Kernel_new__Async__Source"));
      Ok ()

let test_fold_package_sources_resolves_root_local_module_wrappers = fun _ctx ->
  let async_root = prepared_check_source
    ~source_id:(SourceId.of_int 0)
    ~filename:"async.ml"
    ~internal_module_name:"Kernel_new__Async"
    ~local_module_name:"Async"
    ~public_module_name:None
    ~text:"let answer = 42\n" in
  let consumer = prepared_check_source
    ~source_id:(SourceId.of_int 1)
    ~filename:"consumer.ml"
    ~internal_module_name:"Kernel_new__Consumer"
    ~local_module_name:"Consumer"
    ~public_module_name:None
    ~text:"let run = Async.answer\n" in
  let config = Config.default |> Config.with_capture_traces ~capture_traces:false in
  match Check.fold_package_sources
    ~config
    ~ordered_sources:[ consumer; async_root ]
    ~init:[]
    ~f:(fun groups (group: Check.finished_group) -> group :: groups)
    () with
  | Error Check.MissingRequirements { module_name; requirements } ->
      Error (format
        Format.[
          str "unexpected missing requirements while checking ";
          str (LocalModules.InternalName.to_string module_name);
          str ": ";
          str (Data.Json.to_string (Session.MissingRequirements.to_json requirements));
        ])
  | Error Check.MissingModuleTypings { module_name } ->
      Error (format
        Format.[
          str "missing module typings for ";
          str (LocalModules.InternalName.to_string module_name);
        ])
  | Error Check.MissingAnalysis { module_name; path } ->
      Error (format
        Format.[
          str "missing analysis for ";
          str (LocalModules.InternalName.to_string module_name);
          str " at ";
          str (Path.to_string path);
        ])
  | Error Check.StoreFailure { module_name; reason } ->
      Error (format
        Format.[
          str "store failure for ";
          str (LocalModules.InternalName.to_string module_name);
          str ": ";
          str reason;
        ])
  | Error Check.PackageStoreFailure { package_name; reason } ->
      Error (format
        Format.[ str "package store failure for "; str package_name; str ": "; str reason; ])
  | Ok result ->
      let groups = List.rev result.acc in
      let loaded_modules = result.loaded_modules in
      let module_order = groups
      |> List.map
        (fun (group: Check.finished_group) -> LocalModules.InternalName.to_string group.module_name) in
      let consumer_group =
        groups
        |> List.find_opt
          (fun (group: Check.finished_group) ->
            String.equal (LocalModules.InternalName.to_string group.module_name) "Kernel_new__Consumer")
        |> Option.expect ~msg:"expected Kernel_new__Consumer group"
      in
      let consumer_analysis = consumer_group.checked_sources
      |> List.find_opt
        (fun (checked_source: Check.checked_source) -> Path.to_string checked_source.path = "consumer.ml")
      |> Option.map (fun (checked_source: Check.checked_source) -> checked_source.analysis)
      |> Option.expect ~msg:"expected consumer analysis" in
      let export_names = FileSummary.exports consumer_analysis.file_summary
      |> List.map (fun (name, _scheme) -> SurfacePath.to_string name) in
      Test.assert_equal ~expected:[ "Kernel_new__Async"; "Kernel_new__Consumer" ] ~actual:module_order;
      Test.assert_equal
        ~expected:[]
        ~actual:(List.map Diagnostic.to_string consumer_analysis.typing_diagnostics);
      Test.assert_equal ~expected:[ "run" ] ~actual:export_names;
      Test.assert_equal
        ~expected:true
        ~actual:(LoadedModules.contains
          loaded_modules
          ~required_name:(LocalModules.RequiredName.of_string "Kernel_new__Async"));
      Test.assert_equal
        ~expected:true
        ~actual:(LoadedModules.contains
          loaded_modules
          ~required_name:(LocalModules.RequiredName.of_string "Kernel_new__Consumer"));
      Ok ()

let test_fold_package_sources_shares_imported_world_semantics_for_open_alias_include = fun _ctx ->
  let helpers = prepared_check_source
    ~source_id:(SourceId.of_int 0)
    ~filename:"helpers.ml"
    ~internal_module_name:"Helpers"
    ~local_module_name:"Helpers"
    ~public_module_name:None
    ~text:{ocaml|
type t = Wrap of int
type record = { field : int }

let make value = Wrap value
let project ({ field } : record) = field

module Nested = struct
  let nested_value = 7
end
|ocaml} in
  let consumer = prepared_check_source
    ~source_id:(SourceId.of_int 1)
    ~filename:"consumer.ml"
    ~internal_module_name:"Consumer"
    ~local_module_name:"Consumer"
    ~public_module_name:None
    ~text:{ocaml|
open Helpers
module Alias = Helpers
include Helpers.Nested

let from_alias = match Alias.make 1 with Wrap value -> value
let from_ctor = match Wrap 2 with Wrap value -> value
let from_record = project { field = 3 }
let from_include = nested_value + 1
let via_local_open = Helpers.(match make 4 with Wrap value -> value)
let from_alias_type (value : Alias.record) = project value
|ocaml} in
  let config = Config.default |> Config.with_capture_traces ~capture_traces:false in
  match Check.fold_package_sources
    ~config
    ~ordered_sources:[ consumer; helpers ]
    ~init:[]
    ~f:(fun groups (group: Check.finished_group) -> group :: groups)
    () with
  | Error Check.MissingRequirements { module_name; requirements } ->
      Error (format
        Format.[
          str "unexpected missing requirements while checking ";
          str (LocalModules.InternalName.to_string module_name);
          str ": ";
          str (Data.Json.to_string (Session.MissingRequirements.to_json requirements));
        ])
  | Error Check.MissingModuleTypings { module_name } ->
      Error (format Format.[ str "missing module typings for "; str (LocalModules.InternalName.to_string module_name) ])
  | Error Check.MissingAnalysis { module_name; path } ->
      Error (format
        Format.[
          str "missing analysis for ";
          str (LocalModules.InternalName.to_string module_name);
          str " at ";
          str (Path.to_string path);
        ])
  | Error Check.StoreFailure { module_name; reason } ->
      Error (format
        Format.[
          str "store failure for ";
          str (LocalModules.InternalName.to_string module_name);
          str ": ";
          str reason;
        ])
  | Error Check.PackageStoreFailure { package_name; reason } ->
      Error (format Format.[ str "package store failure for "; str package_name; str ": "; str reason; ])
  | Ok result ->
      let groups = List.rev result.acc in
      let consumer_analysis =
        groups
        |> List.find_opt
          (fun (group: Check.finished_group) ->
            String.equal (LocalModules.InternalName.to_string group.module_name) "Consumer")
        |> Option.expect ~msg:"expected Consumer group"
        |> fun (group: Check.finished_group) ->
          group.checked_sources
          |> List.find_opt
            (fun (checked_source: Check.checked_source) -> Path.to_string checked_source.path = "consumer.ml")
          |> Option.map (fun (checked_source: Check.checked_source) -> checked_source.analysis)
          |> Option.expect ~msg:"expected Consumer analysis"
      in
      let exports = FileSummary.exports consumer_analysis.file_summary in
      let export_scheme name = lookup_export name exports |> Option.map TypePrinter.scheme_to_string in
      let diagnostics = List.map Diagnostic.to_string consumer_analysis.typing_diagnostics in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else if Option.is_none (lookup_export "from_alias_type" exports) then
        Error "expected alias-qualified type export on build path"
      else
        let () = Test.assert_equal ~expected:(Some "int") ~actual:(export_scheme "nested_value") in
        let () = Test.assert_equal ~expected:(Some "int") ~actual:(export_scheme "from_alias") in
        let () = Test.assert_equal ~expected:(Some "int") ~actual:(export_scheme "from_ctor") in
        let () = Test.assert_equal ~expected:(Some "int") ~actual:(export_scheme "from_record") in
        let () = Test.assert_equal ~expected:(Some "int") ~actual:(export_scheme "from_include") in
        let () = Test.assert_equal ~expected:(Some "int") ~actual:(export_scheme "via_local_open") in
        Ok ()

let test_prepare_snapshot_shares_imported_world_semantics_for_open_alias_include = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _helpers_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "helpers.ml")
    ~text:{ocaml|
type t = Wrap of int
type record = { field : int }

let make value = Wrap value
let project ({ field } : record) = field

module Nested = struct
  let nested_value = 7
end
|ocaml} in
  let (session, consumer_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "consumer.ml")
    ~text:{ocaml|
open Helpers
module Alias = Helpers
include Helpers.Nested

let from_alias = match Alias.make 1 with Wrap value -> value
let from_ctor = match Wrap 2 with Wrap value -> value
let from_record = project { field = 3 }
let from_include = nested_value + 1
let via_local_open = Helpers.(match make 4 with Wrap value -> value)
let from_alias_type (value : Alias.record) = project value
|ocaml} in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot consumer_source_id in
  let exported_names = export_names (Query.export_of snapshot consumer_source_id) in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else if not (List.mem "from_alias_type" exported_names) then
    Error "expected alias-qualified type export on snapshot path"
  else
    let () = Test.assert_equal ~expected:(Some "int") ~actual:(export_scheme snapshot consumer_source_id "nested_value") in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:(export_scheme snapshot consumer_source_id "from_alias") in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:(export_scheme snapshot consumer_source_id "from_ctor") in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:(export_scheme snapshot consumer_source_id "from_record") in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:(export_scheme snapshot consumer_source_id "from_include") in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:(export_scheme snapshot consumer_source_id "via_local_open") in
    Ok ()

let test_fold_package_sources_persists_package_bundle = fun _ctx ->
  with_typ_store
    (fun store ->
      let source = prepared_check_source
        ~source_id:(SourceId.of_int 0)
        ~filename:"async.ml"
        ~internal_module_name:"Kernel_new__Async"
        ~local_module_name:"Async"
        ~public_module_name:(Some "Async")
        ~text:"let answer = 42\n" in
      let package_fingerprint = Crypto.hash_string "kernel-new:async" in
      let config = Config.default
      |> Config.with_store ~store:(Some store)
      |> Config.with_capture_traces ~capture_traces:false in
      match Check.fold_package_sources
        ~package_name:"kernel-new"
        ~package_fingerprint
        ~config
        ~ordered_sources:[ source ]
        ~init:[]
        ~f:(fun groups (group: Check.finished_group) -> group :: groups)
        () with
      | Error Check.MissingRequirements { module_name; requirements } ->
          Error (format
            Format.[
              str "unexpected missing requirements while checking ";
              str (LocalModules.InternalName.to_string module_name);
              str ": ";
              str (Data.Json.to_string (Session.MissingRequirements.to_json requirements));
            ])
      | Error Check.MissingModuleTypings { module_name } ->
          Error (format
            Format.[
              str "missing module typings for ";
              str (LocalModules.InternalName.to_string module_name);
            ])
      | Error Check.MissingAnalysis { module_name; path } ->
          Error (format
            Format.[
              str "missing analysis for ";
              str (LocalModules.InternalName.to_string module_name);
              str " at ";
              str (Path.to_string path);
            ])
      | Error Check.StoreFailure { module_name; reason } ->
          Error (format
            Format.[
              str "store failure for ";
              str (LocalModules.InternalName.to_string module_name);
              str ": ";
              str reason;
            ])
      | Error Check.PackageStoreFailure { package_name; reason } ->
          Error (format
            Format.[ str "package store failure for "; str package_name; str ": "; str reason; ])
      | Ok result -> (
          match Store.load_package_bundle store ~package_name:"kernel-new" with
          | None -> Error "expected persisted package bundle"
          | Some bundle ->
              let package_modules = bundle.typings |> List.map ModuleTypings.module_name in
              let returned_modules = result.public_module_typings
              |> LoadedModules.values
              |> List.map ModuleTypings.module_name in
              Test.assert_equal
                ~expected:(Crypto.Digest.hex package_fingerprint)
                ~actual:(Crypto.Digest.hex bundle.fingerprint);
              Test.assert_equal ~expected:[ "Async" ] ~actual:package_modules;
              Test.assert_equal ~expected:[ "Async" ] ~actual:returned_modules;
              Ok ()
        ))

let test_fold_package_sources_keeps_base_loaded_modules_immutable = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, dep_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "dep.mli")
    ~text:"val id: 'a -> 'a\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let dep_typings =
    match Query.module_typings_of seed_snapshot dep_source_id with
    | Some typings -> typings
    | None -> panic "expected dep module typings"
  in
  let config = Config.default
  |> Config.with_loaded_modules ~loaded_modules:[ dep_typings ]
  |> Config.with_capture_traces ~capture_traces:false in
  let source = prepared_check_source
    ~source_id:(SourceId.of_int 0)
    ~filename:"async.ml"
    ~internal_module_name:"Kernel_new__Async"
    ~local_module_name:"Async"
    ~public_module_name:(Some "Async")
    ~text:"let answer = 42\n" in
  match Check.fold_package_sources
    ~config
    ~ordered_sources:[ source ]
    ~init:[]
    ~f:(fun groups (group: Check.finished_group) -> group :: groups)
    () with
  | Error Check.MissingRequirements { module_name; requirements } ->
      Error (format
        Format.[
          str "unexpected missing requirements while checking ";
          str (LocalModules.InternalName.to_string module_name);
          str ": ";
          str (Data.Json.to_string (Session.MissingRequirements.to_json requirements));
        ])
  | Error Check.MissingModuleTypings { module_name } ->
      Error (format
        Format.[
          str "missing module typings for ";
          str (LocalModules.InternalName.to_string module_name);
        ])
  | Error Check.MissingAnalysis { module_name; path } ->
      Error (format
        Format.[
          str "missing analysis for ";
          str (LocalModules.InternalName.to_string module_name);
          str " at ";
          str (Path.to_string path);
        ])
  | Error Check.StoreFailure { module_name; reason } ->
      Error (format
        Format.[
          str "store failure for ";
          str (LocalModules.InternalName.to_string module_name);
          str ": ";
          str reason;
        ])
  | Error Check.PackageStoreFailure { package_name; reason } ->
      Error (format
        Format.[ str "package store failure for "; str package_name; str ": "; str reason; ])
  | Ok result ->
      let base_module_names = config.loaded_modules
      |> LoadedModules.values
      |> List.map ModuleTypings.module_name
      |> List.sort String.compare in
      let result_module_names = result.loaded_modules
      |> LoadedModules.values
      |> List.map ModuleTypings.module_name
      |> List.sort String.compare in
      Test.assert_equal ~expected:[ "Dep" ] ~actual:base_module_names;
      Test.assert_equal ~expected:[ "Dep"; "Kernel_new__Async" ] ~actual:result_module_names;
      Ok ()

let test_prepare_snapshot_emits_structured_events = fun _ctx ->
  let events = ref [] in
  let config = Config.default
  |> Config.with_on_event ~on_event:(fun event -> events := !events @ [ event ]) in
  let session = Session.empty ~config in
  let (session, source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "demo.ml")
    ~text:{ocaml|
      let answer = 1
    |ocaml}
  in
  match Session.prepare_snapshot session ~roots:[ source_id ] with
  | Error missing -> Error (format
    Format.[
      str "expected rooted snapshot, got ";
      str (Data.Json.to_string (Session.MissingRequirements.to_json missing));
    ])
  | Ok snapshot ->
      let _ = Query.analysis_of_source snapshot source_id |> Option.expect ~msg:"expected analysis" in
      if not (typ_event_instants_are_monotonic !events) then
        Error (format
          Format.[ str "expected monotonic typ event instants: "; str (typ_events_json !events); ])
      else
        match !events with
        | [
          { Event.kind=Event.PrepareSnapshotStarted _; _ };
          { Event.kind=Event.PrepareSnapshotFinished _; _ };
          { Event.kind=Event.SnapshotMaterializationStarted _; _ };
          { Event.kind=Event.SnapshotMaterializationFinished _; _ };
          { Event.kind=Event.ModulePairingStarted _; _ };
          {
            Event.kind=Event.SourceAnalysisStarted {
              source_id=analysis_source_id;
              module_name;
              mode=Event.SnapshotAnalysis;
              _
            };
            _
          };
          { Event.kind=Event.SourceAnalysisFinished {
              source_id=finished_source_id;
              module_name=finished_module_name;
              mode=Event.SnapshotAnalysis;
              export_status=Event.TrustedExport;
              _
            }; _ };
          { Event.kind=Event.ModulePairingFinished {
              module_name=paired_module_name;
              export_status=Event.TrustedExport;
              export_count=1;
              type_decl_count=0;
              _
            }; _ };

        ] ->
            let expected_source_id = SourceId.to_int source_id in
            if
              List.for_all
                (fun actual_source_id -> SourceId.to_int actual_source_id = expected_source_id)
                [ analysis_source_id; finished_source_id ]
              && List.for_all
                (fun actual_module_name ->
                  String.equal actual_module_name "Demo")
                [ module_name; finished_module_name; paired_module_name; ]
            then
              Ok ()
            else
              Error (format
                Format.[
                  str "unexpected structured event payloads: ";
                  str (typ_events_json !events);
                ])
        | _ -> Error (format
          Format.[ str "unexpected typ event payloads: "; str (typ_events_json !events); ])

let test_prepare_snapshot_emits_structured_diagnostics_in_events = fun _ctx ->
  let events = ref [] in
  let config = Config.default
  |> Config.with_on_event ~on_event:(fun event -> events := !events @ [ event ]) in
  let session = Session.empty ~config in
  let (session, source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "demo.ml")
    ~text:{ocaml|
      let answer = missing_value
    |ocaml}
  in
  match Session.prepare_snapshot session ~roots:[ source_id ] with
  | Error missing -> Error (format
    Format.[
      str "expected rooted snapshot, got ";
      str (Data.Json.to_string (Session.MissingRequirements.to_json missing));
    ])
  | Ok snapshot ->
      let _ = Query.analysis_of_source snapshot source_id |> Option.expect ~msg:"expected analysis" in
      match !events with
      | [
        { Event.kind=Event.PrepareSnapshotStarted _; _ };
        { Event.kind=Event.PrepareSnapshotFinished _; _ };
        { Event.kind=Event.ModulePairingStarted _; _ };
        { Event.kind=Event.SourceAnalysisStarted _; _ };
        { Event.kind=Event.SourceAnalysisFinished {
            source_id=finished_source_id;
            module_name;
            mode=Event.SnapshotAnalysis;
            parse_diagnostics=[];
            lowering_diagnostics=[];
            typing_diagnostics=[ _ ];
            parse_diagnostic_count=0;
            lowering_diagnostic_count=0;
            typing_diagnostic_count=1;
            export_status=Event.ErroredExport;
            _;

          }; _;  };
        { Event.kind=Event.ModulePairingFinished _; _ };

      ] when SourceId.equal finished_source_id source_id && String.equal module_name "Demo" -> Ok ()
      | _ -> Error (format
        Format.[
          str "expected structured typing diagnostics in event payloads, got ";
          str (typ_events_json !events);
        ])

let test_prepare_snapshot_keeps_imported_value_payloads_out_of_ambient_bindings = fun _ctx ->
  let events = ref [] in
  let config = Config.default
  |> Config.with_on_event ~on_event:(fun event -> events := !events @ [ event ]) in
  let session = Session.empty ~config in
  let helpers_text = {ocaml|
    let value0 = 0
    let value1 = 1
    let value2 = 2
    let value3 = 3
    let value4 = 4
    let value5 = 5
    let value6 = 6
    let value7 = 7
    let value8 = 8
    let value9 = 9
  |ocaml}
  in
  let (session, _helpers_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "helpers.ml")
    ~text:helpers_text in
  let consumer_text = {ocaml|
    open Helpers

    let answer = value0 + value9
  |ocaml}
  in
  let (session, consumer_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "consumer.ml")
    ~text:consumer_text in
  match Session.prepare_snapshot session ~roots:[ consumer_source_id ] with
  | Error missing -> Error (format
    Format.[
      str "expected rooted snapshot, got ";
      str (Data.Json.to_string (Session.MissingRequirements.to_json missing));
    ])
  | Ok snapshot ->
      let answer_scheme = export_scheme snapshot consumer_source_id "answer" in
      let consumer_starts =
        !events
        |> List.filter_map
          (fun ({ Event.kind; _ }: Event.t) ->
            match kind with
            | Event.SourceAnalysisStarted {
                source_id;
                module_name;
                mode=Event.SnapshotAnalysis;
                ambient_binding_count;
                ambient_type_decl_count;
                _;
              }
              when SourceId.equal source_id consumer_source_id ->
                Some (module_name, ambient_binding_count, ambient_type_decl_count)
            | _ -> None)
      in
      match consumer_starts with
      | [ ("Consumer", 0, 0) ] ->
          Test.assert_equal ~expected:(Some "int") ~actual:answer_scheme;
          Ok ()
      | _ -> Error (format
        Format.[
          str "expected consumer snapshot analysis to avoid imported ambient value payloads, got ";
          str (typ_events_json !events);
        ])

let test_prepare_snapshot_keeps_diagnostics_and_exports_stable_after_module_forcing = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _helpers_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "helpers.ml")
    ~text:{ocaml|
      type t = Wrap of int

      let make value = Wrap value
    |ocaml} in
  let consumer_text = {ocaml|
    open Helpers

    let ok = match make 1 with Wrap value -> value
    let broken = missing_value
  |ocaml}
  in
  let (session, consumer_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "consumer.ml")
    ~text:consumer_text in
  match prepare_snapshot_or_error session ~roots:[ consumer_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics_before = diagnostic_strings snapshot consumer_source_id in
      let had_unbound_name_before = has_unbound_name snapshot consumer_source_id in
      let ok_export_before = export_scheme snapshot consumer_source_id "ok" in
      let exports_before = export_names (Query.export_of snapshot consumer_source_id) in
      let _ = Session.Snapshot.find_module_typings_by_name snapshot "Helpers" in
      let _ = Session.Snapshot.module_typings snapshot in
      let _ = Query.module_typings_of snapshot consumer_source_id in
      let diagnostics_after = diagnostic_strings snapshot consumer_source_id in
      let had_unbound_name_after = has_unbound_name snapshot consumer_source_id in
      let ok_export_after = export_scheme snapshot consumer_source_id "ok" in
      let exports_after = export_names (Query.export_of snapshot consumer_source_id) in
      Test.assert_equal ~expected:true ~actual:had_unbound_name_before;
      Test.assert_equal ~expected:true ~actual:had_unbound_name_after;
      Test.assert_equal ~expected:diagnostics_before ~actual:diagnostics_after;
      Test.assert_equal ~expected:exports_before ~actual:exports_after;
      Test.assert_equal ~expected:ok_export_before ~actual:ok_export_after;
      Test.assert_equal ~expected:(Some "int") ~actual:ok_export_after;
      Ok ()

let test_prepare_snapshot_only_pairs_required_local_modules = fun _ctx ->
  let events = ref [] in
  let config = Config.default
  |> Config.with_on_event ~on_event:(fun event -> events := !events @ [ event ]) in
  let session = Session.empty ~config in
  let (session, _dep_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "dep.ml")
    ~text:{ocaml|
      let value = 1
    |ocaml}
  in
  let (session, _unused_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "unused.ml")
    ~text:{ocaml|
      let hidden = 2
    |ocaml}
  in
  let (session, app_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "app.ml")
    ~text:{ocaml|
      open Dep

      let answer = value
    |ocaml}
  in
  match Session.prepare_snapshot session ~roots:[ app_source_id ] with
  | Error missing -> Error (format
    Format.[
      str "expected rooted snapshot, got ";
      str (Data.Json.to_string (Session.MissingRequirements.to_json missing));
    ])
  | Ok snapshot ->
      let _ = Query.analysis_of_source snapshot app_source_id |> Option.expect ~msg:"expected analysis" in
      let paired_modules =
        !events
        |> List.filter_map
          (fun (event: Event.t) ->
            match Event.(event.kind) with
            | Event.ModulePairingStarted { module_name; _ } -> Some module_name
            | _ -> None)
        |> List.sort_uniq String.compare
      in
      if List.mem "Unused" paired_modules then
        Error (format
          Format.[ str "unexpected unrelated module pairing: "; str (typ_events_json !events); ])
      else (
        Test.assert_equal ~expected:[ "App"; "Dep" ] ~actual:paired_modules;
        Ok ()
      )

let test_prepare_snapshot_reuses_shared_transitive_local_modules = fun _ctx ->
  let events = ref [] in
  let config = Config.default
  |> Config.with_on_event ~on_event:(fun event -> events := !events @ [ event ]) in
  let session = Session.empty ~config in
  let base_text = {ocaml|
    let value = 1
  |ocaml}
  in
  let base_parse_result = Syn.parse ~filename:(Path.v "base.ml") base_text in
  let base_cst = expect_cst ~filename:"base.ml" base_parse_result in
  let (session, _base_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~module_name:"Pkg__Base"
    ~implicit_opens:[]
    ~origin:(Source.Label "base.ml")
    ~source_hash:(Source.hash ~implicit_opens:[] ~cst:base_cst)
    ~parse_result:base_parse_result
    ~cst:base_cst in
  let (session, _left_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "left.ml")
    ~text:{ocaml|
      open Base

      let left = value
    |ocaml}
  in
  let (session, _right_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "right.ml")
    ~text:{ocaml|
      open Base

      let right = value
    |ocaml}
  in
  let (session, app_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "app.ml")
    ~text:{ocaml|
      let total = Left.left + Right.right
    |ocaml}
  in
  match Session.prepare_snapshot session ~roots:[ app_source_id ] with
  | Error missing -> Error (format
    Format.[
      str "expected rooted snapshot, got ";
      str (Data.Json.to_string (Session.MissingRequirements.to_json missing));
    ])
  | Ok snapshot ->
      let _ = Query.analysis_of_source snapshot app_source_id |> Option.expect ~msg:"expected analysis" in
      let snapshot_analysis_counts =
        !events
        |> List.fold_left
          (fun counts (event: Event.t) ->
            match Event.(event.kind) with
            | Event.SourceAnalysisStarted { module_name; mode=Event.SnapshotAnalysis; _ } ->
                let count = List.assoc_opt module_name counts |> Option.unwrap_or ~default:0 in
                (module_name, count + 1) :: List.remove_assoc module_name counts
            | _ -> counts)
          []
      in
      let module_pairing_counts =
        !events
        |> List.fold_left
          (fun counts (event: Event.t) ->
            match Event.(event.kind) with
            | Event.ModulePairingStarted { module_name; _ } ->
                let count = List.assoc_opt module_name counts |> Option.unwrap_or ~default:0 in
                (module_name, count + 1) :: List.remove_assoc module_name counts
            | _ -> counts)
          []
      in
      let count_for counts module_name = List.assoc_opt module_name counts
      |> Option.unwrap_or ~default:0 in
      let repeated_modules = [ "App"; "Left"; "Right"; "Base" ]
      |> List.filter
        (fun module_name ->
          count_for snapshot_analysis_counts module_name > 1
          || count_for module_pairing_counts module_name > 1) in
      if not (repeated_modules = []) then
        Error (format
          Format.[
            str "expected shared local modules to be analyzed and paired once, got repeated work for ";
            str (String.concat ", " repeated_modules);
            str ": ";
            str (typ_events_json !events);
          ])
      else (
        Test.assert_equal ~expected:1 ~actual:(count_for snapshot_analysis_counts "App");
        Test.assert_equal ~expected:1 ~actual:(count_for module_pairing_counts "App");
        Ok ()
      )

let test_prepare_snapshot_reuses_paired_local_modules_across_rooted_snapshots = fun _ctx ->
  let events = ref [] in
  let config = Config.default
  |> Config.with_on_event ~on_event:(fun event -> events := !events @ [ event ]) in
  let session = Session.empty ~config in
  let (session, file_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "file.mli")
    ~text:{ocaml|
      val read : unit -> int
    |ocaml}
  in
  let (session, file_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "file.ml")
    ~text:{ocaml|
      let read () = 1
    |ocaml}
  in
  let (session, fs_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "fs.mli")
    ~text:{ocaml|
      val read : unit -> int
    |ocaml}
  in
  let (session, fs_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "fs.ml")
    ~text:{ocaml|
      let read = File.read
    |ocaml}
  in
  let (session, left_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "left.ml")
    ~text:{ocaml|
      let left = Fs.read ()
    |ocaml}
  in
  let (session, right_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "right.ml")
    ~text:{ocaml|
      let right = Fs.read ()
    |ocaml}
  in
  let run_root source_id =
    match Session.prepare_snapshot session ~roots:[ source_id ] with
    | Error missing -> Error ("expected rooted snapshot, got "
    ^ (Session.MissingRequirements.to_json missing |> Data.Json.to_string))
    | Ok snapshot ->
        let _ = Query.analysis_of_source snapshot source_id |> Option.expect ~msg:"expected analysis" in
        let diagnostics = diagnostic_strings snapshot source_id in
        if List.is_empty diagnostics then
          Ok ()
        else
          Error (String.concat "\n" diagnostics)
  in
  match run_root left_source_id with
  | Error _ as err -> err
  | Ok () -> (
      match run_root right_source_id with
      | Error _ as err -> err
      | Ok () ->
          let snapshot_analysis_counts =
            !events
            |> List.fold_left
              (fun counts (event: Event.t) ->
                match Event.(event.kind) with
                | Event.SourceAnalysisStarted { source_id; mode=Event.SnapshotAnalysis; _ } ->
                    let key = SourceId.to_int source_id in
                    let count = List.assoc_opt key counts |> Option.unwrap_or ~default:0 in
                    (key, count + 1) :: List.remove_assoc key counts
                | _ -> counts)
              []
          in
          let module_pairing_counts =
            !events
            |> List.fold_left
              (fun counts (event: Event.t) ->
                match Event.(event.kind) with
                | Event.ModulePairingStarted { module_name; _ } ->
                    let count = List.assoc_opt module_name counts |> Option.unwrap_or ~default:0 in
                    (module_name, count + 1) :: List.remove_assoc module_name counts
                | _ -> counts)
              []
          in
          let count_for_source counts source_id = List.assoc_opt (SourceId.to_int source_id) counts
          |> Option.unwrap_or ~default:0 in
          let count_for_module counts module_name = List.assoc_opt module_name counts
          |> Option.unwrap_or ~default:0 in
          let repeated_sources =
            [
              ("File.mli", file_intf_source_id);
              ("File.ml", file_impl_source_id);
              ("Fs.mli", fs_intf_source_id);
              ("Fs.ml", fs_impl_source_id);
            ]
            |> List.filter_map
              (fun (label, source_id) ->
                let count = count_for_source snapshot_analysis_counts source_id in
                if count > 1 then
                  Some (format Format.[ str label; str "="; int count ])
                else
                  None)
          in
          let repeated_modules =
            [ "File"; "Fs" ]
            |> List.filter_map
              (fun module_name ->
                let count = count_for_module module_pairing_counts module_name in
                if count > 1 then
                  Some (format Format.[ str module_name; str "="; int count ])
                else
                  None)
          in
          if not (List.is_empty repeated_sources) || not (List.is_empty repeated_modules) then
            Error (format
              Format.[
                str "expected paired local dependencies to be reused across rooted snapshots, got repeated source analyses [";
                str (String.concat ", " repeated_sources);
                str "] and pairings [";
                str (String.concat ", " repeated_modules);
                str "]: ";
                str (typ_events_json !events);
              ])
          else (
            Test.assert_equal
              ~expected:1
              ~actual:(count_for_source snapshot_analysis_counts file_intf_source_id);
            Test.assert_equal
              ~expected:1
              ~actual:(count_for_source snapshot_analysis_counts file_impl_source_id);
            Test.assert_equal
              ~expected:1
              ~actual:(count_for_source snapshot_analysis_counts fs_intf_source_id);
            Test.assert_equal
              ~expected:1
              ~actual:(count_for_source snapshot_analysis_counts fs_impl_source_id);
            Test.assert_equal ~expected:1 ~actual:(count_for_module module_pairing_counts "File");
            Test.assert_equal ~expected:1 ~actual:(count_for_module module_pairing_counts "Fs");
            Ok ()
          )
    )

let test_prepare_snapshot_reuses_shared_implicit_open_alias_modules = fun _ctx ->
  let events = ref [] in
  let config = Config.default
  |> Config.with_on_event ~on_event:(fun event -> events := !events @ [ event ]) in
  let session = Session.empty ~config in
  let aliases_text = {ocaml|
    module Base = Base
    module Left = Left
    module Right = Right
  |ocaml}
  in
  let aliases_parse_result = Syn.parse ~filename:(Path.v "Aliases.ml-gen") aliases_text in
  let aliases_cst = expect_cst ~filename:"Aliases.ml-gen" aliases_parse_result in
  let (session, _aliases_source_id) = Session.create_source
    session
    ~kind:Source.Generated
    ~module_name:"Aliases"
    ~implicit_opens:[]
    ~origin:(Source.Label "Aliases.ml-gen")
    ~source_hash:(Source.hash ~implicit_opens:[] ~cst:aliases_cst)
    ~parse_result:aliases_parse_result
    ~cst:aliases_cst in
  let (session, _base_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "base.ml")
    ~text:{ocaml|
      let value = 1
    |ocaml}
  in
  let implicit_opens = [ SurfacePath.of_string "Aliases" ] in
  let left_text = {ocaml|
    let left = Base.value
  |ocaml}
  in
  let left_parse_result = Syn.parse ~filename:(Path.v "left.ml") left_text in
  let left_cst = expect_cst ~filename:"left.ml" left_parse_result in
  let (session, _left_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~module_name:"Left"
    ~implicit_opens
    ~origin:(Source.Label "left.ml")
    ~source_hash:(Source.hash ~implicit_opens ~cst:left_cst)
    ~parse_result:left_parse_result
    ~cst:left_cst in
  let right_text = {ocaml|
    let right = Base.value
  |ocaml}
  in
  let right_parse_result = Syn.parse ~filename:(Path.v "right.ml") right_text in
  let right_cst = expect_cst ~filename:"right.ml" right_parse_result in
  let (session, _right_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~module_name:"Right"
    ~implicit_opens
    ~origin:(Source.Label "right.ml")
    ~source_hash:(Source.hash ~implicit_opens ~cst:right_cst)
    ~parse_result:right_parse_result
    ~cst:right_cst in
  let app_text = {ocaml|
    let total = Left.left + Right.right
  |ocaml}
  in
  let app_parse_result = Syn.parse ~filename:(Path.v "app.ml") app_text in
  let app_cst = expect_cst ~filename:"app.ml" app_parse_result in
  let (session, app_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~module_name:"App"
    ~implicit_opens
    ~origin:(Source.Label "app.ml")
    ~source_hash:(Source.hash ~implicit_opens ~cst:app_cst)
    ~parse_result:app_parse_result
    ~cst:app_cst in
  match Session.prepare_snapshot session ~roots:[ app_source_id ] with
  | Error missing -> Error (format
    Format.[
      str "expected rooted snapshot, got ";
      str (Data.Json.to_string (Session.MissingRequirements.to_json missing));
    ])
  | Ok snapshot ->
      let _ = Query.analysis_of_source snapshot app_source_id |> Option.expect ~msg:"expected analysis" in
      let snapshot_analysis_counts =
        !events
        |> List.fold_left
          (fun counts (event: Event.t) ->
            match Event.(event.kind) with
            | Event.SourceAnalysisStarted { module_name; mode=Event.SnapshotAnalysis; _ } ->
                let count = List.assoc_opt module_name counts |> Option.unwrap_or ~default:0 in
                (module_name, count + 1) :: List.remove_assoc module_name counts
            | _ -> counts)
          []
      in
      let module_pairing_counts =
        !events
        |> List.fold_left
          (fun counts (event: Event.t) ->
            match Event.(event.kind) with
            | Event.ModulePairingStarted { module_name; _ } ->
                let count = List.assoc_opt module_name counts |> Option.unwrap_or ~default:0 in
                (module_name, count + 1) :: List.remove_assoc module_name counts
            | _ -> counts)
          []
      in
      let count_for counts module_name = List.assoc_opt module_name counts
      |> Option.unwrap_or ~default:0 in
      let alias_analysis_count = count_for snapshot_analysis_counts "Aliases" in
      let alias_pairing_count = count_for module_pairing_counts "Aliases" in
      if alias_analysis_count > 1 || alias_pairing_count > 1 then
        Error (format
          Format.[
            str "expected implicit-open alias module to be analyzed and paired once, got analysis=";
            int alias_analysis_count;
            str " pairing=";
            int alias_pairing_count;
            str ": ";
            str (typ_events_json !events);
          ])
      else (
        Test.assert_equal ~expected:1 ~actual:alias_analysis_count;
        Test.assert_equal ~expected:1 ~actual:alias_pairing_count;
        Ok ()
      )

let test_prepare_snapshot_imports_bare_local_module_exports_into_rooted_analysis = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _dep_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "dep.ml")
    ~text:{ocaml|
      let value = 1
    |ocaml}
  in
  let (session, app_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "app.ml")
    ~text:{ocaml|
      open Dep

      let answer = value
      let also_answer = Dep.value
    |ocaml}
  in
  match Session.prepare_snapshot session ~roots:[ app_source_id ] with
  | Error missing -> Error (format
    Format.[
      str "expected rooted snapshot, got ";
      str (Data.Json.to_string (Session.MissingRequirements.to_json missing));
    ])
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot app_source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        let answer_type = export_scheme snapshot app_source_id "answer" in
        let also_answer_type = export_scheme snapshot app_source_id "also_answer" in
        if answer_type = Some "int" && also_answer_type = Some "int" then
          Ok ()
        else
          Error (format
            Format.[
              str "unexpected exported types: answer=";
              str (show_option answer_type);
              str ", also_answer=";
              str (show_option also_answer_type);
            ])

let test_prepare_snapshot_store_hydration_emits_structured_events = fun _ctx ->
  with_typ_store
    (fun store ->
      let baseline_loaded_module_count = Model.LoadedModules.len Config.default.loaded_modules in
      let seed_session = Session.empty ~config:Config.default in
      let (seed_session, colors_source_id) = create_source seed_session ~kind:Source.File ~origin:(Source.Label "colors.ml")
        ~text:{ocaml|
          module RGB = struct
            let blend x y = x
          end

          let to_string value = value
        |ocaml}
      in
      let seed_snapshot = Session.snapshot seed_session in
      let loaded_colors =
        match Query.module_typings_of seed_snapshot colors_source_id with
        | Some typings -> typings
        | None -> panic "expected seed module typings"
      in
      let _ = Store.save_module_typings store loaded_colors |> Result.expect ~msg:"save_module_typings should succeed" in
      let events = ref [] in
      let config = Config.default
      |> Config.with_store ~store:(Some store)
      |> Config.with_on_event ~on_event:(fun event -> events := !events @ [ event ]) in
      let session = Session.empty ~config in
      let (session, demo_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "blend_demo.ml")
        ~text:{ocaml|
          open Colors

          let midpoint = RGB.blend 1 2
          let label = to_string "ok"
        |ocaml}
      in
      match Session.prepare_snapshot session ~roots:[ demo_source_id ] with
      | Error missing -> Error (format
        Format.[
          str "expected store-backed snapshot, got ";
          str (Data.Json.to_string (Session.MissingRequirements.to_json missing));
        ])
      | Ok _snapshot ->
          let actual = !events |> List.map typ_event_name in
          let expected = [
            "typ_prepare_snapshot_start";
            "typ_hydrate_module_typings_start";
            "typ_hydrate_module_typings_finish";
            "typ_prepare_snapshot_finish";
          ] in
          if not (actual = expected) then
            Error ("unexpected hydration event order: "
            ^ (Data.Json.Array (!events |> List.map Event.to_json) |> Data.Json.to_string))
          else
            match !events with
            | [
              { Event.kind=Event.PrepareSnapshotStarted _; _ };
              { Event.kind=Event.HydrateModuleTypingsStarted { missing_modules; _ }; _ };
              {
                Event.kind=Event.HydrateModuleTypingsFinished {
                  hydrated_modules;
                  loaded_module_count;
                  _
                };
                _
              };
              {
                Event.kind=Event.PrepareSnapshotFinished {
                  loaded_module_count=final_loaded_module_count;
                  _
                };
                _
              };

            ] ->
                if
                  missing_modules = [ "Colors"; "RGB" ]
                  && hydrated_modules = [ "Colors" ]
                  && loaded_module_count = baseline_loaded_module_count + 1
                  && final_loaded_module_count = baseline_loaded_module_count + 1
                then
                  Ok ()
                else
                  Error ("unexpected hydration payloads: "
                  ^ (Data.Json.Array (!events |> List.map Event.to_json) |> Data.Json.to_string))
            | _ -> Error ("unexpected hydration events: "
            ^ (Data.Json.Array (!events |> List.map Event.to_json) |> Data.Json.to_string)))

let test_prepare_snapshot_missing_requirements_emit_structured_events = fun _ctx ->
  let events = ref [] in
  let config = Config.default
  |> Config.with_on_event ~on_event:(fun event -> events := !events @ [ event ]) in
  let session = Session.empty ~config in
  let (session, source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "demo.ml")
    ~text:{ocaml|
      open Missing

      let answer = value
    |ocaml}
  in
  match Session.prepare_snapshot session ~roots:[ source_id ] with
  | Ok _ -> Error "expected missing module requirements"
  | Error _missing ->
      let actual = !events |> List.map typ_event_name in
      let expected = [ "typ_prepare_snapshot_start"; "typ_prepare_snapshot_failed"; ] in
      let () = Test.assert_equal ~expected ~actual in
      match !events with
      | [
        { Event.kind=Event.PrepareSnapshotStarted _; _ };
        { Event.kind=Event.PrepareSnapshotFailed { missing_root_source_ids; missing_modules; _ }; _ };

      ] ->
          let () = Test.assert_equal
            ~expected:[]
            ~actual:(missing_root_source_ids |> List.map SourceId.to_int) in
          let () = Test.assert_equal ~expected:[ "Missing" ] ~actual:missing_modules in
          Ok ()
      | _ -> Error ("unexpected missing-requirements events: "
      ^ (Data.Json.Array (!events |> List.map Event.to_json) |> Data.Json.to_string))

let test_prepare_snapshot_reports_match_coverage_diagnostics = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
    type 'a option =
      | None
      | Some of 'a

    let nonexhaustive x =
      match x with
      | Some value -> value

    let redundant x =
      match x with
      | _ -> 0
      | Some value -> value
  |ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "match_coverage.ml")
    ~text:source in
  match prepare_snapshot_or_error session ~roots:[ source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = typing_diagnostic_summaries snapshot source_id in
      let () = Test.assert_equal
        ~expected:[
          ("TYP1012", "warning", "non-exhaustive match: missing case None");
          ("TYP1013", "warning", "match case is redundant");
        ]
        ~actual:diagnostics in
      Ok ()

let test_prepare_snapshot_includes_interface_sibling_dependencies = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, impl_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"let answer value = value\n" in
  let (session, intf_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.mli")
    ~text:"open Missing_module\nval answer : int -> int\n" in
  match Session.prepare_snapshot session ~roots:[ impl_source_id ] with
  | Ok _ -> Error "expected rooted snapshot preparation to include sibling interface dependencies"
  | Error missing ->
      let actual = Session.MissingRequirements.to_json missing |> Data.Json.to_string in
      let expected = Data.Json.Array [
        Data.Json.Object [
          ("tag", Data.Json.String "missing_module_summary");
          ("module_name", Data.Json.String "Missing_module");
          ("requested_by", Data.Json.Array [ Data.Json.Int (SourceId.to_int intf_source_id) ]);
        ];
      ]
      |> Data.Json.to_string in
      if String.equal expected actual then
        Ok ()
      else
        Error (String.concat "\n" [ "expected"; expected; "actual"; actual ])

let test_loaded_module_typings_override_store = fun _ctx ->
  with_typ_store
    (fun store ->
      let good_seed = Session.empty ~config:Config.default in
      let (good_seed, good_source_id) = create_source
        good_seed
        ~kind:Source.File
        ~origin:(Source.Label "colors.ml")
        ~text:"module RGB = struct let blend x y = x end\nlet to_string value = value\n" in
      let good_snapshot = Session.snapshot good_seed in
      let good_colors =
        match Query.module_typings_of good_snapshot good_source_id with
        | Some typings -> typings
        | None -> panic "expected good colors module typings"
      in
      let bad_seed = Session.empty ~config:Config.default in
      let (bad_seed, bad_source_id) = create_source
        bad_seed
        ~kind:Source.File
        ~origin:(Source.Label "colors.ml")
        ~text:"let to_string value = value\n" in
      let bad_snapshot = Session.snapshot bad_seed in
      let bad_colors =
        match Query.module_typings_of bad_snapshot bad_source_id with
        | Some typings -> typings
        | None -> panic "expected bad colors module typings"
      in
      let _ = Store.save_module_typings store bad_colors |> Result.expect ~msg:"save_module_typings should succeed" in
      let config = Config.default
      |> Config.with_store ~store:(Some store)
      |> Config.with_loaded_modules ~loaded_modules:[ good_colors ] in
      let session = Session.empty ~config in
      let (session, demo_source_id) = create_source
        session
        ~kind:Source.File
        ~origin:(Source.Label "blend_demo.ml")
        ~text:"open Colors\nlet midpoint = RGB.blend 1 2\nlet label = to_string \"ok\"\n" in
      let snapshot = Session.snapshot session in
      let diagnostics = diagnostic_strings snapshot demo_source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        let midpoint_type = inferred_type_at snapshot demo_source_id 34 in
        let label_type = inferred_type_at snapshot demo_source_id 58 in
        let () = Test.assert_equal ~expected:(Some "int -> int -> int") ~actual:midpoint_type in
        let () = Test.assert_equal ~expected:(Some "string -> string") ~actual:label_type in
        Ok ())

let test_snapshot_uses_sibling_source_record_types = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, colors_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"type point = { x: int; y: int }\n" in
  let source = {ocaml|
    open Colors

    let origin = { x = 0; y = 0 }
    let total point = point.x + point.y
  |ocaml}
  in
  let (session, demo_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "blend_demo.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot demo_source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let seed_summary =
      match Query.module_typings_of snapshot colors_source_id with
      | Some typings -> typings
      | None -> panic "expected colors module typings"
    in
    let type_decl_names = ModuleTypings.type_decls seed_summary
    |> List.map (fun (type_decl: FileSummary.type_decl) -> type_decl.declaration.type_name) in
    let field_access_offset =
      let access_start = offset_of_substring source "point.x +" |> Option.expect ~msg:"expected record field access in test source" in
      access_start + String.length "point."
    in
    let origin_type = export_scheme snapshot demo_source_id "origin" in
    let total_type = export_scheme snapshot demo_source_id "total" in
    let field_access_type = inferred_type_at snapshot demo_source_id field_access_offset in
    if not (type_decl_names = [ "point" ]) then
      Error ("unexpected type decl names: " ^ String.concat ", " type_decl_names)
    else if not (origin_type = Some "Colors.point") then
      Error ("unexpected origin type: " ^ show_option origin_type)
    else if not (total_type = Some "Colors.point -> int") then
      Error ("unexpected total type: " ^ show_option total_type)
    else if not (field_access_type = Some "int") then
      Error ("unexpected field access type: " ^ show_option field_access_type)
    else
      Ok ()

let test_snapshot_uses_loaded_module_record_types = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, colors_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"type point = { x: int; y: int }\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_colors =
    match Query.module_typings_of seed_snapshot colors_source_id with
    | Some typings -> typings
    | None -> panic "expected colors module typings"
  in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ loaded_colors ] in
  let session = Session.empty ~config in
  let source = {ocaml|
    open Colors

    let origin = { x = 0; y = 0 }
    let total point = point.x + point.y
  |ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "consumer.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let origin_type = export_scheme snapshot source_id "origin" in
    let total_type = export_scheme snapshot source_id "total" in
    if not (origin_type = Some "Colors.point") then
      Error ("unexpected origin type: " ^ show_option origin_type)
    else if not (total_type = Some "Colors.point -> int") then
      Error ("unexpected total type: " ^ show_option total_type)
    else
      Ok ()

let test_include_reexports_loaded_module_record_types = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, helpers_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "helpers.ml")
    ~text:"type point = { x: int; y: int }\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_helpers =
    match Query.module_typings_of seed_snapshot helpers_source_id with
    | Some typings -> typings
    | None -> panic "expected helper module typings"
  in
  let consumer_config = Config.default
  |> Config.with_loaded_modules ~loaded_modules:[ loaded_helpers ] in
  let consumer_session = Session.empty ~config:consumer_config in
  let (consumer_session, consumer_source_id) = create_source
    consumer_session
    ~kind:Source.File
    ~origin:(Source.Label "consumer.ml")
    ~text:"include Helpers\n" in
  let consumer_snapshot = Session.snapshot consumer_session in
  let consumer_diagnostics = diagnostic_strings consumer_snapshot consumer_source_id in
  if not (List.is_empty consumer_diagnostics) then
    Error (String.concat "\n" consumer_diagnostics)
  else
    let consumer_summary =
      match Query.module_typings_of consumer_snapshot consumer_source_id with
      | Some typings -> typings
      | None -> panic "expected consumer module typings"
    in
    let exported_type_decls = ModuleTypings.type_decls consumer_summary
    |> List.map
      (fun (type_decl: FileSummary.type_decl) ->
        (SurfacePath.to_segments type_decl.scope_path, type_decl.declaration.type_name)) in
    let client_config = Config.default
    |> Config.with_loaded_modules ~loaded_modules:[ consumer_summary ] in
    let client_session = Session.empty ~config:client_config in
    let client_source = "let origin = { x = 0; y = 0 }\nlet total point = point.x + point.y\n" in
    let (client_session, client_source_id) = create_source
      client_session
      ~kind:Source.File
      ~origin:(Source.Label "client.ml")
      ~text:client_source in
    let client_snapshot = Session.snapshot client_session in
    let client_diagnostics = diagnostic_strings client_snapshot client_source_id in
    if not (List.is_empty client_diagnostics) then
      Error (String.concat "\n" client_diagnostics)
    else
      let origin_type = export_scheme client_snapshot client_source_id "origin" in
      let total_type = export_scheme client_snapshot client_source_id "total" in
      if not (exported_type_decls = [ ([], "point") ]) then
        Error ("unexpected exported type decls: "
        ^ String.concat
          ", "
          (List.map (fun (scope, name) -> "[" ^ String.concat "." scope ^ "]." ^ name) exported_type_decls))
      else if not (origin_type = Some "Consumer.point") then
        Error ("unexpected origin type: " ^ show_option origin_type)
      else if not (total_type = Some "Consumer.point -> int") then
        Error ("unexpected total type: " ^ show_option total_type)
      else
        Ok ()

let test_module_alias_reexports_loaded_module_record_types = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, helpers_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "helpers.ml")
    ~text:"type point = { x: int; y: int }\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_helpers =
    match Query.module_typings_of seed_snapshot helpers_source_id with
    | Some typings -> typings
    | None -> panic "expected helper module typings"
  in
  let consumer_config = Config.default
  |> Config.with_loaded_modules ~loaded_modules:[ loaded_helpers ] in
  let consumer_session = Session.empty ~config:consumer_config in
  let (consumer_session, consumer_source_id) = create_source
    consumer_session
    ~kind:Source.File
    ~origin:(Source.Label "consumer.ml")
    ~text:"module Util = Helpers\n" in
  let consumer_snapshot = Session.snapshot consumer_session in
  let consumer_diagnostics = diagnostic_strings consumer_snapshot consumer_source_id in
  if not (List.is_empty consumer_diagnostics) then
    Error (String.concat "\n" consumer_diagnostics)
  else
    let consumer_summary =
      match Query.module_typings_of consumer_snapshot consumer_source_id with
      | Some typings -> typings
      | None -> panic "expected consumer module typings"
    in
    let exported_type_decls = ModuleTypings.type_decls consumer_summary
    |> List.map
      (fun (type_decl: FileSummary.type_decl) ->
        (SurfacePath.to_segments type_decl.scope_path, type_decl.declaration.type_name)) in
    let client_config = Config.default
    |> Config.with_loaded_modules ~loaded_modules:[ consumer_summary ] in
    let client_session = Session.empty ~config:client_config in
    let client_source = "let origin = { x = 0; y = 0 }\nlet total point = point.x + point.y\n" in
    let (client_session, client_source_id) = create_source
      client_session
      ~kind:Source.File
      ~origin:(Source.Label "client.ml")
      ~text:client_source in
    let client_snapshot = Session.snapshot client_session in
    let client_diagnostics = diagnostic_strings client_snapshot client_source_id in
    if not (List.is_empty client_diagnostics) then
      Error (String.concat "\n" client_diagnostics)
    else
      let origin_type = export_scheme client_snapshot client_source_id "origin" in
      let total_type = export_scheme client_snapshot client_source_id "total" in
      if not (exported_type_decls = [ ([ "Util" ], "point") ]) then
        Error ("unexpected exported type decls: "
        ^ String.concat
          ", "
          (List.map (fun (scope, name) -> "[" ^ String.concat "." scope ^ "]." ^ name) exported_type_decls))
      else if not (origin_type = Some "Consumer.Util.point") then
        Error ("unexpected origin type: " ^ show_option origin_type)
      else if not (total_type = Some "Consumer.Util.point -> int") then
        Error ("unexpected total type: " ^ show_option total_type)
      else
        Ok ()

let test_include_reexports_loaded_module_typings = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, helpers_source_id) = create_source seed_session ~kind:Source.File ~origin:(Source.Label "helpers.ml")
    ~text:{ocaml|
type 'a option =
  | None
  | Some of 'a

let id x = x

let wrap value = Some value
|ocaml}
  in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_helpers =
    match Query.module_typings_of seed_snapshot helpers_source_id with
    | Some typings -> typings
    | None -> panic "expected helper module typings"
  in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ loaded_helpers ] in
  let session = Session.empty ~config in
  let source = "include Helpers\nlet answer = wrap (id 1)\n" in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "consumer.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let id_type = export_scheme snapshot source_id "id" in
    let wrap_type = export_scheme snapshot source_id "wrap" in
    let answer_type = export_scheme snapshot source_id "answer" in
    let exported_names = export_names (Query.export_of snapshot source_id) in
    if not (id_type = Some "'a. 'a -> 'a") then
      Error ("unexpected id type: " ^ show_option id_type)
    else if not (exported_names = [ "answer"; "id"; "wrap" ]) then
      Error ("unexpected exported names: " ^ String.concat ", " exported_names)
    else if not
        (wrap_type
        |> Option.map (fun text -> Option.is_some (offset_of_substring text "option"))
        |> Option.unwrap_or ~default:false) then
      Error ("unexpected wrap type: " ^ show_option wrap_type)
    else if not
        (answer_type
        |> Option.map (fun text -> Option.is_some (offset_of_substring text "option"))
        |> Option.unwrap_or ~default:false) then
      Error ("unexpected answer type: " ^ show_option answer_type)
    else
      Ok ()

let test_include_reexports_package_shaped_loaded_module_typings = fun _ctx ->
  let kernel_seed_session = Session.empty ~config:Config.default in
  let (kernel_seed_session, _result_impl_source_id) = create_source kernel_seed_session ~kind:Source.File ~origin:(Source.Label "result.ml")
    ~text:{ocaml|
let map f x = f x
|ocaml}
  in
  let (kernel_seed_session, _result_intf_source_id) = create_source kernel_seed_session ~kind:Source.File ~origin:(Source.Label "result.mli")
    ~text:{ocaml|
val map : ('a -> 'b) -> 'a -> 'b
|ocaml}
  in
  let (kernel_seed_session, _kernel_impl_source_id) = create_source kernel_seed_session ~kind:Source.File ~origin:(Source.Label "kernel.ml")
    ~text:{ocaml|
module Result = Result
|ocaml}
  in
  let (kernel_seed_session, _kernel_intf_source_id) = create_source kernel_seed_session ~kind:Source.File ~origin:(Source.Label "kernel.mli")
    ~text:{ocaml|
module Result = Result
|ocaml}
  in
  let kernel_seed_snapshot = Session.snapshot kernel_seed_session in
  let loaded_modules = Snapshot.module_typings kernel_seed_snapshot in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules in
  let session = Session.empty ~config in
  let (session, source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "consumer.ml")
    ~text:{ocaml|
include Kernel.Result

let answer = map (fun x -> x + 1) 1
|ocaml}
  in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let map_type = export_scheme snapshot source_id "map" in
    let answer_type = export_scheme snapshot source_id "answer" in
    let exported_names = export_names (Query.export_of snapshot source_id) in
    let () = Test.assert_equal ~expected:(Some "'a 'b. ('a -> 'b) -> 'a -> 'b") ~actual:map_type in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:answer_type in
    let () = Test.assert_equal ~expected:[ "answer"; "map" ] ~actual:exported_names in
    Ok ()

let test_include_module_type_of_canonicalizes_loaded_nominal_types = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, actors_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "actors.mli")
    ~text:"module Process: sig\n  type exit_reason = exn\nend\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_actors =
    match Query.module_typings_of seed_snapshot actors_source_id with
    | Some typings -> typings
    | None -> panic "expected actors module typings"
  in
  let source = prepared_source ~filename:"process.mli"
    ~text:{ocaml|
      include module type of Actors.Process
      val spawn: (unit -> (unit, exit_reason) result) -> int
    |ocaml}
  in
  let imported_world = imported_world_of_loaded_modules [ loaded_actors ] source in
  let analysis = SourceAnalysis.analyze ~imported_world ~config:Config.default source in
  let diagnostics = analysis.lowering_diagnostics @ analysis.typing_diagnostics
  |> List.map Diagnostic.to_string in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    match lookup_export "spawn" (FileSummary.exports analysis.file_summary) with
    | None -> Error "expected spawn export"
    | Some spawn_scheme ->
        if scheme_has_named_path spawn_scheme then
          Error ("spawn scheme still contains symbolic named paths: "
          ^ TypePrinter.scheme_to_string spawn_scheme)
        else
          (
            try
              let _ = ModuleTypings.of_file_summary
                ~module_name:"Process"
                ~source_hash:(Source.input_hash source)
                analysis.file_summary
              |> ModuleTypings.Json.to_json in
              Ok ()
            with
            | Failure message -> Error message
          )

let test_include_module_type_of_loaded_modules_canonicalizes_nominal_types = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, actors_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "actors.mli")
    ~text:"module Process: sig\n  type exit_reason = exn\nend\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_actors =
    match Query.module_typings_of seed_snapshot actors_source_id with
    | Some typings -> typings
    | None -> panic "expected actors module typings"
  in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ loaded_actors ] in
  let session = Session.empty ~config in
  let source = {ocaml|
    include module type of Actors.Process
    val spawn: (unit -> (unit, exit_reason) result) -> int
  |ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "process.mli")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    match Query.module_typings_of snapshot source_id with
    | None -> Error "expected process module typings"
    | Some typings ->
        let exports = ModuleTypings.exports typings in
        (
          match lookup_export "spawn" exports with
          | None ->
              Error "expected spawn export"
          | Some spawn_scheme when scheme_has_named_path spawn_scheme ->
              Error ("spawn scheme still contains symbolic named paths: "
              ^ TypePrinter.scheme_to_string spawn_scheme)
          | Some _ -> (
              try
                let _ = ModuleTypings.Json.to_json typings in
                Ok ()
              with
              | Failure message -> Error message
            )
        )

let test_loaded_module_reexports_canonicalize_dependency_result_aliases = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, actors_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "actors.mli")
    ~text:"module Process: sig\n  type exit_reason = exn\nend\n" in
  let (seed_session, kernel_source_id) = create_source seed_session ~kind:Source.File ~origin:(Source.Label "kernel.mli")
    ~text:{ocaml|
type ('ok, 'error) result =
  | Ok of 'ok
  | Error of 'error
|ocaml}
  in
  let (seed_session, _global_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "global.mli")
    ~text:"val spawn : (unit -> (unit, Actors.Process.exit_reason) Kernel.result) -> int\n" in
  let (seed_session, std_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "std.mli")
    ~text:"include module type of Global\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_actors =
    match Query.module_typings_of seed_snapshot actors_source_id with
    | Some typings -> typings
    | None -> panic "expected actors module typings"
  in
  let loaded_kernel =
    match Query.module_typings_of seed_snapshot kernel_source_id with
    | Some typings -> typings
    | None -> panic "expected kernel module typings"
  in
  let loaded_std =
    match Query.module_typings_of seed_snapshot std_source_id with
    | Some typings -> typings
    | None -> panic "expected std module typings"
  in
  let config = Config.default
  |> Config.with_loaded_modules ~loaded_modules:[ loaded_std; loaded_kernel; loaded_actors ] in
  let session = Session.empty ~config in
  let source = {ocaml|
type ('a, 'e) result = ('a, 'e) Kernel.result =
  | Ok of 'a
  | Error of 'e

exception Abort

let task (): (unit, Actors.Process.exit_reason) result = Error Abort

let pid = Std.spawn task
|ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let pid_type = export_scheme snapshot source_id "pid" in
    Test.assert_equal ~expected:(Some "int") ~actual:pid_type;
    Ok ()

let test_operator_pattern_bindings_lower_and_typecheck = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "ops_aliases.ml")
    ~text:{ocaml|
      external equal : 'a -> 'a -> bool = "%equal"
      external add_int : int -> int -> int = "%addint"
      external revapply : 'a -> ('a -> 'b) -> 'b = "%revapply"

      let ( = ) = equal
      let ( + ) = add_int
      let ( |> ) = revapply

      let same = 1 = 1
      let sum = 1 + 2
      let piped = 1 |> fun x -> x + 1
    |ocaml}
  in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let exported_names = export_names (Query.export_of snapshot source_id) |> List.sort String.compare in
    let required_exports = [ "="; "+"; "|>"; "same"; "sum"; "piped" ] in
    match required_exports |> List.find_opt (fun name -> not (List.mem name exported_names)) with
    | Some missing_export -> Error ("missing expected export "
    ^ missing_export
    ^ " from "
    ^ String.concat ", " exported_names)
    | None ->
        let () = Test.assert_equal
          ~expected:(Some "bool")
          ~actual:(export_scheme snapshot source_id "same") in
        let () = Test.assert_equal
          ~expected:(Some "int")
          ~actual:(export_scheme snapshot source_id "sum") in
        let () = Test.assert_equal
          ~expected:(Some "int")
          ~actual:(export_scheme snapshot source_id "piped") in
        Ok ()

let test_cons_patterns_lower_and_typecheck = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "cons_patterns.ml")
    ~text:{ocaml|
      let rec append left right =
        match left with
        | [] -> right
        | head :: tail -> head :: append tail right
    |ocaml}
  in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    match export_scheme snapshot source_id "append" with
    | Some _ -> Ok ()
    | None -> Error "missing append export"

let test_recursive_operator_bindings_typecheck = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "recursive_operator_bindings.ml")
    ~text:{ocaml|
      let rec ( @ ) left right =
        match left with
        | [] -> right
        | head :: tail -> head :: (tail @ right)
    |ocaml}
  in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    match export_scheme snapshot source_id "@" with
    | Some _ -> Ok ()
    | None -> Error "missing @ export"

let test_language_prelude_supports_angle_not_equal = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "prelude_ops.ml")
    ~text:{ocaml|
      let different = 1 <> 2
    |ocaml}
  in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let exported_names = export_names (Query.export_of snapshot source_id) |> List.sort String.compare in
    let required_exports = [ "<>"; "different" ] in
    match required_exports |> List.find_opt (fun name -> not (List.mem name exported_names)) with
    | Some missing_export -> Error ("missing expected export "
    ^ missing_export
    ^ " from "
    ^ String.concat ", " exported_names)
    | None ->
        let () = Test.assert_equal
          ~expected:(Some "bool")
          ~actual:(export_scheme snapshot source_id "different") in
        Ok ()

let test_kernel_ops_surface_typechecks = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "kernel_ops.ml")
    ~text:{ocaml|
      external equal : 'a -> 'a -> bool = "%equal"
      let ( = ) = equal

      external not_equal : 'a -> 'a -> bool = "%notequal"
      let ( != ) = not_equal
      let ( <> ) = not_equal

      external ptr_eq : 'a -> 'a -> bool = "%eq"
      external ptr_not_eq : 'a -> 'a -> bool = "%noteq"

      external less_than : 'a -> 'a -> bool = "%lessthan"
      let ( < ) = less_than

      external greater_than : 'a -> 'a -> bool = "%greaterthan"
      let ( > ) = greater_than

      external less_or_equal : 'a -> 'a -> bool = "%lessequal"
      let ( <= ) = less_or_equal

      external greater_or_equal : 'a -> 'a -> bool = "%greaterequal"
      let ( >= ) = greater_or_equal

      external neg_int : int -> int = "%negint"
      let ( ~- ) = neg_int

      external id_int : int -> int = "%identity"
      let ( ~+ ) = id_int

      external add_int : int -> int -> int = "%addint"
      let ( + ) = add_int

      external sub_int : int -> int -> int = "%subint"
      let ( - ) = sub_int

      external mul_int : int -> int -> int = "%mulint"
      let ( * ) = mul_int

      external div_int : int -> int -> int = "%divint"
      let ( / ) = div_int

      external rem_int : int -> int -> int = "%modint"
      let ( mod ) = rem_int

      let abs value =
        if value >= 0 then
          value
        else
          -value

      external int_logand : int -> int -> int = "%andint"
      let ( land ) = int_logand

      external int_logor : int -> int -> int = "%orint"
      let ( lor ) = int_logor

      external int_logxor : int -> int -> int = "%xorint"
      let ( lxor ) = int_logxor

      let lnot value = value lxor (-1)

      external shift_left_int : int -> int -> int = "%lslint"
      let ( lsl ) = shift_left_int

      external shift_right_logical_int : int -> int -> int = "%lsrint"
      let ( lsr ) = shift_right_logical_int

      external shift_right_int : int -> int -> int = "%asrint"
      let ( asr ) = shift_right_int

      external neg_float : float -> float = "%negfloat"
      let ( ~-. ) = neg_float

      external id_float : float -> float = "%identity"
      let ( ~+. ) = id_float

      external add_float : float -> float -> float = "%addfloat"
      let ( +. ) = add_float

      external sub_float : float -> float -> float = "%subfloat"
      let ( -. ) = sub_float

      external mul_float : float -> float -> float = "%mulfloat"
      let ( *. ) = mul_float

      external div_float : float -> float -> float = "%divfloat"
      let ( /. ) = div_float

      external pow_float : float -> float -> float = "caml_power_float" "pow"
        [@@unboxed] [@@noalloc]

      let ( ** ) = pow_float

      external not : bool -> bool = "%boolnot"

      external and_bool : bool -> bool -> bool = "%sequand"
      let ( && ) = and_bool

      external or_bool : bool -> bool -> bool = "%sequor"
      let ( || ) = or_bool

      external revapply : 'a -> ('a -> 'b) -> 'b = "%revapply"
      let ( |> ) = revapply

      external apply : ('a -> 'b) -> 'a -> 'b = "%apply"
      let ( @@ ) = apply

      external string_length : string -> int = "%string_length"
      external bytes_create : int -> bytes = "caml_create_bytes"
      external string_blit : string -> int -> bytes -> int -> int -> unit = "caml_blit_string" [@@noalloc]
      external bytes_unsafe_to_string : bytes -> string = "%bytes_to_string"

      let ( ^ ) left right =
        let left_length = string_length left in
        let right_length = string_length right in
        let output = bytes_create (left_length + right_length) in
        string_blit left 0 output 0 left_length;
        string_blit right 0 output left_length right_length;
        bytes_unsafe_to_string output

      let rec ( @ ) left right =
        match left with
        | [] -> right
        | head :: tail -> head :: (tail @ right)
    |ocaml}
  in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    Ok ()

let test_opened_sibling_double_underscore_exports_typecheck = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _global0_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "global0.ml")
    ~text:{ocaml|
      exception Sys__Not_found

      let sys__getenv _ = ""
      let unix__putenv _ _ = ()
      let unix__environment () = [||]
      let unix__getcwd () = ""
      let unix__chdir _ = ()
    |ocaml}
  in
  let (session, _global0_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "global0.mli")
    ~text:{ocaml|
      exception Sys__Not_found

      val sys__getenv : string -> string
      val unix__putenv : string -> string -> unit
      val unix__environment : unit -> string array
      val unix__getcwd : unit -> string
      val unix__chdir : string -> unit
    |ocaml}
  in
  let (session, env_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "env.ml")
    ~text:{ocaml|
      type 'a option =
        | None
        | Some of 'a

      open Global0

      let getenv var =
        try Some (sys__getenv var) with
        | Sys__Not_found -> None

      let getenv_exn var = sys__getenv var

      let putenv var value = unix__putenv var value

      let unsetenv var = unix__putenv var ""

      let environment () = unix__environment ()

      let getcwd () = unix__getcwd ()

      let chdir path = unix__chdir path
    |ocaml}
  in
  let (session, env_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "env.mli")
    ~text:{ocaml|
      type 'a option =
        | None
        | Some of 'a

      val getenv : string -> string option
      val getenv_exn : string -> string
      val putenv : string -> string -> unit
      val unsetenv : string -> unit
      val environment : unit -> string array
      val getcwd : unit -> string
      val chdir : string -> unit
    |ocaml}
  in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot env_impl_source_id @ diagnostic_strings snapshot env_intf_source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let () = Test.assert_equal
      ~expected:false
      ~actual:(has_signature_error snapshot env_impl_source_id) in
    let () = Test.assert_equal
      ~expected:false
      ~actual:(has_signature_error snapshot env_intf_source_id) in
    let () = Test.assert_equal
      ~expected:(Some "string -> string option")
      ~actual:(export_scheme snapshot env_impl_source_id "getenv") in
    let () = Test.assert_equal
      ~expected:(Some "string -> string")
      ~actual:(export_scheme snapshot env_impl_source_id "getenv_exn") in
    let () = Test.assert_equal
      ~expected:(Some "string -> string -> unit")
      ~actual:(export_scheme snapshot env_impl_source_id "putenv") in
    Ok ()

let test_nonrec_same_name_option_alias_prefers_outer_type = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "types.ml")
    ~text:{ocaml|
type nonrec 'a option = 'a option =
  | None
  | Some of 'a

let value : int option = Some 1
|ocaml}
  in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let value_type = export_scheme snapshot source_id "value" in
    if value_type = Some "int option" then
      Ok ()
    else
      Error ("unexpected exported value type: " ^ show_option value_type)

let test_nonrec_same_name_result_alias_prefers_outer_type = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "types.ml")
    ~text:{ocaml|
type nonrec ('a, 'e) result = ('a, 'e) result =
  | Ok of 'a
  | Error of 'e

let value : (int, string) result = Ok 1
|ocaml}
  in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let value_type = export_scheme snapshot source_id "value" in
    if value_type = Some "(int, string) result" then
      Ok ()
    else
      Error ("unexpected exported value type: " ^ show_option value_type)

let test_source_analysis_with_loaded_modules_canonicalizes_nominal_types = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, actors_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "actors.mli")
    ~text:"module Process: sig\n  type exit_reason = exn\nend\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_actors =
    match Query.module_typings_of seed_snapshot actors_source_id with
    | Some typings -> typings
    | None -> panic "expected actors module typings"
  in
  let loaded_modules = [ loaded_actors ] in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules in
  let source = prepared_source ~filename:"process.mli"
    ~text:{ocaml|
      include module type of Actors.Process
      val spawn: (unit -> (unit, exit_reason) result) -> int
    |ocaml}
  in
  let imported_world = imported_world_of_loaded_modules loaded_modules source in
  let analysis = SourceAnalysis.analyze ~imported_world ~config source in
  let diagnostics = analysis.lowering_diagnostics @ analysis.typing_diagnostics
  |> List.map Diagnostic.to_string in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    match lookup_export "spawn" (FileSummary.exports analysis.file_summary) with
    | None -> Error "expected spawn export"
    | Some spawn_scheme ->
        if scheme_has_named_path spawn_scheme then
          Error ("spawn scheme still contains symbolic named paths: "
          ^ TypePrinter.scheme_to_string spawn_scheme)
        else
          (
            try
              let _ = ModuleTypings.of_file_summary
                ~module_name:"Process"
                ~source_hash:(Source.input_hash source)
                analysis.file_summary
              |> ModuleTypings.Json.to_json in
              Ok ()
            with
            | Failure message -> Error message
          )

let test_source_analysis_with_opened_loaded_module_canonicalizes_nominal_types = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, common_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "common.mli")
    ~text:"type error = int\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_common =
    match Query.module_typings_of seed_snapshot common_source_id with
    | Some typings -> typings
    | None -> panic "expected common module typings"
  in
  let loaded_modules = [ loaded_common ] in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules in
  let source = prepared_source ~filename:"reader.mli"
    ~text:{ocaml|
      open Common
      val close: unit -> (unit, error) result
    |ocaml}
  in
  let imported_world = imported_world_of_loaded_modules loaded_modules source in
  let analysis = SourceAnalysis.analyze ~imported_world ~config source in
  let diagnostics = analysis.lowering_diagnostics @ analysis.typing_diagnostics
  |> List.map Diagnostic.to_string in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    match lookup_export "close" (FileSummary.exports analysis.file_summary) with
    | None -> Error "expected close export"
    | Some close_scheme ->
        if scheme_has_named_path close_scheme then
          Error ("close scheme still contains symbolic named paths: "
          ^ TypePrinter.scheme_to_string close_scheme)
        else
          (
            try
              let _ = ModuleTypings.of_file_summary
                ~module_name:"Reader"
                ~source_hash:(Source.input_hash source)
                analysis.file_summary
              |> ModuleTypings.Json.to_json in
              Ok ()
            with
            | Failure message -> Error message
          )

let test_snapshot_exports_opened_loaded_nominal_types_from_implementation = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, common_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "common.mli")
    ~text:"type error = int\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_common =
    match Query.module_typings_of seed_snapshot common_source_id with
    | Some typings -> typings
    | None -> panic "expected common module typings"
  in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ loaded_common ] in
  let session = Session.empty ~config in
  let (session, source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "reader.ml")
    ~text:{ocaml|
open Common

type ('a, 'e) result =
  | Ok of 'a
  | Error of 'e

let close (): (unit, error) result = Ok ()
|ocaml}
  in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    match Query.module_typings_of snapshot source_id with
    | None -> Error "expected reader module typings"
    | Some typings -> (
        match lookup_export "close" (ModuleTypings.exports typings) with
        | None ->
            Error "expected close export"
        | Some close_scheme when scheme_has_named_path close_scheme ->
            Error ("close scheme still contains symbolic named paths: "
            ^ TypePrinter.scheme_to_string close_scheme)
        | Some _ -> (
            try
              let _ = ModuleTypings.Json.to_json typings in
              Ok ()
            with
            | Failure message -> Error message
          )
      )

let test_snapshot_type_decls_use_opened_sibling_nominal_types = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, common_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "common.mli")
    ~text:"type error = int\n" in
  let (session, reader_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "reader.ml")
    ~text:{ocaml|
      open Common
      type wrapped = Wrap of error
    |ocaml}
  in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot reader_source_id in
  let _ = common_source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    match Query.module_typings_of snapshot reader_source_id with
    | None -> Error "expected reader module typings"
    | Some typings -> (
        try
          let _ = ModuleTypings.Json.to_json typings in
          Ok ()
        with
        | Failure message -> Error message
      )

let test_prepare_snapshot_type_decls_use_opened_sibling_nominal_types = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _common_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "common.mli")
    ~text:"type error = int\n" in
  let (session, reader_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "reader.ml")
    ~text:{ocaml|
      open Common
      type wrapped = Wrap of error
    |ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ reader_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot reader_source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        match Query.module_typings_of snapshot reader_source_id with
        | None -> Error "expected reader module typings"
        | Some typings -> (
            try
              let _ = ModuleTypings.Json.to_json typings in
              Ok ()
            with
            | Failure message -> Error message
          )

let test_prepare_snapshot_type_decls_use_opened_loaded_nominal_types = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, common_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "common.mli")
    ~text:"type error = int\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_common =
    match Query.module_typings_of seed_snapshot common_source_id with
    | Some typings -> typings
    | None -> panic "expected common module typings"
  in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ loaded_common ] in
  let session = Session.empty ~config in
  let (session, reader_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "reader.ml")
    ~text:{ocaml|
      open Common
      type wrapped = Wrap of error
    |ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ reader_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot reader_source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        match Query.module_typings_of snapshot reader_source_id with
        | None -> Error "expected reader module typings"
        | Some typings -> (
            try
              let _ = ModuleTypings.Json.to_json typings in
              Ok ()
            with
            | Failure message -> Error message
          )

let test_prepare_snapshot_type_decls_use_opened_loaded_nominal_types_with_underscored_module_name = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, parser_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "markdown_parser.mli")
    ~text:"type inline_node = Text of string\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_parser =
    match Query.module_typings_of seed_snapshot parser_source_id with
    | Some typings -> typings
    | None -> panic "expected markdown_parser module typings"
  in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ loaded_parser ] in
  let session = Session.empty ~config in
  let (session, reader_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "markdown_lower.ml")
    ~text:{ocaml|
      open Markdown_parser
      type inline_stack_item = Inline_node of inline_node
    |ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ reader_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot reader_source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        match Query.module_typings_of snapshot reader_source_id with
        | None -> Error "expected markdown_lower module typings"
        | Some typings -> (
            try
              let _ = ModuleTypings.Json.to_json typings in
              Ok ()
            with
            | Failure message -> Error message
          )

let test_prepare_snapshot_polyvariant_exports_canonicalize_sibling_structural_types = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _ansi_table_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "ansi_table.ml")
    ~text:"let to_rgb = [| `rgb (0, 0, 0) |]\n" in
  let (session, colors_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "colors.ml")
    ~text:{ocaml|
      type rgb = [ `rgb of int * int * int ]
      module ANSI = struct
        let first = Ansi_table.to_rgb.(0)
      end
    |ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ colors_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot colors_source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        let actual = export_scheme snapshot colors_source_id "ANSI.first" in
        let expected = Some "rgb" in
        if actual = expected then
          Ok ()
        else
          Error ("expected sibling structural polyvariant export scheme "
          ^ Option.unwrap_or ~default:"<none>" expected
          ^ " but got "
          ^ Option.unwrap_or ~default:"<none>" actual)

let test_prepare_snapshot_polyvariant_exports_canonicalize_sibling_structural_types_inside_arrows = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _ansi_table_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "ansi_table.ml")
    ~text:"let to_rgb = [| `rgb (0, 0, 0) |]\n" in
  let (session, colors_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "colors.ml")
    ~text:{ocaml|
      type ansi = [ `ansi of int ]
      type rgb = [ `rgb of int * int * int ]
      module ANSI = struct
        let to_rgb = fun (`ansi i) ->
          let _ = i in
          Ansi_table.to_rgb.(0)
      end
    |ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ colors_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot colors_source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        let actual = export_scheme snapshot colors_source_id "ANSI.to_rgb" in
        let expected = Some "ansi -> rgb" in
        if actual = expected then
          Ok ()
        else
          Error ("expected sibling structural polyvariant arrow export scheme "
          ^ Option.unwrap_or ~default:"<none>" expected
          ^ " but got "
          ^ Option.unwrap_or ~default:"<none>" actual)

let test_module_alias_reexports_loaded_module_typings = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, helpers_source_id) = create_source seed_session ~kind:Source.File ~origin:(Source.Label "helpers.ml")
    ~text:{ocaml|
type 'a option =
  | None
  | Some of 'a

let id x = x

let wrap value = Some value
|ocaml}
  in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_helpers =
    match Query.module_typings_of seed_snapshot helpers_source_id with
    | Some typings -> typings
    | None -> panic "expected helper module typings"
  in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ loaded_helpers ] in
  let session = Session.empty ~config in
  let source = "module Util = Helpers\nlet answer = Util.wrap (Util.id 1)\n" in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "consumer.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let util_id_type = export_scheme snapshot source_id "Util.id" in
    let util_wrap_type = export_scheme snapshot source_id "Util.wrap" in
    let answer_type = export_scheme snapshot source_id "answer" in
    let exported_names = export_names (Query.export_of snapshot source_id) in
    if not (exported_names = [ "answer"; "Util.id"; "Util.wrap" ]) then
      Error ("unexpected exported names: " ^ String.concat ", " exported_names)
    else if not (util_id_type = Some "'a. 'a -> 'a") then
      Error ("unexpected Util.id type: " ^ show_option util_id_type)
    else if not
        (util_wrap_type
        |> Option.map (fun text -> Option.is_some (offset_of_substring text "option"))
        |> Option.unwrap_or ~default:false) then
      Error ("unexpected Util.wrap type: " ^ show_option util_wrap_type)
    else if not
        (answer_type
        |> Option.map (fun text -> Option.is_some (offset_of_substring text "option"))
        |> Option.unwrap_or ~default:false) then
      Error ("unexpected answer type: " ^ show_option answer_type)
    else
      Ok ()

let test_module_alias_reexports_same_named_local_modules = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _cell_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "cell.ml")
    ~text:"let create value = value\n" in
  let (session, sync_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "sync.ml")
    ~text:"module Cell = Cell\nlet answer = Cell.create 1\n" in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot sync_source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let create_type = export_scheme snapshot sync_source_id "Cell.create" in
    let answer_type = export_scheme snapshot sync_source_id "answer" in
    let exported_names = export_names (Query.export_of snapshot sync_source_id) in
    if not (exported_names = [ "answer"; "Cell.create" ]) then
      Error ("unexpected exported names: " ^ String.concat ", " exported_names)
    else if not (create_type = Some "'a. 'a -> 'a") then
      Error ("unexpected Cell.create type: " ^ show_option create_type)
    else if not (answer_type = Some "int") then
      Error ("unexpected answer type: " ^ show_option answer_type)
    else
      Ok ()

let test_loaded_module_typings_preserve_nested_same_named_alias_exports = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, _cell_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "cell.ml")
    ~text:"let create value = value\n" in
  let (seed_session, _sync_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "sync.ml")
    ~text:"module Cell = Cell\n" in
  let (seed_session, std_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "std.ml")
    ~text:"module Sync = Sync\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let std_summary =
    match Query.module_typings_of seed_snapshot std_source_id with
    | Some typings -> typings
    | None -> panic "expected std module typings"
  in
  let std_exported_names = module_typings_export_names std_summary in
  let client_config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ std_summary ] in
  let client_session = Session.empty ~config:client_config in
  let (client_session, client_source_id) = create_source
    client_session
    ~kind:Source.File
    ~origin:(Source.Label "client.ml")
    ~text:"open Std.Sync\nlet answer = Cell.create 1\n" in
  if not (List.equal String.equal std_exported_names [ "Sync.Cell.create" ]) then
    Error ("unexpected std exports: " ^ String.concat ", " std_exported_names)
  else
    match Session.prepare_snapshot client_session ~roots:[ client_source_id ] with
    | Error missing -> Error ("missing requirements: "
    ^ (Session.MissingRequirements.to_json missing |> Data.Json.to_string))
    | Ok client_snapshot ->
        let client_diagnostics = diagnostic_strings client_snapshot client_source_id in
        if not (List.is_empty client_diagnostics) then
          Error (String.concat "\n" client_diagnostics)
        else
          let answer_type = export_scheme client_snapshot client_source_id "answer" in
          let () = Test.assert_equal ~expected:(Some "int") ~actual:answer_type in
          Ok ()

let test_paired_loaded_module_typings_preserve_nested_alias_exports_across_include_chain = fun _ctx ->
  let kernel_seed_session = Session.empty ~config:Config.default in
  let (kernel_seed_session, _cell_impl_source_id) = create_source
    kernel_seed_session
    ~kind:Source.File
    ~origin:(Source.Label "cell.ml")
    ~text:"let create value = value\n" in
  let (kernel_seed_session, _cell_intf_source_id) = create_source
    kernel_seed_session
    ~kind:Source.File
    ~origin:(Source.Label "cell.mli")
    ~text:"val create : 'a -> 'a\n" in
  let (kernel_seed_session, _sync_impl_source_id) = create_source
    kernel_seed_session
    ~kind:Source.File
    ~origin:(Source.Label "sync.ml")
    ~text:"module Cell = Cell\n" in
  let (kernel_seed_session, _sync_intf_source_id) = create_source
    kernel_seed_session
    ~kind:Source.File
    ~origin:(Source.Label "sync.mli")
    ~text:"module Cell = Cell\n" in
  let (kernel_seed_session, kernel_source_id) = create_source
    kernel_seed_session
    ~kind:Source.File
    ~origin:(Source.Label "kernel.ml")
    ~text:"module Sync = Sync\n" in
  let (kernel_seed_session, _kernel_intf_source_id) = create_source
    kernel_seed_session
    ~kind:Source.File
    ~origin:(Source.Label "kernel.mli")
    ~text:"module Sync = Sync\n" in
  let kernel_seed_snapshot = Session.snapshot kernel_seed_session in
  let kernel_summary =
    match Query.module_typings_of kernel_seed_snapshot kernel_source_id with
    | Some typings -> typings
    | None -> panic "expected kernel module typings"
  in
  let kernel_exported_names = module_typings_export_names kernel_summary in
  let kernel_create_type = ModuleTypings.exports kernel_summary
  |> lookup_export "Sync.Cell.create"
  |> Option.map TypePrinter.scheme_to_string in
  if not (List.equal String.equal kernel_exported_names [ "Sync.Cell.create" ]) then
    Error ("unexpected kernel exports: " ^ String.concat ", " kernel_exported_names)
  else if not (kernel_create_type = Some "'a. 'a -> 'a") then
    Error ("unexpected kernel Sync.Cell.create type: " ^ show_option kernel_create_type)
  else
    let std_seed_config = Config.default
    |> Config.with_loaded_modules ~loaded_modules:[ kernel_summary ] in
    let std_seed_session = Session.empty ~config:std_seed_config in
    let (std_seed_session, _sync_impl_source_id) = create_source
      std_seed_session
      ~kind:Source.File
      ~origin:(Source.Label "sync.ml")
      ~text:"include Kernel.Sync\n" in
    let (std_seed_session, _sync_intf_source_id) = create_source
      std_seed_session
      ~kind:Source.File
      ~origin:(Source.Label "sync.mli")
      ~text:"include module type of Kernel.Sync\n" in
    let (std_seed_session, std_source_id) = create_source
      std_seed_session
      ~kind:Source.File
      ~origin:(Source.Label "std.ml")
      ~text:"module Sync = Sync\n" in
    let (std_seed_session, _std_intf_source_id) = create_source
      std_seed_session
      ~kind:Source.File
      ~origin:(Source.Label "std.mli")
      ~text:"module Sync = Sync\n" in
    let std_seed_snapshot = Session.snapshot std_seed_session in
    let std_summary =
      match Query.module_typings_of std_seed_snapshot std_source_id with
      | Some typings -> typings
      | None -> panic "expected std module typings"
    in
    let std_exported_names = module_typings_export_names std_summary in
    let std_create_type = ModuleTypings.exports std_summary
    |> lookup_export "Sync.Cell.create"
    |> Option.map TypePrinter.scheme_to_string in
    if not (List.equal String.equal std_exported_names [ "Sync.Cell.create" ]) then
      Error ("unexpected std exports: " ^ String.concat ", " std_exported_names)
    else if not (std_create_type = Some "'a. 'a -> 'a") then
      Error ("unexpected std Sync.Cell.create type: " ^ show_option std_create_type)
    else
      let client_config = Config.default
      |> Config.with_loaded_modules ~loaded_modules:[ std_summary ] in
      let client_session = Session.empty ~config:client_config in
      let (client_session, client_source_id) = create_source
        client_session
        ~kind:Source.File
        ~origin:(Source.Label "client.ml")
        ~text:"open Std.Sync\nlet answer = Cell.create 1\n" in
      match Session.prepare_snapshot client_session ~roots:[ client_source_id ] with
      | Error missing -> Error ("missing requirements: "
      ^ (Session.MissingRequirements.to_json missing |> Data.Json.to_string))
      | Ok client_snapshot ->
          let client_diagnostics = diagnostic_strings client_snapshot client_source_id in
          if not (List.is_empty client_diagnostics) then
            Error (String.concat "\n" client_diagnostics)
          else
            let callee_offset = offset_of_substring "open Std.Sync\nlet answer = Cell.create 1\n" "Cell.create"
            |> Option.expect ~msg:"expected Cell.create in client source" in
            let answer_type = export_scheme client_snapshot client_source_id "answer" in
            let callee_type = inferred_type_at client_snapshot client_source_id callee_offset in
            if answer_type = Some "int" then
              Ok ()
            else
              Error ("unexpected answer type: "
              ^ show_option answer_type
              ^ " callee="
              ^ show_option callee_type)

let test_prepare_snapshot_uses_nested_exports_from_local_include_of_loaded_module = fun _ctx ->
  let kernel_seed_session = Session.empty ~config:Config.default in
  let (kernel_seed_session, _cell_impl_source_id) = create_source kernel_seed_session ~kind:Source.File ~origin:(Source.Label "cell.ml")
    ~text:{ocaml|
let create value = value
|ocaml}
  in
  let (kernel_seed_session, _cell_intf_source_id) = create_source kernel_seed_session ~kind:Source.File ~origin:(Source.Label "cell.mli")
    ~text:{ocaml|
val create : 'a -> 'a
|ocaml}
  in
  let (kernel_seed_session, _sync_impl_source_id) = create_source kernel_seed_session ~kind:Source.File ~origin:(Source.Label "sync.ml")
    ~text:{ocaml|
module Cell = Cell
|ocaml}
  in
  let (kernel_seed_session, _sync_intf_source_id) = create_source kernel_seed_session ~kind:Source.File ~origin:(Source.Label "sync.mli")
    ~text:{ocaml|
module Cell = Cell
|ocaml}
  in
  let (kernel_seed_session, kernel_impl_source_id) = create_source kernel_seed_session ~kind:Source.File ~origin:(Source.Label "kernel.ml")
    ~text:{ocaml|
module Sync = Sync
|ocaml}
  in
  let (kernel_seed_session, _kernel_intf_source_id) = create_source kernel_seed_session ~kind:Source.File ~origin:(Source.Label "kernel.mli")
    ~text:{ocaml|
module Sync = Sync
|ocaml}
  in
  let kernel_seed_snapshot = Session.snapshot kernel_seed_session in
  let kernel_summary =
    match Query.module_typings_of kernel_seed_snapshot kernel_impl_source_id with
    | Some typings -> typings
    | None -> panic "expected kernel module typings"
  in
  let std_config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ kernel_summary ] in
  let std_session = Session.empty ~config:std_config in
  let (std_session, _sync_impl_source_id) = create_source std_session ~kind:Source.File ~origin:(Source.Label "sync.ml")
    ~text:{ocaml|
include Kernel.Sync
|ocaml}
  in
  let (std_session, _sync_intf_source_id) = create_source std_session ~kind:Source.File ~origin:(Source.Label "sync.mli")
    ~text:{ocaml|
include module type of Kernel.Sync
|ocaml}
  in
  let consumer_source = {ocaml|
open Sync

let answer = Cell.create 1
|ocaml}
  in
  let (std_session, consumer_source_id) = create_source
    std_session
    ~kind:Source.File
    ~origin:(Source.Label "consumer.ml")
    ~text:consumer_source in
  match Session.prepare_snapshot std_session ~roots:[ consumer_source_id ] with
  | Error missing -> Error ("missing requirements: "
  ^ (Session.MissingRequirements.to_json missing |> Data.Json.to_string))
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot consumer_source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        let answer_type = export_scheme snapshot consumer_source_id "answer" in
        let callee_offset = offset_of_substring consumer_source "Cell.create" |> Option.expect ~msg:"expected Cell.create in consumer source" in
        let callee_type = inferred_type_at snapshot consumer_source_id callee_offset in
        let () = Test.assert_equal ~expected:(Some "int") ~actual:answer_type in
        let () = Test.assert_equal ~expected:(Some "int -> int") ~actual:callee_type in
        Ok ()

let test_sibling_source_uses_loaded_module_record_reexport = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, helpers_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "helpers.ml")
    ~text:"type point = { x: int; y: int }\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_helpers =
    match Query.module_typings_of seed_snapshot helpers_source_id with
    | Some typings -> typings
    | None -> panic "expected helper module typings"
  in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ loaded_helpers ] in
  let session = Session.empty ~config in
  let consumer_source = {ocaml|
    include Helpers
    let origin = { x = 0; y = 0 }
  |ocaml}
  in
  let (session, _consumer_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "consumer.ml")
    ~text:consumer_source in
  let client_source = "let total = Consumer.origin.x + Consumer.origin.y\n" in
  let (session, client_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "client.ml")
    ~text:client_source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot client_source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let field_access_offset =
      let access_start = offset_of_substring client_source "Consumer.origin.x +"
      |> Option.expect ~msg:"expected record field access in test source" in
      access_start + String.length "Consumer.origin."
    in
    let total_type = export_scheme snapshot client_source_id "total" in
    let field_access_type = inferred_type_at snapshot client_source_id field_access_offset in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:total_type in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:field_access_type in
    Ok ()

let test_prepare_snapshot_is_rooted = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, colors_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"module RGB = struct let blend x y = x end\nlet to_string value = value\n" in
  let (session, demo_source_id) = create_source
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
      let actual = Session.MissingRequirements.to_json missing in
      let expected = Data.Json.Array [
        Data.Json.Object [
          ("tag", Data.Json.String "missing_root_source");
          ("source_id", Data.Json.Int 99);
        ]
      ] in
      let () = Test.assert_equal ~expected ~actual in
      Ok ()

let test_prepare_snapshot_reports_missing_module_summary = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "uses_missing_module.ml")
    ~text:"open Missing_module\nlet answer = 1\n" in
  match Session.prepare_snapshot session ~roots:[ source_id ] with
  | Ok _ -> Error "expected rooted snapshot preparation to report missing module summaries"
  | Error missing ->
      let actual = Session.MissingRequirements.to_json missing |> Data.Json.to_string in
      let expected = Data.Json.Array [
        Data.Json.Object [
          ("tag", Data.Json.String "missing_module_summary");
          ("module_name", Data.Json.String "Missing_module");
          ("requested_by", Data.Json.Array [ Data.Json.Int (SourceId.to_int source_id) ]);
        ]
      ]
      |> Data.Json.to_string in
      if String.equal expected actual then
        Ok ()
      else
        Error (String.concat "\n" [ "expected"; expected; "actual"; actual ])

let test_prepare_snapshot_collects_transitive_missing_modules = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "dependent.ml")
    ~text:"open Inner\nlet answer = 1\n" in
  let (session, _inner_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "inner.ml")
    ~text:"open Missing_module\nlet value = 42\n" in
  match Session.prepare_snapshot session ~roots:[ source_id ] with
  | Ok _ -> Error "expected rooted snapshot preparation to report transitive missing module summaries"
  | Error missing ->
      let actual = Session.MissingRequirements.to_json missing |> Data.Json.to_string in
      let expected = Data.Json.Array [
        Data.Json.Object [
          ("tag", Data.Json.String "missing_module_summary");
          ("module_name", Data.Json.String "Missing_module");
          ("requested_by", Data.Json.Array [ Data.Json.Int 1 ]);
        ]
      ]
      |> Data.Json.to_string in
      if String.equal expected actual then
        Ok ()
      else
        Error (String.concat "\n" [ "expected"; expected; "actual"; actual ])

let test_prepare_snapshot_collects_missing_module_for_qualified_reference = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "qualified.ml")
    ~text:"let answer = Missing_module.value 1\n" in
  match Session.prepare_snapshot session ~roots:[ source_id ] with
  | Ok _ -> Error "expected rooted snapshot preparation to report missing module summaries for qualified access"
  | Error missing ->
      let actual = Session.MissingRequirements.to_json missing |> Data.Json.to_string in
      let expected = Data.Json.Array [
        Data.Json.Object [
          ("tag", Data.Json.String "missing_module_summary");
          ("module_name", Data.Json.String "Missing_module");
          ("requested_by", Data.Json.Array [ Data.Json.Int (SourceId.to_int source_id) ]);
        ]
      ]
      |> Data.Json.to_string in
      if String.equal expected actual then
        Ok ()
      else
        Error (String.concat "\n" [ "expected"; expected; "actual"; actual ])

let test_prepare_snapshot_keeps_nested_sibling_modules_out_of_top_level_requirements = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _provider_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "provider.ml")
    ~text:"module Missing_module = struct let value = 42 end\n" in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "consumer.ml")
    ~text:"open Missing_module\nlet answer = value\n" in
  match Session.prepare_snapshot session ~roots:[ source_id ] with
  | Ok _ -> Error "expected rooted snapshot preparation to keep sibling nested modules out of top-level module requirements"
  | Error missing ->
      let actual = Session.MissingRequirements.to_json missing |> Data.Json.to_string in
      let expected = Data.Json.Array [
        Data.Json.Object [
          ("tag", Data.Json.String "missing_module_summary");
          ("module_name", Data.Json.String "Missing_module");
          ("requested_by", Data.Json.Array [ Data.Json.Int (SourceId.to_int source_id) ]);
        ]
      ]
      |> Data.Json.to_string in
      if String.equal expected actual then
        Ok ()
      else
        Error (String.concat "\n" [ "expected"; expected; "actual"; actual ])

let test_prepare_snapshot_keeps_loaded_nested_module_exports_out_of_top_level_requirements = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, colors_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"module RGB = struct let blend x y = x end\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_colors =
    match Query.module_typings_of seed_snapshot colors_source_id with
    | Some typings -> typings
    | None -> panic "expected colors module typings"
  in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ loaded_colors ] in
  let session = Session.empty ~config in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "consumer.ml")
    ~text:"let midpoint = RGB.blend 1 2\n" in
  match Session.prepare_snapshot session ~roots:[ source_id ] with
  | Ok _ -> Error "expected rooted snapshot preparation to keep loaded nested module exports out of top-level module requirements"
  | Error missing ->
      let actual = Session.MissingRequirements.to_json missing |> Data.Json.to_string in
      let expected = Data.Json.Array [
        Data.Json.Object [
          ("tag", Data.Json.String "missing_module_summary");
          ("module_name", Data.Json.String "RGB");
          ("requested_by", Data.Json.Array [ Data.Json.Int (SourceId.to_int source_id) ]);
        ]
      ]
      |> Data.Json.to_string in
      if String.equal expected actual then
        Ok ()
      else
        Error (String.concat "\n" [ "expected"; expected; "actual"; actual ])

let test_prepare_snapshot_canonicalizes_missing_requirements = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, first_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "first.ml")
    ~text:"open Missing_module\nlet first = 1\n" in
  let (session, second_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "second.ml")
    ~text:"open Missing_module\nlet second = 2\n" in
  let missing_root_a = SourceId.of_int 99 in
  let missing_root_b = SourceId.of_int 42 in
  match Session.prepare_snapshot
    session
    ~roots:[ second_source_id; missing_root_a; first_source_id; second_source_id; missing_root_b ] with
  | Ok _ -> Error "expected rooted snapshot preparation to report canonical missing requirements"
  | Error missing ->
      let actual = Session.MissingRequirements.to_json missing |> Data.Json.to_string in
      let expected = Data.Json.Array [
        Data.Json.Object [
          ("tag", Data.Json.String "missing_root_source");
          ("source_id", Data.Json.Int 42);
        ];
        Data.Json.Object [
          ("tag", Data.Json.String "missing_root_source");
          ("source_id", Data.Json.Int 99);
        ];
        Data.Json.Object [
          ("tag", Data.Json.String "missing_module_summary");
          ("module_name", Data.Json.String "Missing_module");
          (
            "requested_by",
            Data.Json.Array [
              Data.Json.Int (SourceId.to_int first_source_id);
              Data.Json.Int (SourceId.to_int second_source_id);
            ]
          );
        ];
      ]
      |> Data.Json.to_string in
      if String.equal expected actual then
        Ok ()
      else
        Error (String.concat "\n" [ "expected"; expected; "actual"; actual ])

let test_prepare_snapshot_sorts_missing_modules_by_name = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, zed_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "zed_consumer.ml")
    ~text:"open Zed\nlet z = 1\n" in
  let (session, alpha_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "alpha_consumer.ml")
    ~text:"open Alpha\nlet a = 2\n" in
  match Session.prepare_snapshot session ~roots:[ zed_source_id; alpha_source_id ] with
  | Ok _ -> Error "expected rooted snapshot preparation to report sorted missing module summaries"
  | Error missing ->
      let actual = Session.MissingRequirements.to_json missing |> Data.Json.to_string in
      let expected = Data.Json.Array [
        Data.Json.Object [
          ("tag", Data.Json.String "missing_module_summary");
          ("module_name", Data.Json.String "Alpha");
          ("requested_by", Data.Json.Array [ Data.Json.Int (SourceId.to_int alpha_source_id) ]);
        ];
        Data.Json.Object [
          ("tag", Data.Json.String "missing_module_summary");
          ("module_name", Data.Json.String "Zed");
          ("requested_by", Data.Json.Array [ Data.Json.Int (SourceId.to_int zed_source_id) ]);
        ];
      ]
      |> Data.Json.to_string in
      if String.equal expected actual then
        Ok ()
      else
        Error (String.concat "\n" [ "expected"; expected; "actual"; actual ])

let test_prepare_snapshot_uses_local_prelude_variant_constructors = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _prelude_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "prelude.ml")
    ~text:{ocaml|
      type 'a option =
        | None
        | Some of 'a

      type ('ok, 'error) result =
        | Ok of 'ok
        | Error of 'error
    |ocaml}
  in
  let (session, source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "consumer.ml")
    ~text:{ocaml|
      open Prelude

      let wrap value = Some value

      let classify = fun value ->
        match value with
        | Some value -> Ok value
        | None -> Error 0
    |ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else if has_unbound_name snapshot source_id then
        Error "expected rooted analysis to resolve Prelude constructors from sibling module typings"
      else
        let wrap_type = export_scheme snapshot source_id "wrap" in
        let classify_type = export_scheme snapshot source_id "classify" in
        let () = Test.assert_equal ~expected:true ~actual:(Option.is_some wrap_type) in
        let () = Test.assert_equal ~expected:true ~actual:(Option.is_some classify_type) in
        Ok ()

let test_prepare_snapshot_uses_sibling_int_module_with_local_prelude = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _prelude_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "prelude.ml")
    ~text:{ocaml|
      type 'a option =
        | None
        | Some of 'a
    |ocaml}
  in
  let (session, _int_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "int.ml")
    ~text:{ocaml|
      let to_string _value = "n"
    |ocaml}
  in
  let (session, source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "consumer.ml")
    ~text:{ocaml|
      open Prelude

      let render = fun value ->
        match value with
        | Some value -> Int.to_string value
        | None -> "none"
    |ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        let render_type = export_scheme snapshot source_id "render" in
        if
          render_type
          |> Option.map (fun text -> Option.is_some (offset_of_substring text "string"))
          |> Option.unwrap_or ~default:false
        then
          Ok ()
        else
          Error ("unexpected render type: " ^ show_option render_type)

let test_prepare_snapshot_types_mutable_record_field_assignment = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "counter.ml")
    ~text:{ocaml|
      type counter = {
        mutable value: int;
      }

      let bump = fun counter ->
        counter.value <- counter.value + 1;
        counter.value
    |ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        let bump_type = export_scheme snapshot source_id "bump" in
        if bump_type = Some "counter -> int" then
          Ok ()
        else
          Error ("unexpected bump type: " ^ show_option bump_type)

let test_prepare_snapshot_types_local_source_module_pack = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "source.ml")
    ~text:{ocaml|
      type ('ok, 'error) result =
        | Ok of 'ok
        | Error of 'error

      module Async = struct
        module Token = struct
          type t = int
        end

        module Adapter = struct
          type error = unit

          module Selector = struct
            type t = unit

            let register = fun _selector ~(fd: int) ~token:_ ~interest:_ -> Ok ()

            let reregister = fun _selector ~(fd: int) ~token:_ ~interest:_ -> Ok ()

            let deregister = fun _selector ~(fd: int) -> Ok ()
          end
        end

        module Source = struct
          module type Intf = sig
            type t

            val register:
              t -> Adapter.Selector.t -> Token.t -> int -> (unit, Adapter.error) result

            val reregister:
              t -> Adapter.Selector.t -> Token.t -> int -> (unit, Adapter.error) result

            val deregister:
              t -> Adapter.Selector.t -> (unit, Adapter.error) result
          end

          type t =
            | S: (module Intf with type t = 'state) * 'state -> t

          let make = fun implementation state -> S (implementation, state)
        end
      end

      type t = int

      let to_source = fun fd ->
        let module Source = struct
          type nonrec t = t

          let register = fun fd selector token interest ->
            Async.Adapter.Selector.register selector ~fd ~token ~interest

          let reregister = fun fd selector token interest ->
            Async.Adapter.Selector.reregister selector ~fd ~token ~interest

          let deregister = fun fd selector ->
            Async.Adapter.Selector.deregister selector ~fd
        end in
        Async.Source.make (module Source) fd
    |ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot source_id in
      if not (List.is_empty diagnostics) then
        Error (String.concat "\n" diagnostics)
      else
        let to_source_type = export_scheme snapshot source_id "to_source" in
        if
          to_source_type = Some "t -> Async.Source.t" || to_source_type = Some "int -> Async.Source.t"
        then
          Ok ()
        else
          Error ("unexpected to_source type: " ^ show_option to_source_type)

let test_check_source_recovers_when_snapshot_preparation_reports_missing_module_summaries = fun _ctx ->
  let report = check_source_text ~filename:(Path.v "uses_missing_module.ml") "open Missing_module\nlet answer = Missing_module.value 1\n" in
  let diagnostics = List.length report.parse_diagnostics
  + List.length report.lowering_diagnostics
  + List.length report.typing_diagnostics in
  if diagnostics > 0 then
    Ok ()
  else
    Error "expected single-source session checking to surface diagnostics instead of panicking"

let test_match_guards_typecheck_in_pattern_scope = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
    type 'a option =
      | None
      | Some of 'a

    let classify value =
      match value with
      | Some n when n > 0 -> n
      | Some _ -> 0
      | None -> 0
  |ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "match_guard.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let guard_binding_offset = offset_of_substring source "n > 0" |> Option.expect ~msg:"expected match guard in test source" in
    let classify_type = export_scheme snapshot source_id "classify" in
    let guard_binding_type = inferred_type_at snapshot source_id guard_binding_offset in
    if not (classify_type = Some "int option -> int") then
      Error ("unexpected classify type: " ^ show_option classify_type)
    else if not (guard_binding_type = Some "int") then
      Error ("unexpected guard binding type: " ^ show_option guard_binding_type)
    else
      Ok ()

let test_optional_arguments_can_be_omitted_and_reordered = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
    let make_key = fun ?(kind = 0) ?(mods = 1) code -> code + kind + mods
    let omitted = make_key 3
    let reordered = make_key ~mods:4 3
    let explicit = make_key ~kind:5 ~mods:6 7
  |ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "optional_apply.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let reordered_offset = offset_of_substring source "make_key ~mods:4 3" |> Option.expect ~msg:"expected reordered call in test source" in
    let explicit_offset = offset_of_substring source "make_key ~kind:5 ~mods:6 7"
    |> Option.expect ~msg:"expected explicit call in test source" in
    let make_key_type = export_scheme snapshot source_id "make_key" in
    let omitted_type = export_scheme snapshot source_id "omitted" in
    let reordered_type = export_scheme snapshot source_id "reordered" in
    let explicit_type = export_scheme snapshot source_id "explicit" in
    let reordered_callee_type = inferred_type_at snapshot source_id reordered_offset in
    let explicit_callee_type = inferred_type_at snapshot source_id explicit_offset in
    let () = Test.assert_equal ~expected:(Some "?kind:int -> ?mods:int -> int -> int") ~actual:make_key_type in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:omitted_type in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:reordered_type in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:explicit_type in
    let () = Test.assert_equal ~expected:(Some "?kind:int -> ?mods:int -> int -> int") ~actual:reordered_callee_type in
    let () = Test.assert_equal ~expected:(Some "?kind:int -> ?mods:int -> int -> int") ~actual:explicit_callee_type in
    Ok ()

let test_optional_argument_forwarding_preserves_option_wrapper = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
type selector = unit

type ('a, 'e) result =
  | Ok of 'a
  | Error of 'e

module Selector = struct
  type t = selector

  let select : ?timeout:int64 -> ?max_events:int -> t -> (unit, string) result =
    fun ?timeout:_ ?max_events:_ _ ->
      Ok ()
end

let poll = fun ?max_events ?timeout selector ->
  Selector.select ?timeout ?max_events selector
|ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "optional_forwarding.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let poll_type = export_scheme snapshot source_id "poll" in
    if poll_type = Some "?max_events:int -> ?timeout:int64 -> unit -> (unit, string) result" then
      Ok ()
    else
      Error ("unexpected poll type: " ^ show_option poll_type)

let test_inline_record_constructor_payloads_use_constructor_owner = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
type error =
  | Invalid_nanoseconds of { nanos: int }

type t = {
  secs: int;
  nanos: int;
}

let make_error = fun nanos -> Invalid_nanoseconds { nanos }
|ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "inline_record_constructor_payload.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let make_error_type = export_scheme snapshot source_id "make_error" in
    if make_error_type = Some "int -> error" then
      Ok ()
    else
      Error ("unexpected make_error type: " ^ show_option make_error_type)

let test_records_flow_through_snapshot_queries = fun _ctx ->
  let expect label expected actual =
    if actual = expected then
      Ok ()
    else
      Error (
        label ^ ": expected " ^ (
          match expected with
          | Some value -> value
          | None -> "<none>"
        ) ^ " but got " ^ (
          match actual with
          | Some value -> value
          | None -> "<none>"
        )
      )
  in
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
    type point = { x: int; y: int }
    let origin = { x = 0; y = 0 }
    let move_x point dx = { point with x = point.x + dx }
    let total = fun { x; y } -> x + y
    let answer = total (move_x origin 3)
  |ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "records.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let field_access_offset =
      let access_start = offset_of_substring source "point.x +" |> Option.expect ~msg:"expected record field access in test source" in
      access_start + String.length "point."
    in
    let origin_type = export_scheme snapshot source_id "origin" in
    let move_x_type = export_scheme snapshot source_id "move_x" in
    let total_type = export_scheme snapshot source_id "total" in
    let answer_type = export_scheme snapshot source_id "answer" in
    let field_access_type = inferred_type_at snapshot source_id field_access_offset in
    match expect "origin type" (Some "point") origin_type with
    | Error _ as error -> error
    | Ok () -> (
        match expect "move_x type" (Some "point -> int -> point") move_x_type with
        | Error _ as error -> error
        | Ok () -> (
            match expect "total type" (Some "point -> int") total_type with
            | Error _ as error -> error
            | Ok () -> (
                match expect "answer type" (Some "int") answer_type with
                | Error _ as error -> error
                | Ok () -> expect "field access type" (Some "int") field_access_type
              )
          )
      )

let test_fun_cases_keep_preceding_parameters = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = "let choose = fun base ~delta ->\n"
  ^ "  function\n"
  ^ "  | true -> base + delta\n"
  ^ "  | false -> base\n"
  ^ "\n"
  ^ "let picked = choose 1 ~delta:2 true\n" in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "fun_cases_with_params.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let delta_offset = offset_of_substring source "base + delta"
    |> Option.expect ~msg:"expected labeled parameter reference in test source"
    |> fun start -> start + String.length "base + " in
    let choose_type = export_scheme snapshot source_id "choose" in
    let picked_type = export_scheme snapshot source_id "picked" in
    let delta_type = inferred_type_at snapshot source_id delta_offset in
    let () = Test.assert_equal ~expected:(Some "int -> ~delta:int -> bool -> int") ~actual:choose_type in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:picked_type in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:delta_type in
    Ok ()

let test_explicit_locally_abstract_let_annotations_are_checked = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
let id : type a. a -> a = fun x -> x
|ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "locally_abstract_let_annotation.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let id_type = export_scheme snapshot source_id "id" in
    if id_type = Some "'a. 'a -> 'a" then
      Ok ()
    else
      Error ("unexpected id type: " ^ show_option id_type)

let test_for_loops_lower_and_typecheck = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
let loop limit =
  for i = 0 to limit do
    let _ = i in
    ()
  done

let countdown start =
  for i = start downto 0 do
    let _ = i in
    ()
  done
|ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "for_loops.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let loop_type = export_scheme snapshot source_id "loop" in
    let countdown_type = export_scheme snapshot source_id "countdown" in
    if loop_type = Some "int -> unit" && countdown_type = Some "int -> unit" then
      Ok ()
    else
      Error ("unexpected loop types: loop=" ^ show_option loop_type ^ ", countdown=" ^ show_option countdown_type)

let test_if_branches_do_not_capture_trailing_sequences = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
let implicit flag =
  if flag then
    ()
  ;
  "ok"

let explicit flag =
  if flag then
    ()
  else
    ()
  ;
  "ok"
|ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "if_sequence.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let implicit_type = export_scheme snapshot source_id "implicit" in
    let explicit_type = export_scheme snapshot source_id "explicit" in
    if implicit_type = Some "bool -> string" && explicit_type = Some "bool -> string" then
      Ok ()
    else
      Error ("unexpected if/sequence types: implicit="
      ^ show_option implicit_type
      ^ ", explicit="
      ^ show_option explicit_type)

let test_let_operator_lower_and_typecheck = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
type error =
  | Boom

type ('a, 'e) result =
  | Ok of 'a
  | Error of 'e

module Result = struct
  let and_then result fn =
    match result with
    | Ok value -> fn value
    | Error error -> Error error
end

let ( let* ) = Result.and_then

let increment_ok input =
  let* value = Ok input in
  Ok (value + 1)
|ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "let_operator.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let increment_ok_type = export_scheme snapshot source_id "increment_ok" in
    if increment_ok_type = Some "'a. int -> (int, 'a) result" then
      Ok ()
    else
      Error ("unexpected increment_ok type: " ^ show_option increment_ok_type)

let test_external_identity_cast_token_shape_typechecks = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
type t

module Int = struct
  let hash value = value
end

external unsafe_cast : 'a -> 'b = "%identity"

let unsafe_to_value = fun (token: t) -> unsafe_cast token

let unsafe_to_int: t -> int = fun token -> unsafe_to_value token

let hash = fun token -> Int.hash (unsafe_to_int token)

let equal: ?eq:('value -> 'value -> bool) -> t -> t -> bool = fun ?eq:_ left right ->
  unsafe_to_int left = unsafe_to_int right

let make: 'value -> t = fun value -> unsafe_cast value
|ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "external_identity_cast_token.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let unsafe_to_value_type = export_scheme snapshot source_id "unsafe_to_value" in
    let unsafe_to_int_type = export_scheme snapshot source_id "unsafe_to_int" in
    let equal_type = export_scheme snapshot source_id "equal" in
    let make_type = export_scheme snapshot source_id "make" in
    if
      unsafe_to_value_type = Some "'a. t -> 'a"
      && unsafe_to_int_type = Some "t -> int"
      && equal_type = Some "'a. ?eq:('a -> 'a -> bool) -> t -> t -> bool"
      && make_type = Some "'a. 'a -> t"
    then
      Ok ()
    else
      Error ("unexpected token cast types: unsafe_to_value="
      ^ show_option unsafe_to_value_type
      ^ ", unsafe_to_int="
      ^ show_option unsafe_to_int_type
      ^ ", equal="
      ^ show_option equal_type
      ^ ", make="
      ^ show_option make_type)

let test_first_class_module_pack_and_unpack_typecheck = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
module type Intf = sig
  type t
  val run: t -> int
end

module Impl = struct
  type t = int
  let run value = value
end

let packed : (module Intf with type t = int) = (module Impl : Intf with type t = int)

let run_packed =
  let (module M : Intf with type t = int) = packed in
  M.run 1
|ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "first_class_module_pack_and_unpack.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let run_packed_type = export_scheme snapshot source_id "run_packed" in
    if run_packed_type = Some "int" then
      Ok ()
    else
      Error ("unexpected run_packed type: " ^ show_option run_packed_type)

let test_first_class_module_existential_event_shape_typechecks = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
module type Intf = sig
  type t
  val is_error: t -> bool
  val token: t -> int
end

type t =
  | E : (module Intf with type t = 'state) * 'state -> t

let make = fun implementation state -> E (implementation, state)

module Impl = struct
  type t = int
  let is_error _value = false
  let token value = value
end

let value = make (module Impl) 1

let token = fun (E ((module Event), state)) -> Event.token state

let is_error = fun (E ((module Event), state)) -> Event.is_error state
|ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "first_class_module_existential_event.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let value_type = export_scheme snapshot source_id "value" in
    let token_type = export_scheme snapshot source_id "token" in
    let is_error_type = export_scheme snapshot source_id "is_error" in
    if value_type = Some "t" && token_type = Some "t -> int" && is_error_type = Some "t -> bool" then
      Ok ()
    else
      Error ("unexpected first-class module existential types: value="
      ^ show_option value_type
      ^ ", token="
      ^ show_option token_type
      ^ ", is_error="
      ^ show_option is_error_type)

let test_prepare_snapshot_paired_module_preserves_first_class_module_value_signatures = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, event_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "event.mli")
    ~text:{ocaml|
      module type Intf = sig
        type t
        val is_error : t -> bool
        val token : t -> int
      end

      type t
      val make : (module Intf with type t = 'state) -> 'state -> t
      val token : t -> int
      val is_error : t -> bool
    |ocaml}
  in
  let (session, event_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "event.ml")
    ~text:{ocaml|
      module type Intf = sig
        type t
        val is_error : t -> bool
        val token : t -> int
      end

      type t =
        | E : (module Intf with type t = 'state) * 'state -> t

      let make = fun implementation state -> E (implementation, state)

      let token = fun (E ((module Event), state)) -> Event.token state

      let is_error = fun (E ((module Event), state)) -> Event.is_error state
    |ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ event_intf_source_id; event_impl_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let event_intf_diagnostics = diagnostic_strings snapshot event_intf_source_id in
      let event_impl_diagnostics = diagnostic_strings snapshot event_impl_source_id in
      if not (List.is_empty event_intf_diagnostics) then
        Error (String.concat "\n" event_intf_diagnostics)
      else if not (List.is_empty event_impl_diagnostics) then
        Error (String.concat "\n" event_impl_diagnostics)
      else
        let make_type = export_scheme snapshot event_impl_source_id "make" in
        let token_type = export_scheme snapshot event_impl_source_id "token" in
        let is_error_type = export_scheme snapshot event_impl_source_id "is_error" in
        if
          make_type = Some "'a. (module sig val is_error : 'a -> bool; val token : 'a -> int end) -> 'a -> t"
          && token_type = Some "t -> int"
          && is_error_type = Some "t -> bool"
        then
          Ok ()
        else
          Error (format
            Format.[
              str "unexpected event exports: make=";
              str (show_option make_type);
              str ", token=";
              str (show_option token_type);
              str ", is_error=";
              str (show_option is_error_type);
            ])

let test_prepare_snapshot_paired_module_allows_hidden_manifest_alias_with_same_variant_shape = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, adapter_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "adapter.mli")
    ~text:{ocaml|
      type error =
        | Boom
    |ocaml}
  in
  let (session, adapter_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "adapter.ml")
    ~text:{ocaml|
      type error =
        | Boom
    |ocaml}
  in
  let (session, async_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "async.mli")
    ~text:{ocaml|
      type error =
        | Boom

      val id : error -> error
    |ocaml}
  in
  let (session, async_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "async.ml")
    ~text:{ocaml|
      type error = Adapter.error =
        | Boom

      let id value = value
    |ocaml}
  in
  match prepare_snapshot_or_error
    session
    ~roots:[
      adapter_intf_source_id;
      adapter_impl_source_id;
      async_intf_source_id;
      async_impl_source_id
    ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let async_intf_diagnostics = diagnostic_strings snapshot async_intf_source_id in
      let async_impl_diagnostics = diagnostic_strings snapshot async_impl_source_id in
      if not (List.is_empty async_intf_diagnostics) then
        Error (String.concat "\n" async_intf_diagnostics)
      else if not (List.is_empty async_impl_diagnostics) then
        Error (String.concat "\n" async_impl_diagnostics)
      else
        let id_type = export_scheme snapshot async_impl_source_id "id" in
        if id_type = Some "error -> error" then
          Ok ()
        else
          Error ("unexpected async id type: " ^ show_option id_type)

let test_prepare_snapshot_include_prefers_local_unix_wrapper_over_loaded_unix = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, path_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "path.mli")
    ~text:{ocaml|
      type t = string
    |ocaml}
  in
  let (session, path_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "path.ml")
    ~text:{ocaml|
      type t = string
    |ocaml}
  in
  let (session, unix_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "unix.mli")
    ~text:{ocaml|
      type t
      val close : t -> unit
      val open_file : Path.t -> t
    |ocaml}
  in
  let (session, unix_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "unix.ml")
    ~text:{ocaml|
      type t = int
      let close _ = ()
      let open_file (_ : Path.t) = 0
    |ocaml}
  in
  let (session, file_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "file.mli")
    ~text:{ocaml|
      type t
      val close : t -> unit
      val open_file : Path.t -> t
    |ocaml}
  in
  let (session, file_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "file.ml")
    ~text:{ocaml|
      include Unix
    |ocaml}
  in
  match prepare_snapshot_or_error
    session
    ~roots:[
      path_intf_source_id;
      path_impl_source_id;
      unix_intf_source_id;
      unix_impl_source_id;
      file_intf_source_id;
      file_impl_source_id;
    ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let file_intf_diagnostics = diagnostic_strings snapshot file_intf_source_id in
      let file_impl_diagnostics = diagnostic_strings snapshot file_impl_source_id in
      if not (List.is_empty file_intf_diagnostics) then
        Error (String.concat "\n" file_intf_diagnostics)
      else if not (List.is_empty file_impl_diagnostics) then
        Error (String.concat "\n" file_impl_diagnostics)
      else
        let close_type = export_scheme snapshot file_impl_source_id "close" in
        let open_file_type = export_scheme snapshot file_impl_source_id "open_file" in
        if close_type = Some "t -> unit" && open_file_type = Some "Path.t -> t" then
          Ok ()
        else
          Error (format
            Format.[
              str "unexpected file exports: close=";
              str (show_option close_type);
              str ", open_file=";
              str (show_option open_file_type);
            ])

let test_prepare_snapshot_include_uses_interface_shaped_exports_from_errored_local_wrapper = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, path_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "path.mli")
    ~text:{ocaml|
      type t = string
    |ocaml}
  in
  let (session, path_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "path.ml")
    ~text:{ocaml|
      type t = string
    |ocaml}
  in
  let (session, unix_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "unix.mli")
    ~text:{ocaml|
      type t
      val close : t -> unit
      val open_file : Path.t -> t
    |ocaml}
  in
  let (session, unix_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "unix.ml")
    ~text:{ocaml|
      type t = int
      let close _ = ()
      let open_file (_ : Path.t) = 0

      let trigger_lowering_error =
        let module Local = struct
        end in
        ()
    |ocaml}
  in
  let (session, file_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "file.mli")
    ~text:{ocaml|
      type t
      val close : t -> unit
      val open_file : Path.t -> t
    |ocaml}
  in
  let (session, file_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "file.ml")
    ~text:{ocaml|
      include Unix
    |ocaml}
  in
  match prepare_snapshot_or_error
    session
    ~roots:[
      path_intf_source_id;
      path_impl_source_id;
      unix_intf_source_id;
      unix_impl_source_id;
      file_intf_source_id;
      file_impl_source_id;
    ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let unix_impl_diagnostics = diagnostic_strings snapshot unix_impl_source_id in
      let file_intf_diagnostics = diagnostic_strings snapshot file_intf_source_id in
      let file_impl_diagnostics = diagnostic_strings snapshot file_impl_source_id in
      if List.is_empty unix_impl_diagnostics then
        Error "expected unix.ml to surface a lowering diagnostic"
      else if not (List.is_empty file_intf_diagnostics) then
        Error (String.concat "\n" file_intf_diagnostics)
      else if not (List.is_empty file_impl_diagnostics) then
        Error (String.concat "\n" file_impl_diagnostics)
      else
        let open_file_type = export_scheme snapshot file_impl_source_id "open_file" in
        if open_file_type = Some "Path.t -> t" then
          Ok ()
        else
          Error ("unexpected file open_file type after errored include: " ^ show_option open_file_type)

let test_prepare_snapshot_errored_wrapper_preserves_persisted_named_type_ids = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "source.mli")
    ~text:{ocaml|
      type t = int
    |ocaml}
  in
  let (session, source_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "source.ml")
    ~text:{ocaml|
      type t = int
    |ocaml}
  in
  let (session, async_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "async.mli")
    ~text:{ocaml|
      module Source : sig
        type t = int
      end
    |ocaml}
  in
  let (session, async_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "async.ml")
    ~text:{ocaml|
      module Source = Source
    |ocaml}
  in
  let (session, unix_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "unix.mli")
    ~text:{ocaml|
      type t
      val to_source : t -> Async.Source.t
    |ocaml}
  in
  let (session, unix_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "unix.ml")
    ~text:{ocaml|
      type t = int
      let to_source _ = 0

      let trigger_lowering_error =
        let module Local = struct
        end in
        ()
    |ocaml}
  in
  let (session, file_intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "file.mli")
    ~text:{ocaml|
      type t
      val to_source : t -> Async.Source.t
    |ocaml}
  in
  let (session, file_impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "file.ml")
    ~text:{ocaml|
      include Unix
    |ocaml}
  in
  match prepare_snapshot_or_error
    session
    ~roots:[
      source_intf_source_id;
      source_impl_source_id;
      async_intf_source_id;
      async_impl_source_id;
      unix_intf_source_id;
      unix_impl_source_id;
      file_intf_source_id;
      file_impl_source_id;
    ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let file_intf_diagnostics = diagnostic_strings snapshot file_intf_source_id in
      let file_impl_diagnostics = diagnostic_strings snapshot file_impl_source_id in
      if not (List.is_empty file_intf_diagnostics) then
        Error (String.concat "\n" file_intf_diagnostics)
      else if not (List.is_empty file_impl_diagnostics) then
        Error (String.concat "\n" file_impl_diagnostics)
      else
        let to_source_type = export_scheme snapshot file_impl_source_id "to_source" in
        if to_source_type = Some "t -> Async.Source.t" then
          Ok ()
        else
          Error ("unexpected file to_source type after errored include: " ^ show_option to_source_type)

let test_prepare_snapshot_nested_internal_include_prefers_scoped_unix_wrapper = fun _ctx ->
  let module_name_suffix_aliases module_name =
    let segments = module_name
    |> String.split_on_char '.'
    |> List.filter (fun segment -> not (String.equal segment "")) in
    let rec loop aliases = function
      | [] -> List.rev aliases
      | _ :: rest as current -> loop (String.concat "." current :: aliases) rest
    in
    loop [] segments |> List.sort_uniq String.compare
  in
  let create_named_source session ~module_name ~filename ~text =
    let parse_result = Syn.parse ~filename:(Path.v filename) text in
    let cst = expect_cst ~filename parse_result in
    Session.create_source
      session
      ~kind:Source.File
      ~module_name
      ~implicit_opens:[]
      ~origin:(Source.Label filename)
      ~source_hash:(Source.hash ~implicit_opens:[] ~cst)
      ~parse_result
      ~cst
  in
  let register_local_aliases session source_id local_module_name = module_name_suffix_aliases local_module_name
  |> List.fold_left
    (fun session module_name -> Session.register_source_alias session source_id ~module_name)
    session in
  let session = Session.empty ~config:Config.default in
  let (session, path_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Path" ~filename:"path.mli"
    ~text:{ocaml|
        type t = string
      |ocaml}
  in
  let session = register_local_aliases session path_intf_source_id "Path" in
  let (session, path_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Path" ~filename:"path.ml"
    ~text:{ocaml|
        type t = string
      |ocaml}
  in
  let session = register_local_aliases session path_impl_source_id "Path" in
  let (session, unix_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Fs__File__Unix" ~filename:"file_unix.mli"
    ~text:{ocaml|
        type t
        val close : t -> unit
        val open_file : Path.t -> t
      |ocaml}
  in
  let session = register_local_aliases session unix_intf_source_id "Fs.File.Unix" in
  let (session, unix_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Fs__File__Unix" ~filename:"file_unix.ml"
    ~text:{ocaml|
        type t = int
        let close _ = ()
        let open_file (_ : Path.t) = 0
      |ocaml}
  in
  let session = register_local_aliases session unix_impl_source_id "Fs.File.Unix" in
  let (session, file_intf_source_id) = create_named_source session ~module_name:"Kernel_new__Fs__File" ~filename:"file.mli"
    ~text:{ocaml|
        type t
        val close : t -> unit
        val open_file : Path.t -> t
      |ocaml}
  in
  let session = register_local_aliases session file_intf_source_id "Fs.File" in
  let (session, file_impl_source_id) = create_named_source session ~module_name:"Kernel_new__Fs__File" ~filename:"file.ml"
    ~text:{ocaml|
        include Unix
      |ocaml}
  in
  let session = register_local_aliases session file_impl_source_id "Fs.File" in
  match prepare_snapshot_or_error
    session
    ~roots:[
      path_intf_source_id;
      path_impl_source_id;
      unix_intf_source_id;
      unix_impl_source_id;
      file_intf_source_id;
      file_impl_source_id;
    ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let file_intf_diagnostics = diagnostic_strings snapshot file_intf_source_id in
      let file_impl_diagnostics = diagnostic_strings snapshot file_impl_source_id in
      if not (List.is_empty file_intf_diagnostics) then
        Error (String.concat "\n" file_intf_diagnostics)
      else if not (List.is_empty file_impl_diagnostics) then
        Error (String.concat "\n" file_impl_diagnostics)
      else
        let close_type = export_scheme snapshot file_impl_source_id "close" in
        let open_file_type = export_scheme snapshot file_impl_source_id "open_file" in
        if close_type = Some "t -> unit" && open_file_type = Some "Path.t -> t" then
          Ok ()
        else
          Error (format
            Format.[
              str "unexpected nested file exports: close=";
              str (show_option close_type);
              str ", open_file=";
              str (show_option open_file_type);
            ])

let test_gadt_constructor_existentials_typecheck = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
type 'a option =
  | None
  | Some of 'a

type ('a, 'b) eq =
  | Equal : ('a, 'a) eq

type 'a tag = int

let type_equal : type a b. a tag -> b tag -> (a, b) eq option = fun _ _ -> None

type request =
  | Get : {
      fn: 'state -> 'reply;
      state_tag: 'state tag;
      reply_tag: 'reply tag;
    } -> request

let run: type state. state tag -> state -> request -> unit = fun state_tag state ->
  function
  | Get { fn; state_tag=other_tag; reply_tag=_ } -> (
      match type_equal state_tag other_tag with
      | Some Equal ->
          let _ = fn state in
          ()
      | None -> ()
    )
|ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "gadt_constructor_existentials.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if List.is_empty diagnostics then
    let run_type = export_scheme snapshot source_id "run" in
    if run_type = Some "'a. 'a tag -> 'a -> request -> unit" then
      Ok ()
    else
      Error ("unexpected run type: " ^ show_option run_type)
  else
    Error (String.concat "\n" diagnostics)

let test_gadt_tuple_constructor_existentials_typecheck = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
type 'a option =
  | None
  | Some of 'a

type ('a, 'b) eq =
  | Equal : ('a, 'a) eq

type 'a tag = int

let type_equal : type a b. a tag -> b tag -> (a, b) eq option = fun _ _ -> None

type response =
  | Reply : 'reply * 'reply tag -> response

let handle: type reply. reply tag -> response -> reply option = fun reply_tag ->
  function
  | Reply (result, rr) -> (
      match type_equal reply_tag rr with
      | Some Equal -> Some result
      | None -> None
    )
|ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "gadt_tuple_constructor_existentials.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if List.is_empty diagnostics then
    let handle_type = export_scheme snapshot source_id "handle" in
    if handle_type = Some "'a. 'a tag -> response -> 'a option" then
      Ok ()
    else
      Error ("unexpected handle type: " ^ show_option handle_type)
  else
    Error (String.concat "\n" diagnostics)

let test_selector_receive_with_existential_constructors_typecheck = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
type 'a option =
  | None
  | Some of 'a

type ('a, 'b) eq =
  | Equal : ('a, 'a) eq

module Type = struct
  type ('a, 'b) eq = ('a, 'b) eq =
    | Equal : ('a, 'a) eq
end

module Ref = struct
  type 'a t = int

  let make () = 0

  let equal a b = a = b

  let type_equal : type a b. a t -> b t -> (a, b) Type.eq option = fun _ _ -> None
end

type pid = unit

type message = ..

type 'msg selector = message -> [
  | `select of 'msg
  | `skip
]

let receive : selector:'value selector -> unit -> 'value = fun ~selector:_ () ->
  let rec loop () = loop () in
  loop ()

let send : pid -> message -> unit = fun _ _ -> ()

let self () = ()

type 'state agent = {
  pid: pid;
  state_ref: 'state Ref.t;
}

type request =
  | Get : {
      reply_to: pid;
      fn: 'state -> 'reply;
      state_ref: 'state Ref.t;
      reply_ref: 'reply Ref.t;
    } -> request

type response =
  | GetReply : 'reply * 'reply Ref.t -> response

type message +=
  | Request of request
  | Response of response

let loop: type state. state Ref.t -> state -> unit = fun state_ref state ->
  let selector msg =
    match msg with
    | Request req -> `select req
    | _ -> `skip
  in
  match receive ~selector () with
  | Get { reply_to; fn; state_ref=sr; reply_ref } -> (
      match Ref.type_equal state_ref sr with
      | Some Type.Equal ->
          let result = fn state in
          send reply_to (Response (GetReply (result, reply_ref)))
      | None -> ()
    )

let get: type state reply. state agent -> (state -> reply) -> reply = fun agent fn ->
  let reply_ref: reply Ref.t = Ref.make () in
  send agent.pid (Request (Get { reply_to = self (); fn; state_ref = agent.state_ref; reply_ref }));
  let selector msg =
    match msg with
    | Response res -> `select res
    | _ -> `skip
  in
  match receive ~selector () with
  | GetReply (result, rr) when Ref.equal reply_ref rr -> (
      match Ref.type_equal reply_ref rr with
      | Some Type.Equal -> result
      | None ->
          let rec loop () = loop () in
          loop ()
    )
  | _ ->
      let rec loop () = loop () in
      loop ()
|ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "selector_receive_existentials.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if List.is_empty diagnostics then
    let get_type = export_scheme snapshot source_id "get" in
    if get_type = Some "'a 'b. 'a agent -> ('a -> 'b) -> 'b" then
      Ok ()
    else
      Error ("unexpected get type: " ^ show_option get_type)
  else
    Error (String.concat "\n" diagnostics)

let test_extensible_selector_agent_shape_typechecks = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
type 'a option =
  | None
  | Some of 'a

type ('a, 'b) eq =
  | Equal : ('a, 'a) eq

module Type = struct
  type ('a, 'b) eq = ('a, 'b) eq =
    | Equal : ('a, 'a) eq
end

type ('a, 'e) result =
  | Ok of 'a
  | Error of 'e

module Ref = struct
  type 'a t = int

  let make () = 0

  let equal a b = a = b

  let type_equal : type a b. a t -> b t -> (a, b) Type.eq option = fun _ _ -> None
end

module Pid = struct
  type t = unit
end

module Message = struct
  type t = ..
end

type 'msg selector = Message.t -> [
  | `select of 'msg
  | `skip
]

let receive : selector:'value selector -> unit -> 'value = fun ~selector:_ () ->
  let rec loop () = loop () in
  loop ()

let send : Pid.t -> Message.t -> unit = fun _ _ -> ()

let self () = ()

let spawn : (unit -> (unit, string) result) -> Pid.t = fun _ -> ()

let spawn_link : (unit -> (unit, string) result) -> Pid.t = fun _ -> ()

let panic : string -> 'a = fun _ ->
  let rec loop () = loop () in
  loop ()

type 'state t = {
  pid: Pid.t;
  state_ref: 'state Ref.t;
}

type agent_request =
  | Get : {
      reply_to: Pid.t;
      fn: 'state -> 'reply;
      state_ref: 'state Ref.t;
      reply_ref: 'reply Ref.t;
    } -> agent_request
  | GetAndUpdate : {
      reply_to: Pid.t;
      fn: 'state -> 'reply * 'state;
      state_ref: 'state Ref.t;
      reply_ref: 'reply Ref.t;
    } -> agent_request
  | Update : {
      reply_to: Pid.t;
      fn: 'state -> 'state;
      state_ref: 'state Ref.t;
    } -> agent_request
  | Cast : {
      fn: 'state -> 'state;
      state_ref: 'state Ref.t;
    } -> agent_request
  | Stop : {
      reply_to: Pid.t;
    } -> agent_request

type agent_response =
  | GetReply : 'reply * 'reply Ref.t -> agent_response
  | GetAndUpdateReply : 'reply * 'reply Ref.t -> agent_response
  | UpdateReply
  | StopReply

type Message.t +=
  | AgentRequest of agent_request
  | AgentResponse of agent_response

let rec loop: type state. state Ref.t -> state -> (unit, string) result = fun state_ref state ->
  let selector msg =
    match msg with
    | AgentRequest req -> `select req
    | _ -> `skip
  in
  match receive ~selector () with
  | Get { reply_to; fn; state_ref=sr; reply_ref } -> (
      match Ref.type_equal state_ref sr with
      | Some Type.Equal ->
          let result = fn state in
          send reply_to (AgentResponse (GetReply (result, reply_ref)));
          loop state_ref state
      | None -> loop state_ref state
    )
  | Update { reply_to; fn; state_ref=sr } -> (
      match Ref.type_equal state_ref sr with
      | Some Type.Equal ->
          let new_state = fn state in
          send reply_to (AgentResponse UpdateReply);
          loop state_ref new_state
      | None -> loop state_ref state
    )
  | GetAndUpdate { reply_to; fn; state_ref=sr; reply_ref } -> (
      match Ref.type_equal state_ref sr with
      | Some Type.Equal ->
          let (result, new_state) = fn state in
          send reply_to (AgentResponse (GetAndUpdateReply (result, reply_ref)));
          loop state_ref new_state
      | None -> loop state_ref state
    )
  | Cast { fn; state_ref=sr } -> (
      match Ref.type_equal state_ref sr with
      | Some Type.Equal ->
          let new_state = fn state in
          loop state_ref new_state
      | None -> loop state_ref state
    )
  | Stop { reply_to } ->
      send reply_to (AgentResponse StopReply);
      Ok ()

let start: type state. (unit -> state) -> state t = fun init ->
  let state_ref: state Ref.t = Ref.make () in
  let pid =
    spawn (fun () -> loop state_ref (init ()))
  in
  { pid; state_ref }

let start_link: type state. (unit -> state) -> state t = fun init ->
  let state_ref: state Ref.t = Ref.make () in
  let pid =
    spawn_link (fun () -> loop state_ref (init ()))
  in
  { pid; state_ref }

let get: type state reply. state t -> (state -> reply) -> reply = fun agent fn ->
  let reply_ref: reply Ref.t = Ref.make () in
  send
    agent.pid
    (AgentRequest (Get { reply_to = self (); fn; state_ref = agent.state_ref; reply_ref }));
  let selector msg =
    match msg with
    | AgentResponse res -> `select res
    | _ -> `skip
  in
  match receive ~selector () with
  | GetReply (result, rr) when Ref.equal reply_ref rr -> (
      match Ref.type_equal reply_ref rr with
      | Some Type.Equal -> result
      | None -> panic "impossible: reply ref mismatch"
    )
  | _ -> panic "unexpected agent response"

let update: type state. state t -> (state -> state) -> unit = fun agent fn ->
  send agent.pid (AgentRequest (Update { reply_to = self (); fn; state_ref = agent.state_ref }));
  let selector msg =
    match msg with
    | AgentResponse res -> `select res
    | _ -> `skip
  in
  match receive ~selector () with
  | UpdateReply -> ()
  | _ -> panic "unexpected agent response"

let get_and_update: type state reply. state t -> (state -> reply * state) -> reply = fun agent fn ->
  let reply_ref: reply Ref.t = Ref.make () in
  send
    agent.pid
    (
      AgentRequest (GetAndUpdate {
        reply_to = self ();
        fn;
        state_ref = agent.state_ref;
        reply_ref
      })
    );
  let selector msg =
    match msg with
    | AgentResponse res -> `select res
    | _ -> `skip
  in
  match receive ~selector () with
  | GetAndUpdateReply (result, rr) when Ref.equal reply_ref rr -> (
      match Ref.type_equal reply_ref rr with
      | Some Type.Equal -> result
      | None -> panic "impossible: reply ref mismatch"
    )
  | _ -> panic "unexpected agent response"

let cast: type state. state t -> (state -> state) -> unit = fun agent fn ->
  send agent.pid (AgentRequest (Cast { fn; state_ref = agent.state_ref }))

let stop: type state. state t -> unit = fun agent ->
  send agent.pid (AgentRequest (Stop { reply_to = self () }));
  let selector msg =
    match msg with
    | AgentResponse res -> `select res
    | _ -> `skip
  in
  match receive ~selector () with
  | StopReply -> ()
  | _ -> panic "unexpected agent response"
|ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "extensible_selector_agent_shape.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if List.is_empty diagnostics then
    let get_type = export_scheme snapshot source_id "get" in
    let get_and_update_type = export_scheme snapshot source_id "get_and_update" in
    let update_type = export_scheme snapshot source_id "update" in
    let cast_type = export_scheme snapshot source_id "cast" in
    let stop_type = export_scheme snapshot source_id "stop" in
    if
      get_type = Some "'a 'b. 'a t -> ('a -> 'b) -> 'b"
      && get_and_update_type = Some "'a 'b. 'a t -> ('a -> 'b * 'a) -> 'b"
      && update_type = Some "'a. 'a t -> ('a -> 'a) -> unit"
      && cast_type = Some "'a. 'a t -> ('a -> 'a) -> unit"
      && stop_type = Some "'a. 'a t -> unit"
    then
      Ok ()
    else
      Error ("unexpected exported types: get="
      ^ show_option get_type
      ^ ", get_and_update="
      ^ show_option get_and_update_type
      ^ ", update="
      ^ show_option update_type
      ^ ", cast="
      ^ show_option cast_type
      ^ ", stop="
      ^ show_option stop_type)
  else
    Error (String.concat "\n" diagnostics)

let test_loaded_module_selector_agent_shape_typechecks = fun _ctx ->
  let config = Config.default |> Config.with_capture_traces ~capture_traces:true in
  let session = Session.empty ~config in
  let (session, _type_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "type.ml")
    ~text:{ocaml|
type ('a, 'b) eq =
  | Equal : ('a, 'a) eq
|ocaml}
  in
  let (session, _ref_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "ref.ml")
    ~text:{ocaml|
type 'a t = int

let make () = 0

let equal a b = a = b

let type_equal : type a b. a t -> b t -> (a, b) Type.eq option = fun _ _ -> None
|ocaml}
  in
  let (session, _pid_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "pid.ml")
    ~text:{ocaml|
type t = unit
|ocaml}
  in
  let (session, _message_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "message.ml")
    ~text:{ocaml|
type t = ..
|ocaml}
  in
  let (session, _global_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "global.ml")
    ~text:{ocaml|
type ('a, 'e) result =
  | Ok of 'a
  | Error of 'e

type 'msg selector = Message.t -> [
  | `select of 'msg
  | `skip
]

let receive : selector:'value selector -> unit -> 'value = fun ~selector:_ () ->
  let rec loop () = loop () in
  loop ()

let send : Pid.t -> Message.t -> unit = fun _ _ -> ()

let self () = ()

let spawn : (unit -> (unit, string) result) -> Pid.t = fun _ -> ()

let spawn_link : (unit -> (unit, string) result) -> Pid.t = fun _ -> ()

let panic : string -> 'a = fun _ ->
  let rec loop () = loop () in
  loop ()
|ocaml}
  in
  let (session, agent_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "agent.ml")
    ~text:{ocaml|
open Global

type 'state t = {
  pid: Pid.t;
  state_ref: 'state Ref.t;
}

type agent_request =
  | Get : {
      reply_to: Pid.t;
      fn: 'state -> 'reply;
      state_ref: 'state Ref.t;
      reply_ref: 'reply Ref.t;
    } -> agent_request
  | GetAndUpdate : {
      reply_to: Pid.t;
      fn: 'state -> 'reply * 'state;
      state_ref: 'state Ref.t;
      reply_ref: 'reply Ref.t;
    } -> agent_request
  | Update : {
      reply_to: Pid.t;
      fn: 'state -> 'state;
      state_ref: 'state Ref.t;
    } -> agent_request
  | Cast : {
      fn: 'state -> 'state;
      state_ref: 'state Ref.t;
    } -> agent_request
  | Stop : {
      reply_to: Pid.t;
    } -> agent_request

type agent_response =
  | GetReply : 'reply * 'reply Ref.t -> agent_response
  | GetAndUpdateReply : 'reply * 'reply Ref.t -> agent_response
  | UpdateReply
  | StopReply

type Message.t +=
  | AgentRequest of agent_request
  | AgentResponse of agent_response

let rec loop: type state. state Ref.t -> state -> (unit, string) Global.result = fun state_ref state ->
  let selector msg =
    match msg with
    | AgentRequest req -> `select req
    | _ -> `skip
  in
  match receive ~selector () with
  | Get { reply_to; fn; state_ref=sr; reply_ref } -> (
      match Ref.type_equal state_ref sr with
      | Some Type.Equal ->
          let result = fn state in
          send reply_to (AgentResponse (GetReply (result, reply_ref)));
          loop state_ref state
      | None -> loop state_ref state
    )
  | Update { reply_to; fn; state_ref=sr } -> (
      match Ref.type_equal state_ref sr with
      | Some Type.Equal ->
          let new_state = fn state in
          send reply_to (AgentResponse UpdateReply);
          loop state_ref new_state
      | None -> loop state_ref state
    )
  | GetAndUpdate { reply_to; fn; state_ref=sr; reply_ref } -> (
      match Ref.type_equal state_ref sr with
      | Some Type.Equal ->
          let (result, new_state) = fn state in
          send reply_to (AgentResponse (GetAndUpdateReply (result, reply_ref)));
          loop state_ref new_state
      | None -> loop state_ref state
    )
  | Cast { fn; state_ref=sr } -> (
      match Ref.type_equal state_ref sr with
      | Some Type.Equal ->
          let new_state = fn state in
          loop state_ref new_state
      | None -> loop state_ref state
    )
  | Stop { reply_to } ->
      send reply_to (AgentResponse StopReply);
      Ok ()

let start: type state. (unit -> state) -> state t = fun init ->
  let state_ref: state Ref.t = Ref.make () in
  let pid =
    spawn (fun () -> loop state_ref (init ()))
  in
  { pid; state_ref }

let start_link: type state. (unit -> state) -> state t = fun init ->
  let state_ref: state Ref.t = Ref.make () in
  let pid =
    spawn_link (fun () -> loop state_ref (init ()))
  in
  { pid; state_ref }

let get: type state reply. state t -> (state -> reply) -> reply = fun agent fn ->
  let reply_ref: reply Ref.t = Ref.make () in
  send
    agent.pid
    (AgentRequest (Get { reply_to = self (); fn; state_ref = agent.state_ref; reply_ref }));
  let selector msg =
    match msg with
    | AgentResponse res -> `select res
    | _ -> `skip
  in
  match receive ~selector () with
  | GetReply (result, rr) when Ref.equal reply_ref rr -> (
      match Ref.type_equal reply_ref rr with
      | Some Type.Equal -> result
      | None -> panic "impossible: reply ref mismatch"
    )
  | _ -> panic "unexpected agent response"

let update: type state. state t -> (state -> state) -> unit = fun agent fn ->
  send agent.pid (AgentRequest (Update { reply_to = self (); fn; state_ref = agent.state_ref }));
  let selector msg =
    match msg with
    | AgentResponse res -> `select res
    | _ -> `skip
  in
  match receive ~selector () with
  | UpdateReply -> ()
  | _ -> panic "unexpected agent response"

let get_and_update: type state reply. state t -> (state -> reply * state) -> reply = fun agent fn ->
  let reply_ref: reply Ref.t = Ref.make () in
  send
    agent.pid
    (
      AgentRequest (GetAndUpdate {
        reply_to = self ();
        fn;
        state_ref = agent.state_ref;
        reply_ref
      })
    );
  let selector msg =
    match msg with
    | AgentResponse res -> `select res
    | _ -> `skip
  in
  match receive ~selector () with
  | GetAndUpdateReply (result, rr) when Ref.equal reply_ref rr -> (
      match Ref.type_equal reply_ref rr with
      | Some Type.Equal -> result
      | None -> panic "impossible: reply ref mismatch"
    )
  | _ -> panic "unexpected agent response"

let cast: type state. state t -> (state -> state) -> unit = fun agent fn ->
  send agent.pid (AgentRequest (Cast { fn; state_ref = agent.state_ref }))

let stop: type state. state t -> unit = fun agent ->
  send agent.pid (AgentRequest (Stop { reply_to = self () }));
  let selector msg =
    match msg with
    | AgentResponse res -> `select res
    | _ -> `skip
  in
  match receive ~selector () with
  | StopReply -> ()
  | _ -> panic "unexpected agent response"
|ocaml}
  in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot agent_source_id in
  if List.is_empty diagnostics then
    let get_type = export_scheme snapshot agent_source_id "get" in
    let get_and_update_type = export_scheme snapshot agent_source_id "get_and_update" in
    let update_type = export_scheme snapshot agent_source_id "update" in
    let cast_type = export_scheme snapshot agent_source_id "cast" in
    let stop_type = export_scheme snapshot agent_source_id "stop" in
    if
      get_type = Some "'a 'b. 'a t -> ('a -> 'b) -> 'b"
      && get_and_update_type = Some "'a 'b. 'a t -> ('a -> 'b * 'a) -> 'b"
      && update_type = Some "'a. 'a t -> ('a -> 'a) -> unit"
      && cast_type = Some "'a. 'a t -> ('a -> 'a) -> unit"
      && stop_type = Some "'a. 'a t -> unit"
    then
      Ok ()
    else
      Error ("unexpected exported types: get="
      ^ show_option get_type
      ^ ", get_and_update="
      ^ show_option get_and_update_type
      ^ ", update="
      ^ show_option update_type
      ^ ", cast="
      ^ show_option cast_type
      ^ ", stop="
      ^ show_option stop_type)
  else
    Error (String.concat "\n" (diagnostics @ trace_debug snapshot agent_source_id))

let test_snapshot_exposes_loaded_module_agent_support_exports = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _type_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "type.ml")
    ~text:{ocaml|
type ('a, 'b) eq =
  | Equal : ('a, 'a) eq
|ocaml}
  in
  let (session, ref_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "ref.ml")
    ~text:{ocaml|
type 'a t = int

let make () = 0

let equal a b = a = b

let type_equal : type a b. a t -> b t -> (a, b) Type.eq option = fun _ _ -> None
|ocaml}
  in
  let (session, _pid_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "pid.ml")
    ~text:{ocaml|
type t = unit
|ocaml}
  in
  let (session, _message_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "message.ml")
    ~text:{ocaml|
type t = ..
|ocaml}
  in
  let (session, global_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "global.ml")
    ~text:{ocaml|
type ('a, 'e) result =
  | Ok of 'a
  | Error of 'e

type 'msg selector = Message.t -> [
  | `select of 'msg
  | `skip
]

let receive : selector:'value selector -> unit -> 'value = fun ~selector:_ () ->
  let rec loop () = loop () in
  loop ()

let send : Pid.t -> Message.t -> unit = fun _ _ -> ()

let self () = ()

let spawn : (unit -> (unit, string) result) -> Pid.t = fun _ -> ()

let spawn_link : (unit -> (unit, string) result) -> Pid.t = fun _ -> ()

let panic : string -> 'a = fun _ ->
  let rec loop () = loop () in
  loop ()
|ocaml}
  in
  let snapshot = Session.snapshot session in
  let ref_exports = export_names (Query.export_of snapshot ref_source_id) in
  let global_exports = export_names (Query.export_of snapshot global_source_id) in
  let ref_type_equal = export_scheme snapshot ref_source_id "type_equal" in
  let global_spawn = export_scheme snapshot global_source_id "spawn" in
  if
    ref_exports = [ "equal"; "make"; "type_equal" ]
    && global_exports = [ "panic"; "receive"; "self"; "send"; "spawn"; "spawn_link" ]
    && ref_type_equal = Some "'a 'b. int -> int -> ('a, 'b) Type.eq option"
    && global_spawn = Some "(unit -> (unit, string) result) -> Pid.t"
  then
    Ok ()
  else
    Error ("unexpected module exports: ref="
    ^ String.concat ", " ref_exports
    ^ " global="
    ^ String.concat ", " global_exports
    ^ " ref.type_equal="
    ^ show_option ref_type_equal
    ^ " global.spawn="
    ^ show_option global_spawn)

let test_deps_collect_loaded_module_agent_dependencies = fun _ctx ->
  let text = {ocaml|
open Global

type 'state t = {
  pid: Pid.t;
  state_ref: 'state Ref.t;
}

let start: type state. (unit -> state) -> state t = fun init ->
  let state_ref: state Ref.t = Ref.make () in
  let pid =
    spawn (fun () -> Ok ())
  in
  send pid (Obj.magic ());
  ignore (receive ~selector:(fun _ -> `skip) ());
  ignore (self ());
  ignore (panic "boom");
  { pid; state_ref }
|ocaml}
  in
  let parse_result = Syn.parse ~filename:(Path.v "agent.ml") text in
  match Syn.Deps.of_parse_result parse_result with
  | Error (Syn.Deps.Parse_diagnostics diagnostics) ->
      Error (format
        Format.[
          str "unexpected deps diagnostics: ";
          str (String.concat "; " (List.map Syn.Diagnostic.to_string diagnostics));
        ])
  | Error (Syn.Deps.Cst_builder_error error) ->
      Error (format Format.[ str "unexpected deps CST error: "; str error.message; ])
  | Ok deps ->
      let modules = Syn.Deps.modules deps |> List.sort String.compare in
      if modules = [ "Global"; "Obj"; "Pid"; "Ref" ] then
        Ok ()
      else
        Error (format Format.[ str "unexpected deps: "; str (String.concat ", " modules); ])

let test_selector_alias_chain_across_loaded_modules_typechecks = fun _ctx ->
  let config = Config.default |> Config.with_capture_traces ~capture_traces:true in
  let session = Session.empty ~config in
  let (session, _actors_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "actors.ml")
    ~text:{ocaml|
type ('a, 'e) result =
  | Ok of 'a
  | Error of 'e

module Pid = struct
  type t = unit
end

module Message = struct
  type t = ..
end

type 'msg selector = Message.t -> [
  | `select of 'msg
  | `skip
]

let self () = ()

let send : Pid.t -> Message.t -> unit = fun _ _ -> ()

let receive : selector:'value selector -> ?timeout:float -> unit -> 'value = fun ~selector:_ ?timeout:_ () ->
  let rec loop () = loop () in
  loop ()

let spawn : (unit -> (unit, string) result) -> Pid.t = fun _ -> ()

let spawn_link : (unit -> (unit, string) result) -> Pid.t = fun _ -> ()
|ocaml}
  in
  let (session, _type_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "type.ml")
    ~text:{ocaml|
type ('a, 'b) eq =
  | Equal : ('a, 'a) eq
|ocaml}
  in
  let (session, _pid_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "pid.ml")
    ~text:{ocaml|
include Actors.Pid
|ocaml}
  in
  let (session, _message_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "message.ml")
    ~text:{ocaml|
include Actors.Message
|ocaml}
  in
  let (session, _ref_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "ref.ml")
    ~text:{ocaml|
type 'a t = int

let make () = 0

let equal a b = a = b

let type_equal : type a b. a t -> b t -> (a, b) Type.eq option = fun _ _ -> None
|ocaml}
  in
  let (session, _global_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "global.ml")
    ~text:{ocaml|
type ('a, 'e) result = ('a, 'e) Actors.result =
  | Ok of 'a
  | Error of 'e

type 'msg selector = 'msg Actors.selector

let self = Actors.self

let send = Actors.send

let receive = fun ~selector ?timeout () ->
  Actors.receive ~selector ?timeout ()

let spawn = Actors.spawn

let spawn_link = Actors.spawn_link

let panic : string -> 'a = fun _ ->
  let rec loop () = loop () in
  loop ()
|ocaml}
  in
  let (session, agent_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "agent.ml")
    ~text:{ocaml|
open Global

type 'state t = {
  pid: Pid.t;
  state_ref: 'state Ref.t;
}

type agent_request =
  | Get : {
      reply_to: Pid.t;
      fn: 'state -> 'reply;
      state_ref: 'state Ref.t;
      reply_ref: 'reply Ref.t;
    } -> agent_request

type agent_response =
  | GetReply : 'reply * 'reply Ref.t -> agent_response

type Message.t +=
  | AgentRequest of agent_request
  | AgentResponse of agent_response

let get: type state reply. state t -> (state -> reply) -> reply = fun agent fn ->
  let reply_ref: reply Ref.t = Ref.make () in
  send
    agent.pid
    (AgentRequest (Get { reply_to = self (); fn; state_ref = agent.state_ref; reply_ref }));
  let selector msg =
    match msg with
    | AgentResponse res -> `select res
    | _ -> `skip
  in
  match receive ~selector () with
  | GetReply (result, rr) when Ref.equal reply_ref rr -> (
      match Ref.type_equal reply_ref rr with
      | Some Type.Equal -> result
      | None -> panic "impossible: reply ref mismatch"
    )
  | _ -> panic "unexpected agent response"
|ocaml}
  in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot agent_source_id in
  if List.is_empty diagnostics then
    let get_type = export_scheme snapshot agent_source_id "get" in
    if get_type = Some "'a 'b. 'a t -> ('a -> 'b) -> 'b" then
      Ok ()
    else
      Error (format Format.[ str "unexpected get type: "; str (show_option get_type); ])
  else
    Error (String.concat "\n" (diagnostics @ trace_debug snapshot agent_source_id))

let test_included_extensible_types_preserve_constructor_identity_across_siblings = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _actors_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "actors.ml")
    ~text:{ocaml|
module Message = struct
  type t = ..
end
|ocaml}
  in
  let (session, _message_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "message.ml")
    ~text:{ocaml|
include Actors.Message
|ocaml}
  in
  let (session, _global_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "global.ml")
    ~text:{ocaml|
let send : Actors.Message.t -> unit = fun _ -> ()
|ocaml}
  in
  let (session, source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "worker.ml")
    ~text:{ocaml|
type Message.t += Ping

let ping = Ping

let run () =
  Global.send Ping
|ocaml}
  in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if List.is_empty diagnostics then
    match export_scheme snapshot source_id "run" with
    | Some "unit -> unit" -> Ok ()
    | Some actual -> Error (format Format.[ str "unexpected exported run type: "; str actual; ])
    | None -> Error "missing exported run binding"
  else
    Error (String.concat "\n" diagnostics)

let test_loaded_module_ref_shape_typechecks = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _type_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "type.ml")
    ~text:{ocaml|
type ('a, 'b) eq =
  | Equal : ('a, 'a) eq
|ocaml}
  in
  let (session, _kernel_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "kernel.ml")
    ~text:{ocaml|
module Type = Type

let dangerous_unsafe_cast : 'a -> 'b = fun _ ->
  let rec loop () = loop () in
  loop ()
|ocaml}
  in
  let (session, _sync_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "sync.ml")
    ~text:{ocaml|
module Atomic = struct
  type 'a t = 'a

  let make value = value

  let get value = value

  let compare_and_set _ _ _ = false
end
|ocaml}
  in
  let (session, _global_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "global.ml")
    ~text:{ocaml|
let id x = x
|ocaml}
  in
  let (session, _int64_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "int64.ml")
    ~text:{ocaml|
type t = int64

let succ (value: t) : t = value

let equal (_: t) (_: t) = false

let compare (_: t) (_: t) = 0

let hash (_: t) = 0
|ocaml}
  in
  let (session, ref_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "ref.ml")
    ~text:{ocaml|
type 'a option =
  | None
  | Some of 'a

open Global
open Sync

type 'a t =
  Ref of int64 [@@unboxed]

let __current__ = Atomic.make 0L

let rec make = fun () ->
  let last = Atomic.get __current__ in
  let current = last |> Int64.succ in
  if Atomic.compare_and_set __current__ last current then
    Ref last
  else
    make ()

let equal = fun (Ref a) (Ref b) ->
  Int64.equal a b

let type_equal: type a b. a t -> b t -> (a, b) Type.eq option = fun a b ->
  match (a, b) with
  | Ref a', Ref b' when Int64.equal a' b' -> Some (Kernel.dangerous_unsafe_cast Type.Equal)
  | _ -> None

let cast: type a b. (a, b) Type.eq -> a -> b = fun (Type.Equal) a -> a

let cast: type a b. a t -> b t -> a -> b option = fun a b value ->
  match type_equal a b with
  | Some witness -> Some (cast witness value)
  | None -> None

let is_newer = fun (Ref a) (Ref b) -> Int64.compare a b = 1

let hash = fun (Ref a) -> Int64.hash a
|ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ ref_source_id ] with
  | Error error -> Error error
  | Ok snapshot ->
      let diagnostics = diagnostic_strings snapshot ref_source_id in
      if List.is_empty diagnostics then
        let make_type = export_scheme snapshot ref_source_id "make" in
        let type_equal_type = export_scheme snapshot ref_source_id "type_equal" in
        if
          make_type = Some "'a. unit -> 'a t" && type_equal_type = Some "'a 'b. 'a t -> 'b t -> ('a, 'b) Type.eq option"
        then
          Ok ()
        else
          Error (format
            Format.[
              str "unexpected exported types: make=";
              str (show_option make_type);
              str ", type_equal=";
              str (show_option type_equal_type);
            ])
      else
        Error (String.concat "\n" diagnostics)

let test_spawn_alias_chain_across_loaded_modules_typechecks = fun _ctx ->
  let config = Config.default |> Config.with_capture_traces ~capture_traces:true in
  let session = Session.empty ~config in
  let (session, _kernel_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "kernel.ml")
    ~text:{ocaml|
type ('a, 'e) result =
  | Ok of 'a
  | Error of 'e
|ocaml}
  in
  let (session, _actors_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "actors.ml")
    ~text:{ocaml|
module Pid = struct
  type t = unit
end

module Process = struct
  type exit_reason = exn
end

let spawn : (unit -> (unit, Process.exit_reason) Kernel.result) -> Pid.t = fun _ -> ()

let spawn_link : (unit -> (unit, Process.exit_reason) Kernel.result) -> Pid.t = fun _ -> ()
|ocaml}
  in
  let (session, _pid_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "pid.ml")
    ~text:{ocaml|
include Actors.Pid
|ocaml}
  in
  let (session, _global_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "global.ml")
    ~text:{ocaml|
type ('a, 'e) result = ('a, 'e) Kernel.result =
  | Ok of 'a
  | Error of 'e

let spawn = Actors.spawn

let spawn_link = Actors.spawn_link
|ocaml}
  in
  let (session, source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "worker.ml")
    ~text:{ocaml|
open Global

type t = {
  pid: Pid.t;
}

let loop () : (unit, exn) result =
  Ok ()

let start () =
  let pid = spawn loop in
  { pid }

let start_link () =
  let pid = spawn_link loop in
  { pid }
|ocaml}
  in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if List.is_empty diagnostics then
    let start_type = export_scheme snapshot source_id "start" in
    let start_link_type = export_scheme snapshot source_id "start_link" in
    if start_type = Some "unit -> t" && start_link_type = Some "unit -> t" then
      Ok ()
    else
      Error (format
        Format.[
          str "unexpected exported types: start=";
          str (show_option start_type);
          str ", start_link=";
          str (show_option start_link_type);
        ])
  else
    Error (String.concat "\n" (diagnostics @ trace_debug snapshot source_id))

let test_function_body_annotation_alias_chain_exports_canonical_result = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _kernel_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "kernel.ml")
    ~text:{ocaml|
type ('a, 'e) result =
  | Ok of 'a
  | Error of 'e
|ocaml}
  in
  let (session, _global_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "global.ml")
    ~text:{ocaml|
type ('a, 'e) result = ('a, 'e) Kernel.result =
  | Ok of 'a
  | Error of 'e
|ocaml}
  in
  let (session, source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "worker.ml")
    ~text:{ocaml|
open Global

let loop () : (unit, exn) result =
  Ok ()
|ocaml}
  in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if List.is_empty diagnostics then
    match export_scheme snapshot source_id "loop" with
    | Some "unit -> (unit, exn) Kernel.result" -> Ok ()
    | Some actual -> Error (format Format.[ str "unexpected exported loop type: "; str actual; ])
    | None -> Error "missing exported loop binding"
  else
    Error (String.concat "\n" diagnostics)

let test_extensible_variant_constructors_lower_and_typecheck = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, impl_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "message.ml")
    ~text:{ocaml|
      type message = ..
      type message += Ack | Data of int

      let ack = Ack

      let classify = fun message ->
        match message with
        | Ack -> 0
        | Data value -> value
    |ocaml}
  in
  let (session, intf_source_id) = create_source session ~kind:Source.File ~origin:(Source.Label "message.mli")
    ~text:{ocaml|
      type message = ..
      type message += Ack | Data of int

      val ack : message
      val classify : message -> int
    |ocaml}
  in
  match prepare_snapshot_or_error session ~roots:[ impl_source_id ] with
  | Error _ as err -> err
  | Ok snapshot ->
      let impl_diagnostics = diagnostic_strings snapshot impl_source_id in
      let intf_diagnostics = diagnostic_strings snapshot intf_source_id in
      if not (List.is_empty impl_diagnostics) then
        Error (String.concat "\n" impl_diagnostics)
      else if not (List.is_empty intf_diagnostics) then
        Error (String.concat "\n" intf_diagnostics)
      else
        let () = Test.assert_equal
          ~expected:false
          ~actual:(has_signature_error snapshot impl_source_id) in
        let () = Test.assert_equal
          ~expected:false
          ~actual:(has_signature_error snapshot intf_source_id) in
        let () = Test.assert_equal
          ~expected:(Some "message")
          ~actual:(export_scheme snapshot impl_source_id "ack") in
        let () = Test.assert_equal
          ~expected:(Some "message -> int")
          ~actual:(export_scheme snapshot impl_source_id "classify") in
        Ok ()

let test_expansive_bindings_stay_monomorphic = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
    let id x = x
    let alias = id id
  |ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "value_restriction.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let id_type = export_scheme snapshot source_id "id" in
    let alias_type = export_scheme snapshot source_id "alias" in
    let () = Test.assert_equal ~expected:(Some "'a. 'a -> 'a") ~actual:id_type in
    let () = Test.assert_equal ~expected:(Some "'a -> 'a") ~actual:alias_type in
    Ok ()

let test_nonexpansive_list_bindings_still_generalize = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = "let empty = []\n" in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "list_value_restriction.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let empty_type = export_scheme snapshot source_id "empty" in
    let () = Test.assert_equal ~expected:(Some "'a. 'a list") ~actual:empty_type in
    Ok ()

let test_expansive_covariant_lists_still_generalize = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
    let make _ = []
    let xs = make ()
  |ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "relaxed_value_restriction.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let xs_type = export_scheme snapshot source_id "xs" in
    let () = Test.assert_equal ~expected:(Some "'a. 'a list") ~actual:xs_type in
    Ok ()

let test_expansive_covariant_nominal_types_still_generalize = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
    type 'a box = Box of 'a list
    let make _ = Box []
    let boxed = make ()
  |ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "nominal_value_restriction.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let boxed_type = export_scheme snapshot source_id "boxed" in
    let () = Test.assert_equal ~expected:(Some "'a. 'a box") ~actual:boxed_type in
    Ok ()

let test_expansive_covariant_record_types_still_generalize = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = {ocaml|
    type 'a box = { items: 'a list }
    let make _ = { items = [] }
    let boxed = make ()
  |ocaml}
  in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "record_value_restriction.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let boxed_type = export_scheme snapshot source_id "boxed" in
    let () = Test.assert_equal ~expected:(Some "'a. 'a box") ~actual:boxed_type in
    Ok ()

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests = [
        Test.case "local module aliases include public wrapper spellings" test_local_module_aliases_include_public_wrapper_spellings;
        Test.case "contextual local module depth prefers deeper shared prefix" test_contextual_local_module_depth_prefers_deeper_shared_prefix;
        Test.case "contextual local module depth keeps single segment suffixes" test_contextual_local_module_depth_keeps_single_segment_suffixes;
        Test.case "local module implicit opens skip enclosing alias wrappers" test_local_module_implicit_opens_skip_enclosing_alias_wrappers;
        Test.case "source id stays stable across updates" test_source_id_stays_stable_across_updates;
        Test.case "snapshots remain immutable after updates" test_snapshots_remain_immutable_after_updates;
        Test.case "type_at uses smallest indexed expression" test_type_at_uses_smallest_indexed_expression;
        Test.case "definition_at uses local pattern origin" test_definition_at_uses_local_pattern_origin;
        Test.case "definition_at uses exported module typings" test_definition_at_uses_exported_module_typings;
        Test.case "definition_at prefers interface export origin" test_definition_at_prefers_interface_export_origin;
        Test.case "definition_at follows include reexports" test_definition_at_follows_include_reexports;
        Test.case "snapshot without traces still reports diagnostics and module typings" test_snapshot_without_traces_still_reports_diagnostics_and_module_typings;
        Test.case "snapshot exposes implicit file modules" test_snapshot_exposes_implicit_file_modules;
        Test.case "prepare_snapshot uses implicit-opened alias modules with internal names" test_prepare_snapshot_uses_implicit_opened_alias_modules_with_internal_names;
        Test.case "prepare_snapshot resolves internal module dependencies by local alias" test_prepare_snapshot_resolves_internal_module_dependencies_by_local_alias;
        Test.case "prepare_snapshot prefers internal local alias dependencies over loaded modules" test_prepare_snapshot_prefers_internal_local_alias_dependencies_over_loaded_modules;
        Test.case "prepare_snapshot uses internal local alias dependencies transitively" test_prepare_snapshot_uses_internal_local_alias_dependencies_transitively;
        Test.case "prepare_snapshot internal local alias dependencies ignore source order" test_prepare_snapshot_internal_local_alias_dependencies_ignore_source_order;
        Test.case "prepare_snapshot nested internal local alias dependencies typecheck" test_prepare_snapshot_nested_internal_local_alias_dependencies_typecheck;
        Test.case "fold_package_sources resolves contextual local modules" test_fold_package_sources_resolves_contextual_local_modules;
        Test.case "fold_package_sources resolves root local module wrappers" test_fold_package_sources_resolves_root_local_module_wrappers;
        Test.case "fold_package_sources shares imported world semantics for open alias include" test_fold_package_sources_shares_imported_world_semantics_for_open_alias_include;
        Test.case "fold_package_sources persists package bundle" test_fold_package_sources_persists_package_bundle;
        Test.case "fold_package_sources keeps base loaded modules immutable" test_fold_package_sources_keeps_base_loaded_modules_immutable;
        Test.case "prepare_snapshot shares imported world semantics for open alias include" test_prepare_snapshot_shares_imported_world_semantics_for_open_alias_include;
        Test.case "prepare_snapshot nested unix submodule sees sibling ip_addr exports" test_prepare_snapshot_nested_unix_submodule_sees_sibling_ip_addr_exports;
        Test.case "prepare_snapshot wrapper module reexports unix exports to sibling modules" test_prepare_snapshot_wrapper_module_reexports_unix_exports_to_sibling_modules;
        Test.case "prepare_snapshot wrapper module preserves same-path nominal value types" test_prepare_snapshot_wrapper_module_preserves_same_path_nominal_value_types;
        Test.case "prepare_snapshot wrapper module preserves local result error surface" test_prepare_snapshot_wrapper_module_preserves_local_result_error_surface;
        Test.case "prepare_snapshot partial wrapper preserves nested module exports" test_prepare_snapshot_partial_wrapper_preserves_nested_module_exports;
        Test.case "prepare_snapshot paired module alias preserves interface-shaped sibling types" test_prepare_snapshot_paired_module_alias_preserves_interface_shaped_sibling_types;
        Test.case "prepare_snapshot nested module alias canonicalizes sibling error types" test_prepare_snapshot_nested_module_alias_canonicalizes_sibling_error_types;
        Test.case "prepare_snapshot kernel named wrapper alias preserves nested error types" test_prepare_snapshot_kernel_named_wrapper_alias_preserves_nested_error_types;
        Test.case "prepare_snapshot multiple wrapper aliases preserve nested error types" test_prepare_snapshot_multiple_wrapper_aliases_preserve_nested_error_types;
        Test.case "prepare_snapshot nested include wrapper alias preserves error types" test_prepare_snapshot_nested_include_wrapper_alias_preserves_error_types;
        Test.case "prepare_snapshot planner aliases preserve nested constructor owners" test_prepare_snapshot_planner_aliases_preserve_nested_constructor_owners;
        Test.case "prepare_snapshot query order preserves in-progress wrapper exports" test_prepare_snapshot_query_order_preserves_in_progress_wrapper_exports;
        Test.case "prepare_snapshot net wrapper graph preserves ip_addr exports" test_prepare_snapshot_net_wrapper_graph_preserves_ip_addr_exports;
        Test.case "prepare_snapshot net wrapper graph preserves ip_addr exports with paired unix" test_prepare_snapshot_net_wrapper_graph_preserves_ip_addr_exports_with_paired_unix;
        Test.case "implicit opens do not leak into module exports" test_implicit_opens_do_not_leak_into_module_exports;
        Test.case "snapshot exports interface declarations" test_snapshot_exports_interface_declarations;
        Test.case "snapshot exports interface externals" test_snapshot_exports_interface_externals;
        Test.case "snapshot collects module typings" test_snapshot_collects_module_typings;
        Test.case "snapshot module typings are canonical per module" test_snapshot_module_typings_are_canonical_per_module;
        Test.case "query module_typings_of uses the canonical root typings" test_query_module_typings_of_uses_canonical_root_typings;
        Test.case "paired modules export interface-shaped module typings" test_paired_modules_export_interface_shaped_module_typings;
        Test.case "paired modules export interface-shaped file summaries" test_paired_modules_export_interface_shaped_file_summaries;
        Test.case "paired modules report signature inclusion mismatches" test_paired_modules_report_signature_inclusion_mismatches;
        Test.case "paired modules skip signature inclusion for errored implementation" test_paired_modules_skip_signature_inclusion_for_errored_implementation;
        Test.case "paired modules skip signature inclusion for unsupported interface types" test_paired_modules_skip_signature_inclusion_for_unsupported_interface_types;
        Test.case "paired modules accept manifest alias specialization" test_paired_modules_accept_manifest_alias_specialization;
        Test.case "paired modules canonicalize builtin aliases in signature inclusion" test_paired_modules_canonicalize_builtin_aliases_in_signature_inclusion;
        Test.case "paired modules accept manifest alias value usage in signature inclusion" test_paired_modules_accept_manifest_alias_value_usage_in_signature_inclusion;
        Test.case "manifest option aliases canonicalize during inference" test_manifest_option_aliases_canonicalize_during_inference;
        Test.case "manifest aliases canonicalize across modules" test_manifest_aliases_canonicalize_across_modules;
        Test.case "paired modules accept option manifest aliases in signature inclusion" test_paired_modules_accept_option_manifest_aliases_in_signature_inclusion;
        Test.case "source input hash ignores source id and revision" test_source_input_hash_ignores_source_id_and_revision;
        Test.case "source input hash ignores comments and docstrings" test_source_input_hash_ignores_comments_and_docstrings;
        Test.case "source input hash changes with implicit opens" test_source_input_hash_changes_with_implicit_opens;
        Test.case "snapshot uses loaded module typings" test_snapshot_uses_loaded_module_typings;
        Test.case "prepare_snapshot hydrates module typings from store" test_prepare_snapshot_hydrates_module_typings_from_store;
        Test.case "prepare_snapshot keeps the store read-only during query forcing" test_prepare_snapshot_keeps_store_read_only_during_query_forcing;
        Test.case "fold_package_sources persists module typings from the authoritative engine" test_fold_package_sources_persists_module_typings_from_authoritative_engine;
        Test.case "prepare_snapshot emits structured events" test_prepare_snapshot_emits_structured_events;
        Test.case "prepare_snapshot emits structured diagnostics in events" test_prepare_snapshot_emits_structured_diagnostics_in_events;
        Test.case
          "prepare_snapshot keeps imported value payloads out of ambient bindings"
          test_prepare_snapshot_keeps_imported_value_payloads_out_of_ambient_bindings;
        Test.case
          "prepare_snapshot keeps diagnostics and exports stable after module forcing"
          test_prepare_snapshot_keeps_diagnostics_and_exports_stable_after_module_forcing;
        Test.case "prepare_snapshot only pairs required local modules" test_prepare_snapshot_only_pairs_required_local_modules;
        Test.case "prepare_snapshot reuses shared transitive local modules" test_prepare_snapshot_reuses_shared_transitive_local_modules;
        Test.case "prepare_snapshot reuses paired local modules across rooted snapshots" test_prepare_snapshot_reuses_paired_local_modules_across_rooted_snapshots;
        Test.case "prepare_snapshot reuses shared implicit-open alias modules" test_prepare_snapshot_reuses_shared_implicit_open_alias_modules;
        Test.case "prepare_snapshot store hydration emits structured events" test_prepare_snapshot_store_hydration_emits_structured_events;
        Test.case "prepare_snapshot missing requirements emit structured events" test_prepare_snapshot_missing_requirements_emit_structured_events;
        Test.case "prepare_snapshot reports match coverage diagnostics" test_prepare_snapshot_reports_match_coverage_diagnostics;
        Test.case "prepare_snapshot includes interface sibling dependencies" test_prepare_snapshot_includes_interface_sibling_dependencies;
        Test.case "loaded module typings override store" test_loaded_module_typings_override_store;
        Test.case "snapshot uses sibling source record types" test_snapshot_uses_sibling_source_record_types;
        Test.case "snapshot uses loaded module record types" test_snapshot_uses_loaded_module_record_types;
        Test.case "include reexports loaded module record types" test_include_reexports_loaded_module_record_types;
        Test.case "module alias reexports loaded module record types" test_module_alias_reexports_loaded_module_record_types;
        Test.case "include reexports loaded module typings" test_include_reexports_loaded_module_typings;
        Test.case "include reexports package-shaped loaded module typings" test_include_reexports_package_shaped_loaded_module_typings;
        Test.case "include module type of canonicalizes loaded nominal types" test_include_module_type_of_canonicalizes_loaded_nominal_types;
        Test.case "include module type of loaded modules canonicalizes nominal types" test_include_module_type_of_loaded_modules_canonicalizes_nominal_types;
        Test.case "loaded module reexports canonicalize dependency result aliases" test_loaded_module_reexports_canonicalize_dependency_result_aliases;
        Test.case "operator pattern bindings lower and typecheck" test_operator_pattern_bindings_lower_and_typecheck;
        Test.case "cons patterns lower and typecheck" test_cons_patterns_lower_and_typecheck;
        Test.case "recursive operator bindings typecheck" test_recursive_operator_bindings_typecheck;
        Test.case "language prelude supports <> operator" test_language_prelude_supports_angle_not_equal;
        Test.case "kernel ops surface typechecks" test_kernel_ops_surface_typechecks;
        Test.case "opened sibling double underscore exports typecheck" test_opened_sibling_double_underscore_exports_typecheck;
        Test.case "paired modules allow private top-level exception helpers" test_paired_modules_allow_private_top_level_exception_helpers;
        Test.case "paired modules include sibling exports during signature inclusion" test_paired_modules_include_sibling_exports_during_signature_inclusion;
        Test.case "paired modules include paired sibling exports during signature inclusion" test_paired_modules_include_paired_sibling_exports_during_signature_inclusion;
        Test.case "nonrec same-name option alias prefers outer type" test_nonrec_same_name_option_alias_prefers_outer_type;
        Test.case "nonrec same-name result alias prefers outer type" test_nonrec_same_name_result_alias_prefers_outer_type;
        Test.case "source analysis with loaded modules canonicalizes nominal types" test_source_analysis_with_loaded_modules_canonicalizes_nominal_types;
        Test.case "source analysis with opened loaded module canonicalizes nominal types" test_source_analysis_with_opened_loaded_module_canonicalizes_nominal_types;
        Test.case "snapshot exports opened loaded nominal types from implementation" test_snapshot_exports_opened_loaded_nominal_types_from_implementation;
        Test.case "snapshot type declarations use opened sibling nominal types" test_snapshot_type_decls_use_opened_sibling_nominal_types;
        Test.case "prepare_snapshot type declarations use opened sibling nominal types" test_prepare_snapshot_type_decls_use_opened_sibling_nominal_types;
        Test.case "prepare_snapshot type declarations use opened loaded nominal types" test_prepare_snapshot_type_decls_use_opened_loaded_nominal_types;
        Test.case
          "prepare_snapshot type declarations use opened loaded nominal types with underscored module name"
          test_prepare_snapshot_type_decls_use_opened_loaded_nominal_types_with_underscored_module_name;
        Test.case "prepare_snapshot canonicalizes sibling structural polyvariant exports" test_prepare_snapshot_polyvariant_exports_canonicalize_sibling_structural_types;
        Test.case
          "prepare_snapshot canonicalizes sibling structural polyvariant exports inside arrows"
          test_prepare_snapshot_polyvariant_exports_canonicalize_sibling_structural_types_inside_arrows;
        Test.case "module aliases reexport loaded module typings" test_module_alias_reexports_loaded_module_typings;
        Test.case "module aliases reexport same-named local modules" test_module_alias_reexports_same_named_local_modules;
        Test.case "loaded module typings preserve nested same-named alias exports" test_loaded_module_typings_preserve_nested_same_named_alias_exports;
        Test.case
          "paired loaded module typings preserve nested alias exports across include chains"
          test_paired_loaded_module_typings_preserve_nested_alias_exports_across_include_chain;
        Test.case "prepare_snapshot uses nested exports from local include of loaded module" test_prepare_snapshot_uses_nested_exports_from_local_include_of_loaded_module;
        Test.case "sibling sources use loaded module record reexports" test_sibling_source_uses_loaded_module_record_reexport;
        Test.case "prepare_snapshot is rooted" test_prepare_snapshot_is_rooted;
        Test.case "prepare_snapshot reports missing roots" test_prepare_snapshot_reports_missing_roots;
        Test.case "prepare_snapshot reports missing module summaries" test_prepare_snapshot_reports_missing_module_summary;
        Test.case "prepare_snapshot collects transitive missing modules" test_prepare_snapshot_collects_transitive_missing_modules;
        Test.case "prepare_snapshot collects missing modules from qualified references" test_prepare_snapshot_collects_missing_module_for_qualified_reference;
        Test.case "prepare_snapshot keeps nested sibling modules out of top-level requirements" test_prepare_snapshot_keeps_nested_sibling_modules_out_of_top_level_requirements;
        Test.case
          "prepare_snapshot keeps loaded nested module exports out of top-level requirements"
          test_prepare_snapshot_keeps_loaded_nested_module_exports_out_of_top_level_requirements;
        Test.case "prepare_snapshot imports bare local module exports into rooted analysis" test_prepare_snapshot_imports_bare_local_module_exports_into_rooted_analysis;
        Test.case "prepare_snapshot canonicalizes missing requirements" test_prepare_snapshot_canonicalizes_missing_requirements;
        Test.case "prepare_snapshot sorts missing modules by name" test_prepare_snapshot_sorts_missing_modules_by_name;
        Test.case "prepare_snapshot uses local Prelude variant constructors" test_prepare_snapshot_uses_local_prelude_variant_constructors;
        Test.case "prepare_snapshot uses sibling Int module with local Prelude" test_prepare_snapshot_uses_sibling_int_module_with_local_prelude;
        Test.case "prepare_snapshot types mutable record field assignment" test_prepare_snapshot_types_mutable_record_field_assignment;
        Test.case "prepare_snapshot types local source module pack" test_prepare_snapshot_types_local_source_module_pack;
        Test.case "check_source recovers when rooted preparation reports missing module summaries" test_check_source_recovers_when_snapshot_preparation_reports_missing_module_summaries;
        Test.case "match guards typecheck in pattern scope" test_match_guards_typecheck_in_pattern_scope;
        Test.case "optional arguments can be omitted and reordered" test_optional_arguments_can_be_omitted_and_reordered;
        Test.case "optional argument forwarding preserves option wrapper" test_optional_argument_forwarding_preserves_option_wrapper;
        Test.case "inline record constructor payloads use constructor owner" test_inline_record_constructor_payloads_use_constructor_owner;
        Test.case "explicit locally abstract let annotations are checked" test_explicit_locally_abstract_let_annotations_are_checked;
        Test.case "for loops lower and typecheck" test_for_loops_lower_and_typecheck;
        Test.case "if branches do not capture trailing sequences" test_if_branches_do_not_capture_trailing_sequences;
        Test.case "let operators lower and typecheck" test_let_operator_lower_and_typecheck;
        Test.case "external identity cast token shape typechecks" test_external_identity_cast_token_shape_typechecks;
        Test.case "first-class module pack and unpack typecheck" test_first_class_module_pack_and_unpack_typecheck;
        Test.case "first-class module existential event shape typechecks" test_first_class_module_existential_event_shape_typechecks;
        Test.case "prepare_snapshot paired module preserves first-class module value signatures" test_prepare_snapshot_paired_module_preserves_first_class_module_value_signatures;
        Test.case
          "prepare_snapshot paired module allows hidden manifest alias with same variant shape"
          test_prepare_snapshot_paired_module_allows_hidden_manifest_alias_with_same_variant_shape;
        Test.case "GADT constructor existentials typecheck" test_gadt_constructor_existentials_typecheck;
        Test.case "GADT tuple constructor existentials typecheck" test_gadt_tuple_constructor_existentials_typecheck;
        Test.case "selector receive with existential constructors typechecks" test_selector_receive_with_existential_constructors_typecheck;
        Test.case "extensible selector agent shape typechecks" test_extensible_selector_agent_shape_typechecks;
        Test.case "loaded module selector agent shape typechecks" test_loaded_module_selector_agent_shape_typechecks;
        Test.case "snapshot exposes loaded module agent support exports" test_snapshot_exposes_loaded_module_agent_support_exports;
        Test.case "deps collect loaded module agent dependencies" test_deps_collect_loaded_module_agent_dependencies;
        Test.case "selector alias chain across loaded modules typechecks" test_selector_alias_chain_across_loaded_modules_typechecks;
        Test.case "included extensible types preserve constructor identity across siblings" test_included_extensible_types_preserve_constructor_identity_across_siblings;
        Test.case "loaded module ref shape typechecks" test_loaded_module_ref_shape_typechecks;
        Test.case "spawn alias chain across loaded modules typechecks" test_spawn_alias_chain_across_loaded_modules_typechecks;
        Test.case "function body annotation alias chain exports canonical result" test_function_body_annotation_alias_chain_exports_canonical_result;
        Test.case "extensible variant constructors lower and typecheck" test_extensible_variant_constructors_lower_and_typecheck;
        Test.case "expansive bindings stay monomorphic" test_expansive_bindings_stay_monomorphic;
        Test.case "nonexpansive list bindings still generalize" test_nonexpansive_list_bindings_still_generalize;
        Test.case "expansive covariant lists still generalize" test_expansive_covariant_lists_still_generalize;
        Test.case "expansive covariant nominal types still generalize" test_expansive_covariant_nominal_types_still_generalize;
        Test.case "expansive covariant record types still generalize" test_expansive_covariant_record_types_still_generalize;
        Test.case "fun cases keep preceding parameters in scope" test_fun_cases_keep_preceding_parameters;
        Test.case "records flow through snapshot queries" test_records_flow_through_snapshot_queries;
      ]
      in
      Test.Cli.main ~name:"typ:session" ~tests ~args)
    ~args:Std.Env.args
    ()
