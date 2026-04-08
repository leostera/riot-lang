open Std
open Typ
open Typ.Analysis
open Typ.Diagnostics
open Typ.Model
open Typ.Session

let export_names = function
  | Some (FileSummary.TrustedExport { exports })
  | Some (FileSummary.ErroredExport { exports }) -> List.map fst exports
  | Some FileSummary.NoExport
  | None -> []

let export_scheme = fun snapshot source_id name ->
  match Query.export_of snapshot source_id with
  | Some (FileSummary.TrustedExport { exports })
  | Some (FileSummary.ErroredExport { exports }) -> List.assoc_opt name exports
  |> Option.map TypePrinter.scheme_to_string
  | Some FileSummary.NoExport
  | None -> None

let inferred_type_at = fun snapshot source_id offset ->
  Query.type_at snapshot source_id (Position.make ~offset) |> function
  | Some ty -> Some (TypePrinter.type_to_string ty)
  | None -> None

let definition_at = fun snapshot source_id offset ->
  Query.definition_at snapshot source_id (Position.make ~offset)

let source_origin_label = function
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
      | Query.Typing diagnostic ->
          Some (
            Diagnostic.code diagnostic,
            Diagnostic.severity diagnostic |> Diagnostic.severity_to_string,
            Diagnostic.message diagnostic
          )
      | Query.Parse _
      | Query.Lowering _ -> None
    )

let show_option = function
  | Some value -> "Some(" ^ value ^ ")"
  | None -> "None"

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

let module_typings_jsons = fun snapshot ->
  Snapshot.module_typings snapshot |> List.map ModuleTypings.Json.to_json

let typ_event_name = function
  | Event.PrepareSnapshotStarted _ -> "typ_prepare_snapshot_start"
  | Event.HydrateModuleTypingsStarted _ -> "typ_hydrate_module_typings_start"
  | Event.HydrateModuleTypingsFinished _ -> "typ_hydrate_module_typings_finish"
  | Event.PrepareSnapshotFailed _ -> "typ_prepare_snapshot_failed"
  | Event.PrepareSnapshotFinished _ -> "typ_prepare_snapshot_finish"
  | Event.SourceAnalysisStarted _ -> "typ_source_analysis_start"
  | Event.SourceAnalysisFinished _ -> "typ_source_analysis_finish"
  | Event.ModulePairingStarted _ -> "typ_module_pairing_start"
  | Event.ModulePairingFinished _ -> "typ_module_pairing_finish"

let exported_type_names = fun snapshot source_id ->
  match Query.module_typings_of snapshot source_id with
  | None -> []
  | Some typings ->
      ModuleTypings.type_decls typings |> List.map
        (fun (type_decl: FileSummary.type_decl) ->
          if IdentPath.is_empty type_decl.scope_path then
            type_decl.declaration.type_name
          else
            IdentPath.append_name type_decl.scope_path type_decl.declaration.type_name |> IdentPath.to_string)

let file_summary_export_names = fun snapshot source_id ->
  match Query.file_summary_of snapshot source_id with
  | None -> []
  | Some summary -> FileSummary.exports summary |> List.map fst

let prepare_snapshot_or_error = fun session ~roots ->
  match Session.prepare_snapshot session ~roots with
  | Ok snapshot -> Ok snapshot
  | Error missing -> Error ("unexpected missing requirements: "
  ^ Data.Json.to_string (Session.MissingRequirements.to_json missing))

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
  let module_path = IdentPath.of_name module_name in
  List.map
    (fun (name, scheme) -> (IdentPath.append_path module_path (IdentPath.of_string name), scheme))
    exports

let qualify_type_decls = fun module_name type_decls ->
  List.map
    (fun (type_decl: FileSummary.type_decl) ->
      {
        FileSummary.scope_path = IdentPath.prepend_name module_name type_decl.scope_path;
        declaration = type_decl.declaration
      })
    type_decls

let expect_cst = fun ~filename parse_result ->
  match Syn.build_cst parse_result with
  | Ok cst -> cst
  | Error (Syn.Parse_diagnostics diagnostics) -> panic
    ("expected successful CST for "
    ^ filename
    ^ " but parser reported diagnostics: "
    ^ String.concat "; " (List.map Syn.Diagnostic.to_string diagnostics))
  | Error (Syn.Cst_builder_error error) -> panic
    ("expected successful CST for " ^ filename ^ " but CST build failed: " ^ error.message)

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

let check_source_text = fun ~filename text ->
  let parse_result = Syn.parse ~filename text in
  let cst = expect_cst ~filename:(Path.to_string filename) parse_result in
  Check.check_source ~filename ~parse_result ~cst

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
          tags
        || List.exists type_has_named_path inherited
    | TypeRepr.Arrow { lhs; rhs; _ } -> type_has_named_path lhs || type_has_named_path rhs
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
  let () = Test.assert_equal ~expected:[ "x" ] ~actual:before_names in
  let () = Test.assert_equal ~expected:[ "y" ] ~actual:after_names in
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
  let () = Test.assert_equal ~expected:(Some "int") ~actual:before_type in
  let () = Test.assert_equal ~expected:(Some "bool") ~actual:after_type in
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
  let () = Test.assert_equal ~expected:(Some "int -> int") ~actual:callee_type in
  let () = Test.assert_equal ~expected:(Some "int") ~actual:argument_type in
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
      let () = Test.assert_equal ~expected:"definition_local.ml" ~actual:(source_origin_label definition.origin) in
      let expected_offset = offset_of_substring source "id x" |> Option.expect ~msg:"expected local binder" in
      let () = Test.assert_equal
        ~expected:true
        ~actual:(definition_covers_offset definition expected_offset) in
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
      let () = Test.assert_equal ~expected:"colors.ml" ~actual:(source_origin_label definition.origin) in
      let expected_offset =
        offset_of_substring colors_source "to_string value" |> Option.expect ~msg:"expected export binder"
      in
      let () = Test.assert_equal
        ~expected:true
        ~actual:(definition_covers_offset definition expected_offset) in
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
      let () = Test.assert_equal ~expected:"colors.mli" ~actual:(source_origin_label definition.origin) in
      let expected_offset =
        offset_of_substring intf_source "answer" |> Option.expect ~msg:"expected interface declaration"
      in
      let () = Test.assert_equal
        ~expected:true
        ~actual:(definition_covers_offset definition expected_offset) in
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
      let () = Test.assert_equal ~expected:"base.ml" ~actual:(source_origin_label definition.origin) in
      let expected_offset =
        offset_of_substring base_source "value = 1" |> Option.expect ~msg:"expected base binder"
      in
      let () = Test.assert_equal
        ~expected:true
        ~actual:(definition_covers_offset definition expected_offset) in
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
  let () = Test.assert_equal ~expected:[] ~actual:analysis.expr_traces in
  let () = Test.assert_equal ~expected:[] ~actual:analysis.item_traces in
  let () = Test.assert_equal
    ~expected:(Data.Json.Array [])
    ~actual:(TypeIndex.to_json analysis.type_index) in
  let () = Test.assert_equal ~expected:None ~actual:inferred in
  if
    List.exists (fun diagnostic -> Option.is_some (offset_of_substring diagnostic "unbound name")) diagnostics
  then
    match module_typings with
    | Some _ -> Ok ()
    | None -> Error "expected module typings even when traces are disabled"
  else
    Error ("expected unbound-name diagnostics, got:\n" ^ String.concat "\n" diagnostics)

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
    Error ("unexpected colors exports: " ^ String.concat ", " color_exports)
  else if not (midpoint_type = Some "int -> int -> int") then
    Error ("unexpected midpoint type: " ^ show_option midpoint_type)
  else if not (label_type = Some "string -> string") then
    Error ("unexpected label type: " ^ show_option label_type)
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
  let implicit_opens = [ IdentPath.of_string "Colors__Aliases" ] in
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
          Error ("unexpected answer type: " ^ show_option answer_type)
        else
          Ok ()

let test_implicit_opens_do_not_leak_into_module_exports = fun _ctx ->
  let config = Config.default |> Config.with_capture_traces ~capture_traces:true in
  let session = Session.empty ~config in
  let helper_text = "let twice x = x + x\n" in
  let helper_parse_result = Syn.parse ~filename:(Path.v "helper.ml") helper_text in
  let helper_cst = expect_cst ~filename:"helper.ml" helper_parse_result in
  let aliases_text = "module Helper = Colors__Helper\n"
  ^ "\n"
  ^ "module Super = struct\n"
  ^ "  module Helper = Colors__Helper\n"
  ^ "end\n" in
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
  let implicit_opens = [ IdentPath.of_string "Colors__Aliases" ] in
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
      Error ("unexpected helper exports: "
      ^ String.concat ", " exports
      ^ "\n"
      ^ String.concat "\n" (trace_debug snapshot helper_source_id))
    else
      Ok ()

let test_snapshot_exports_interface_declarations = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source =
    "type color\n"
    ^ "val id : 'a -> 'a\n"
    ^ "module Local : sig\n"
    ^ "  type t\n"
    ^ "  val id : t -> t\n"
    ^ "end\n"
    ^ "module Uses_outer : sig\n"
    ^ "  val paint : color -> color\n"
    ^ "end\n"
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
    let () = Test.assert_equal ~expected:(Some "'a. 'a -> 'a") ~actual:id_type in
    let () = Test.assert_equal ~expected:(Some "Local.t -> Local.t") ~actual:local_id_type in
    let () = Test.assert_equal ~expected:(Some "color -> color") ~actual:paint_type in
    let () = Test.assert_equal ~expected:[ "color"; "Local.t" ] ~actual:type_names in
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
    let () = Test.assert_equal ~expected:(Some "string -> int") ~actual:strlen_type in
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
  let () = Test.assert_equal ~expected:[ "trusted_export"; "errored_export" ] ~actual:tags in
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
  let () = Test.assert_equal ~expected:[ "Colors" ] ~actual:module_names in
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
      ("expected one canonical module typings value but got " ^ string_of_int (List.length typings))
  in
  let typings_json source_id =
    match Query.module_typings_of snapshot source_id with
    | Some typings -> ModuleTypings.Json.to_json typings |> Data.Json.to_string
    | None -> panic ("expected module typings for " ^ SourceId.to_string source_id)
  in
  let impl_json = typings_json impl_source_id in
  let intf_json = typings_json intf_source_id in
  let () = Test.assert_equal ~expected:canonical_json ~actual:impl_json in
  let () = Test.assert_equal ~expected:canonical_json ~actual:intf_json in
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
    | Some typings -> ModuleTypings.exports typings |> List.map fst
  in
  let () = Test.assert_equal ~expected:[ "answer" ] ~actual:(export_names_for impl_source_id) in
  let () = Test.assert_equal ~expected:[ "answer" ] ~actual:(export_names_for intf_source_id) in
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
  let () = Test.assert_equal ~expected:[ "answer" ] ~actual:(export_names (Query.export_of snapshot impl_source_id)) in
  let () = Test.assert_equal ~expected:[ "answer" ] ~actual:(export_names (Query.export_of snapshot intf_source_id)) in
  let () = Test.assert_equal ~expected:[ "answer" ] ~actual:(file_summary_export_names snapshot impl_source_id) in
  let () = Test.assert_equal ~expected:[ "answer" ] ~actual:(file_summary_export_names snapshot intf_source_id) in
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
  let () = Test.assert_equal ~expected:true ~actual:(has_signature_error impl_source_id) in
  let () = Test.assert_equal ~expected:true ~actual:(has_signature_error intf_source_id) in
  let () = Test.assert_equal
    ~expected:[]
    ~actual:((ModuleTypings.exports impl_typings |> List.map fst)) in
  let () = Test.assert_equal
    ~expected:[]
    ~actual:((ModuleTypings.exports intf_typings |> List.map fst)) in
  let () = Test.assert_equal ~expected:[] ~actual:(export_names (Query.export_of snapshot impl_source_id)) in
  let () = Test.assert_equal ~expected:[] ~actual:(export_names (Query.export_of snapshot intf_source_id)) in
  let () = Test.assert_equal ~expected:[] ~actual:(file_summary_export_names snapshot impl_source_id) in
  let () = Test.assert_equal ~expected:[] ~actual:(file_summary_export_names snapshot intf_source_id) in
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
  let () = Test.assert_equal ~expected:true ~actual:(has_unbound_name snapshot impl_source_id) in
  let () = Test.assert_equal ~expected:false ~actual:(has_signature_error impl_source_id) in
  let () = Test.assert_equal ~expected:false ~actual:(has_signature_error intf_source_id) in
  let () = Test.assert_equal ~expected:[ "answer" ] ~actual:((ModuleTypings.exports impl_typings |> List.map fst)) in
  let () = Test.assert_equal ~expected:[ "answer" ] ~actual:((ModuleTypings.exports intf_typings |> List.map fst)) in
  let () = Test.assert_equal ~expected:(Some "int") ~actual:(export_scheme snapshot impl_source_id "answer") in
  let () = Test.assert_equal ~expected:(Some "int") ~actual:(export_scheme snapshot intf_source_id "answer") in
  let () = Test.assert_equal ~expected:[ "answer" ] ~actual:(file_summary_export_names snapshot impl_source_id) in
  let () = Test.assert_equal ~expected:[ "answer" ] ~actual:(file_summary_export_names snapshot intf_source_id) in
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
  let () = Test.assert_equal ~expected:false ~actual:(has_signature_error impl_source_id) in
  let () = Test.assert_equal ~expected:false ~actual:(has_signature_error intf_source_id) in
  let () = Test.assert_equal ~expected:false ~actual:(has_unsupported_syntax snapshot impl_source_id) in
  let () = Test.assert_equal ~expected:true ~actual:(has_unsupported_syntax snapshot intf_source_id) in
  let () = Test.assert_equal
    ~expected:[ "to_escape_seq" ]
    ~actual:((ModuleTypings.exports impl_typings |> List.map fst)) in
  let () = Test.assert_equal
    ~expected:[ "to_escape_seq" ]
    ~actual:((ModuleTypings.exports intf_typings |> List.map fst)) in
  let () = Test.assert_equal
    ~expected:[ "to_escape_seq" ]
    ~actual:(file_summary_export_names snapshot impl_source_id) in
  let () = Test.assert_equal
    ~expected:[ "to_escape_seq" ]
    ~actual:(file_summary_export_names snapshot intf_source_id) in
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
    ~implicit_opens:[ IdentPath.of_string "Colors__Aliases" ]
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
  let () = Test.assert_equal ~expected:[ "Blend_demo" ] ~actual:summary_modules in
  if demo_has_unbound_name then
    Error (String.concat "\n" (diagnostic_strings snapshot demo_source_id))
  else
    let () = Test.assert_equal ~expected:(Some "int -> int -> int") ~actual:midpoint_type in
    let () = Test.assert_equal ~expected:(Some "string -> string") ~actual:label_type in
    Ok ()

let test_snapshot_uses_bootstrap_float_to_string = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = "let rendered = Float.to_string 1.0\n" in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "float_to_string.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let float_to_string_offset =
      offset_of_substring source "Float.to_string"
      |> Option.expect ~msg:"expected Float.to_string in test source" in
    let rendered_type = export_scheme snapshot source_id "rendered" in
    let float_to_string_type = inferred_type_at snapshot source_id float_to_string_offset in
    let () = Test.assert_equal ~expected:(Some "string") ~actual:rendered_type in
    let () = Test.assert_equal
      ~expected:(Some "?precision:int -> float -> string")
      ~actual:float_to_string_type in
    Ok ()

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
      | Error missing -> Error ("expected store-backed snapshot preparation to succeed, got "
      ^ (Session.MissingRequirements.to_json missing |> Data.Json.to_string))
      | Ok snapshot ->
          let diagnostics = diagnostic_strings snapshot demo_source_id in
          if not (List.is_empty diagnostics) then
            Error (String.concat "\n" diagnostics)
          else
            let midpoint_type = inferred_type_at snapshot demo_source_id 34 in
            let label_type = inferred_type_at snapshot demo_source_id 58 in
            let () = Test.assert_equal ~expected:(Some "int -> int -> int") ~actual:midpoint_type in
            let () = Test.assert_equal ~expected:(Some "string -> string") ~actual:label_type in
            Ok ())

let test_prepare_snapshot_emits_structured_events = fun _ctx ->
  let events = ref [] in
  let config = Config.default |> Config.with_on_event ~on_event:(fun event -> events := !events @ [ event ]) in
  let session = Session.empty ~config in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "demo.ml")
    ~text:{ocaml|
      let answer = 1
    |ocaml} in
  match Session.prepare_snapshot session ~roots:[ source_id ] with
  | Error missing -> Error ("expected rooted snapshot, got "
  ^ (Session.MissingRequirements.to_json missing |> Data.Json.to_string))
  | Ok snapshot ->
      let _ = Query.analysis_of_source snapshot source_id |> Option.expect ~msg:"expected analysis" in
      match !events with
      | [
        Event.PrepareSnapshotStarted _;
        Event.PrepareSnapshotFinished _;
        Event.ModulePairingStarted _;
        Event.ModulePairingStarted _;
        Event.SourceAnalysisStarted {
          source_id = base_source_id;
          module_name = base_module_name;
          mode = Event.BaseAnalysis;
          _
        };
        Event.SourceAnalysisFinished {
          source_id = finished_base_source_id;
          module_name = finished_base_module_name;
          mode = Event.BaseAnalysis;
          export_status = Event.TrustedExport;
          _
        };
        Event.ModulePairingFinished {
          module_name = paired_base_module_name;
          export_status = Event.TrustedExport;
          export_count = 1;
          type_decl_count = 0;
          _
        };
        Event.SourceAnalysisStarted {
          source_id = analysis_source_id;
          module_name;
          mode = Event.SnapshotAnalysis;
          _
        };
        Event.SourceAnalysisFinished {
          source_id = finished_source_id;
          module_name = finished_module_name;
          mode = Event.SnapshotAnalysis;
          export_status = Event.TrustedExport;
          _
        };
        Event.ModulePairingFinished {
          module_name = paired_module_name;
          export_status = Event.TrustedExport;
          export_count = 1;
          type_decl_count = 0;
          _
        };
      ] ->
          let expected_source_id = SourceId.to_int source_id in
          if List.for_all
            (fun actual_source_id ->
              SourceId.to_int actual_source_id = expected_source_id)
            [ base_source_id; finished_base_source_id; analysis_source_id; finished_source_id ]
             && List.for_all
               (fun actual_module_name -> String.equal actual_module_name "Demo")
               [
                 base_module_name;
                 finished_base_module_name;
                 paired_base_module_name;
                 module_name;
                 finished_module_name;
                 paired_module_name;
               ]
          then
            Ok ()
          else
            Error ("unexpected structured event payloads: "
            ^ (Data.Json.Array (!events |> List.map Event.to_json) |> Data.Json.to_string))
      | _ -> Error ("unexpected typ event payloads: "
      ^ (Data.Json.Array (!events |> List.map Event.to_json) |> Data.Json.to_string))

let test_prepare_snapshot_store_hydration_emits_structured_events = fun _ctx ->
  with_typ_store
    (fun store ->
      let baseline_loaded_module_count = List.length Config.default.loaded_modules in
      let seed_session = Session.empty ~config:Config.default in
      let (seed_session, colors_source_id) = create_source
        seed_session
        ~kind:Source.File
        ~origin:(Source.Label "colors.ml")
        ~text:{ocaml|
          module RGB = struct
            let blend x y = x
          end

          let to_string value = value
        |ocaml} in
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
      let (session, demo_source_id) = create_source
        session
        ~kind:Source.File
        ~origin:(Source.Label "blend_demo.ml")
        ~text:{ocaml|
          open Colors

          let midpoint = RGB.blend 1 2
          let label = to_string "ok"
        |ocaml} in
      match Session.prepare_snapshot session ~roots:[ demo_source_id ] with
      | Error missing -> Error ("expected store-backed snapshot, got "
      ^ (Session.MissingRequirements.to_json missing |> Data.Json.to_string))
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
              Event.PrepareSnapshotStarted _;
              Event.HydrateModuleTypingsStarted { missing_modules; _ };
              Event.HydrateModuleTypingsFinished { hydrated_modules; loaded_module_count; _ };
              Event.PrepareSnapshotFinished { loaded_module_count = final_loaded_module_count; _ };
            ] ->
                if missing_modules = [ "Colors"; "RGB" ]
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
  let config = Config.default |> Config.with_on_event ~on_event:(fun event -> events := !events @ [ event ]) in
  let session = Session.empty ~config in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "demo.ml")
    ~text:{ocaml|
      open Missing

      let answer = value
    |ocaml} in
  match Session.prepare_snapshot session ~roots:[ source_id ] with
  | Ok _ -> Error "expected missing module requirements"
  | Error _missing ->
      let actual = !events |> List.map typ_event_name in
      let expected = [
        "typ_prepare_snapshot_start";
        "typ_prepare_snapshot_failed";
      ] in
      let () = Test.assert_equal ~expected ~actual in
      match !events with
      | [
        Event.PrepareSnapshotStarted _;
        Event.PrepareSnapshotFailed { missing_root_source_ids; missing_modules; _ };
      ] ->
          let () = Test.assert_equal ~expected:[] ~actual:(missing_root_source_ids |> List.map SourceId.to_int) in
          let () = Test.assert_equal ~expected:[ "Missing" ] ~actual:missing_modules in
          Ok ()
      | _ -> Error ("unexpected missing-requirements events: "
      ^ (Data.Json.Array (!events |> List.map Event.to_json) |> Data.Json.to_string))

let test_prepare_snapshot_reports_match_coverage_diagnostics = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source =
    "let nonexhaustive x =\n"
    ^ "  match x with\n"
    ^ "  | Some value -> value\n"
    ^ "\n"
    ^ "let redundant x =\n"
    ^ "  match x with\n"
    ^ "  | _ -> 0\n"
    ^ "  | Some value -> value\n"
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
  let source = "open Colors\n" ^ "let origin = { x = 0; y = 0 }\n" ^ "let total point = point.x + point.y\n" in
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
  let source = "open Colors\n" ^ "let origin = { x = 0; y = 0 }\n" ^ "let total point = point.x + point.y\n" in
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
        (IdentPath.to_segments type_decl.scope_path, type_decl.declaration.type_name)) in
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
        (IdentPath.to_segments type_decl.scope_path, type_decl.declaration.type_name)) in
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
  let (seed_session, helpers_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "helpers.ml")
    ~text:"let id x = x\nlet wrap value = Some value\n" in
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
    let () = Test.assert_equal ~expected:(Some "'a. 'a -> 'a") ~actual:id_type in
    let () = Test.assert_equal ~expected:(Some "'a. 'a -> 'a option") ~actual:wrap_type in
    let () = Test.assert_equal ~expected:(Some "int option") ~actual:answer_type in
    let () = Test.assert_equal ~expected:[ "answer"; "id"; "wrap" ] ~actual:exported_names in
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
  let config = Config.default
  |> Config.with_ambient
    ~ambient:(qualify_exports
      (ModuleTypings.module_name loaded_actors)
      (ModuleTypings.exports loaded_actors))
  |> Config.with_ambient_type_decls
    ~ambient_type_decls:(qualify_type_decls
      (ModuleTypings.module_name loaded_actors)
      (ModuleTypings.type_decls loaded_actors)) in
  let source = prepared_source
    ~filename:"process.mli"
    ~text:(("include module type of Actors.Process\n" ^ "val spawn: (unit -> (unit, exit_reason) result) -> int\n")) in
  let analysis = SourceAnalysis.analyze ~config source in
  let diagnostics = analysis.lowering_diagnostics @ analysis.typing_diagnostics
  |> List.map Diagnostic.to_string in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    match List.assoc_opt "spawn" (FileSummary.exports analysis.file_summary) with
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
  let source = "include module type of Actors.Process\n" ^ "val spawn: (unit -> (unit, exit_reason) result) -> int\n" in
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
          match List.assoc_opt "spawn" exports with
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
  let (seed_session, kernel_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "kernel.mli")
    ~text:"type ('ok, 'error) result = ('ok, 'error) Stdlib.result\n" in
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
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"let pid = Std.spawn (fun () -> Ok ())\n" in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let pid_type = export_scheme snapshot source_id "pid" in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:pid_type in
    Ok ()

let test_include_module_type_of_stdlib_float_uses_bootstrap_module_typings = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "float.mli")
    ~text:"include module type of Stdlib.Float\n" in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  let exports = export_names (Query.export_of snapshot source_id) |> List.sort String.compare in
  let expected_exports = [
    "cbrt";
    "of_int";
    "pow";
    "round";
    "to_int";
  ] in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else if not (List.equal String.equal exports expected_exports) then
    Error ("unexpected exports: " ^ String.concat ", " exports)
  else
    Ok ()

let test_include_module_type_of_ocaml_stdlib_hashtbl_uses_loaded_module_typings = fun _ctx ->
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:OCamlStdlib.summaries in
  let session = Session.empty ~config in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "hashtbl.mli")
    ~text:"include module type of Stdlib.Hashtbl\n" in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  let exports = export_names (Query.export_of snapshot source_id) |> List.sort String.compare in
  let expected_exports = [
    "clear";
    "copy";
    "create";
    "find";
    "fold";
    "hash";
    "iter";
    "length";
    "mem";
    "remove";
    "replace";
    "seeded_hash";
  ] in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else if not (List.equal String.equal exports expected_exports) then
    Error ("unexpected exports: " ^ String.concat ", " exports)
  else
    Ok ()

let test_ocaml_stdlib_root_operators_typecheck = fun _ctx ->
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:OCamlStdlib.summaries in
  let session = Session.empty ~config in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "ops.ml")
    ~text:
      "let sum = Stdlib.( + ) 1 2\n\
       let ok = Stdlib.( && ) true false\n\
       let eq = Stdlib.( = ) 1 1\n\
       let xs = Stdlib.( @ ) [1] [2]\n\
       let piped = Stdlib.( |> ) 1 Stdlib.succ\n" in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let () = Test.assert_equal ~expected:(Some "int") ~actual:(export_scheme snapshot source_id "sum") in
    let () = Test.assert_equal ~expected:(Some "bool") ~actual:(export_scheme snapshot source_id "ok") in
    let () = Test.assert_equal ~expected:(Some "bool") ~actual:(export_scheme snapshot source_id "eq") in
    let () = Test.assert_equal ~expected:(Some "int list") ~actual:(export_scheme snapshot source_id "xs") in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:(export_scheme snapshot source_id "piped") in
    Ok ()

let test_ocaml_stdlib_sys_signal_behavior_typechecks = fun _ctx ->
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:OCamlStdlib.summaries in
  let session = Session.empty ~config in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "system.ml")
    ~text:
      "type nonrec signal_behavior = Stdlib.Sys.signal_behavior =\n\
       | Signal_default\n\
       | Signal_ignore\n\
       | Signal_handle of (int -> unit)\n\
       let previous = Stdlib.Sys.signal Stdlib.Sys.sigint (Stdlib.Sys.Signal_handle (fun _ -> ()))\n" in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    Ok ()

let test_ocaml_unix_stats_errors_and_commands_typecheck = fun _ctx ->
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:OCamlStdlib.summaries in
  let session = Session.empty ~config in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "unix_file.ml")
    ~text:{ocaml|
      let is_regular stats =
        let stats = (stats: Unix.stats) in
        stats.st_kind = Unix.S_REG

      let seek fd = Unix.lseek fd 0 Unix.SEEK_SET

      let lock fd = Unix.lockf fd Unix.F_LOCK 0

      let stat_or_error path =
        try Ok (Unix.stat path) with
        | Unix.Unix_error (err, _, _) -> Error (Unix.error_message err)
    |ocaml} in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    Ok ()

let test_ocaml_unix_socket_and_addr_info_typecheck = fun _ctx ->
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:OCamlStdlib.summaries in
  let session = Session.empty ~config in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "net_addr.ml")
    ~text:{ocaml|
      let unwrap sockaddr =
        match sockaddr with
        | Unix.ADDR_INET (addr, port) -> Some (Unix.string_of_inet_addr addr, port)
        | Unix.ADDR_UNIX _ -> None

      let choose info =
        let ai_family = info.ai_family in
        let ai_addr = info.ai_addr in
        let ai_socktype = info.ai_socktype in
        let ai_protocol = info.ai_protocol in
        match ai_addr with
        | Unix.ADDR_INET (addr, port) ->
            if (ai_family = Unix.PF_INET || ai_family = Unix.PF_INET6)
               && (ai_socktype = Unix.SOCK_STREAM || ai_socktype = Unix.SOCK_DGRAM) then
              Some (Unix.string_of_inet_addr addr, port, ai_protocol)
            else
              None
        | Unix.ADDR_UNIX _ -> None

      let _ = Unix.getaddrinfo "localhost" "80" []
      let _ = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0
    |ocaml} in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    Ok ()

let test_ocaml_unix_terminal_process_and_labels_typecheck = fun _ctx ->
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:OCamlStdlib.summaries in
  let session = Session.empty ~config in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "terminal.ml")
    ~text:{ocaml|
      let make_raw_mode termios =
        Unix.{ termios with c_echo = false; c_icanon = false; c_icrnl = false }

      let default_termios () =
        Unix.{
          c_ignbrk = false;
          c_brkint = false;
          c_ignpar = false;
          c_parmrk = false;
          c_inpck = false;
          c_istrip = false;
          c_inlcr = false;
          c_igncr = false;
          c_icrnl = false;
          c_ixon = false;
          c_ixoff = false;
          c_opost = false;
          c_obaud = 0;
          c_ibaud = 0;
          c_csize = 0;
          c_cstopb = 0;
          c_cread = false;
          c_parenb = false;
          c_parodd = false;
          c_hupcl = false;
          c_clocal = false;
          c_isig = false;
          c_icanon = false;
          c_noflsh = false;
          c_echo = false;
          c_echoe = false;
          c_echok = false;
          c_echonl = false;
          c_vintr = '\000';
          c_vquit = '\000';
          c_verase = '\000';
          c_vkill = '\000';
          c_veof = '\000';
          c_veol = '\000';
          c_vmin = 0;
          c_vtime = 0;
          c_vstart = '\000';
          c_vstop = '\000';
        }

      let status_code = function
        | Unix.WEXITED code -> code
        | Unix.WSIGNALED signal -> signal
        | Unix.WSTOPPED signal -> signal

      let copy_once fd buf =
        let len = UnixLabels.read fd ~buf ~pos:0 ~len:(Bytes.length buf) in
        UnixLabels.write fd ~buf ~pos:0 ~len

      let _ = Unix.waitpid [ Unix.WNOHANG ] 0
    |ocaml} in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    Ok ()

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
  let config = Config.default
  |> Config.with_loaded_modules ~loaded_modules
  |> Config.with_ambient
    ~ambient:((loaded_modules
    |> List.concat_map
      (fun typings ->
        qualify_exports (ModuleTypings.module_name typings) (ModuleTypings.exports typings))))
  |> Config.with_ambient_type_decls
    ~ambient_type_decls:((loaded_modules
    |> List.concat_map
      (fun typings ->
        qualify_type_decls (ModuleTypings.module_name typings) (ModuleTypings.type_decls typings)))) in
  let source = prepared_source
    ~filename:"process.mli"
    ~text:(("include module type of Actors.Process\n" ^ "val spawn: (unit -> (unit, exit_reason) result) -> int\n")) in
  let analysis = SourceAnalysis.analyze ~config source in
  let diagnostics = analysis.lowering_diagnostics @ analysis.typing_diagnostics
  |> List.map Diagnostic.to_string in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    match List.assoc_opt "spawn" (FileSummary.exports analysis.file_summary) with
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
  let config = Config.default
  |> Config.with_loaded_modules ~loaded_modules
  |> Config.with_ambient
    ~ambient:((loaded_modules
    |> List.concat_map
      (fun typings ->
        qualify_exports (ModuleTypings.module_name typings) (ModuleTypings.exports typings))))
  |> Config.with_ambient_type_decls
    ~ambient_type_decls:((loaded_modules
    |> List.concat_map
      (fun typings ->
        qualify_type_decls (ModuleTypings.module_name typings) (ModuleTypings.type_decls typings)))) in
  let source = prepared_source
    ~filename:"reader.mli"
    ~text:(("open Common\n" ^ "val close: unit -> (unit, error) result\n")) in
  let analysis = SourceAnalysis.analyze ~config source in
  let diagnostics = analysis.lowering_diagnostics @ analysis.typing_diagnostics
  |> List.map Diagnostic.to_string in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    match List.assoc_opt "close" (FileSummary.exports analysis.file_summary) with
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
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "reader.ml")
    ~text:(("open Common\n" ^ "let close (): (unit, error) result = Ok ()\n")) in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    match Query.module_typings_of snapshot source_id with
    | None -> Error "expected reader module typings"
    | Some typings -> (
        match List.assoc_opt "close" (ModuleTypings.exports typings) with
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
  let (session, reader_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "reader.ml")
    ~text:(("open Common\n" ^ "type wrapped = Wrap of error\n")) in
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
  let (session, reader_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "reader.ml")
    ~text:(("open Common\n" ^ "type wrapped = Wrap of error\n")) in
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
  let (session, reader_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "reader.ml")
    ~text:(("open Common\n" ^ "type wrapped = Wrap of error\n")) in
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
  let (session, reader_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "markdown_lower.ml")
    ~text:(("open Markdown_parser\n" ^ "type inline_stack_item = Inline_node of inline_node\n")) in
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
  let (session, colors_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:(("type rgb = [ `rgb of int * int * int ]\n")
    ^ "module ANSI = struct\n"
    ^ "  let first = Ansi_table.to_rgb.(0)\n"
    ^ "end\n") in
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
  let (session, colors_source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:(("type ansi = [ `ansi of int ]\n")
    ^ "type rgb = [ `rgb of int * int * int ]\n"
    ^ "module ANSI = struct\n"
    ^ "  let to_rgb = fun (`ansi i) ->\n"
    ^ "    let _ = i in\n"
    ^ "    Ansi_table.to_rgb.(0)\n"
    ^ "end\n") in
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
  let (seed_session, helpers_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "helpers.ml")
    ~text:"let id x = x\nlet wrap value = Some value\n" in
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
    else if not (util_wrap_type = Some "'a. 'a -> 'a option") then
      Error ("unexpected Util.wrap type: " ^ show_option util_wrap_type)
    else if not (answer_type = Some "int option") then
      Error ("unexpected answer type: " ^ show_option answer_type)
    else
      Ok ()

let test_loaded_module_alias_chain_preserves_interface_declared_values = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, _float_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "float.mli")
    ~text:("include module type of Stdlib.Float\n"
    ^ "val to_string : ?precision:int -> float -> string\n") in
  let (seed_session, std_source_id) = create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "std.mli")
    ~text:"module Float = Float\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_std =
    match Query.module_typings_of seed_snapshot std_source_id with
    | Some typings -> typings
    | None -> panic "expected std module typings"
  in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ loaded_std ] in
  let session = Session.empty ~config in
  let (session, source_id) = create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "consumer.ml")
    ~text:("open Std\n"
    ^ "let render = Float.to_string 1.0\n") in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let float_to_string_type = inferred_type_at snapshot source_id 22 in
    let render_type = export_scheme snapshot source_id "render" in
    let () = Test.assert_equal
      ~expected:(Some "?precision:int -> float -> string")
      ~actual:float_to_string_type in
    let () = Test.assert_equal ~expected:(Some "string") ~actual:render_type in
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
  let std_exported_names = ModuleTypings.exports std_summary |> List.map fst in
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
  let kernel_exported_names = ModuleTypings.exports kernel_summary |> List.map fst in
  let kernel_create_type =
    ModuleTypings.exports kernel_summary
    |> List.assoc_opt "Sync.Cell.create"
    |> Option.map TypePrinter.scheme_to_string in
  if not (List.equal String.equal kernel_exported_names [ "Sync.Cell.create" ]) then
    Error ("unexpected kernel exports: " ^ String.concat ", " kernel_exported_names)
  else if not (kernel_create_type = Some "'a. 'a -> 'a") then
    Error ("unexpected kernel Sync.Cell.create type: " ^ show_option kernel_create_type)
  else
    let std_seed_config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ kernel_summary ] in
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
    let std_exported_names = ModuleTypings.exports std_summary |> List.map fst in
    let std_create_type =
      ModuleTypings.exports std_summary
      |> List.assoc_opt "Sync.Cell.create"
      |> Option.map TypePrinter.scheme_to_string in
    if not (List.equal String.equal std_exported_names [ "Sync.Cell.create" ]) then
      Error ("unexpected std exports: " ^ String.concat ", " std_exported_names)
    else if not (std_create_type = Some "'a. 'a -> 'a") then
      Error ("unexpected std Sync.Cell.create type: " ^ show_option std_create_type)
    else
      let client_config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ std_summary ] in
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
              Error ("unexpected answer type: " ^ show_option answer_type
              ^ " callee=" ^ show_option callee_type)

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
  let consumer_source = "include Helpers\n" ^ "let origin = { x = 0; y = 0 }\n" in
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

let test_check_source_recovers_when_snapshot_preparation_reports_missing_module_summaries = fun _ctx ->
  let report = check_source_text ~filename:(Path.v "uses_missing_module.ml") "open Missing_module\nlet answer = Missing_module.value 1\n" in
  let diagnostics = List.length report.parse_diagnostics
  + List.length report.lowering_diagnostics
  + List.length report.typing_diagnostics in
  if diagnostics > 0 then
    Ok ()
  else
    Error "expected one-shot check_source to surface diagnostics instead of panicking"

let test_match_guards_typecheck_in_pattern_scope = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = "let classify value =\n"
  ^ "  match value with\n"
  ^ "  | Some n when n > 0 -> n\n"
  ^ "  | Some _ -> 0\n"
  ^ "  | None -> 0\n" in
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
    if not (classify_type = Some "(int option) -> int") then
      Error ("unexpected classify type: " ^ show_option classify_type)
    else if not (guard_binding_type = Some "int") then
      Error ("unexpected guard binding type: " ^ show_option guard_binding_type)
    else
      Ok ()

let test_optional_arguments_can_be_omitted_and_reordered = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = "let make_key = fun ?(kind = 0) ?(mods = 1) code -> code + kind + mods\n"
  ^ "let omitted = make_key 3\n"
  ^ "let reordered = make_key ~mods:4 3\n"
  ^ "let explicit = make_key ~kind:5 ~mods:6 7\n" in
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
  let source = "type point = { x: int; y: int }\n"
  ^ "let origin = { x = 0; y = 0 }\n"
  ^ "let move_x point dx = { point with x = point.x + dx }\n"
  ^ "let total = fun { x; y } -> x + y\n"
  ^ "let answer = total (move_x origin 3)\n" in
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

let test_expansive_bindings_stay_monomorphic = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = "let id x = x\n" ^ "let alias = id id\n" in
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
  let source = "let make _ = []\n" ^ "let xs = make ()\n" in
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
  let source = "type 'a box = Box of 'a list\n" ^ "let make _ = Box []\n" ^ "let boxed = make ()\n" in
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
  let source = "type 'a box = { items: 'a list }\n" ^ "let make _ = { items = [] }\n" ^ "let boxed = make ()\n" in
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
        Test.case "source input hash ignores source id and revision" test_source_input_hash_ignores_source_id_and_revision;
        Test.case "source input hash ignores comments and docstrings" test_source_input_hash_ignores_comments_and_docstrings;
        Test.case "source input hash changes with implicit opens" test_source_input_hash_changes_with_implicit_opens;
        Test.case "snapshot uses loaded module typings" test_snapshot_uses_loaded_module_typings;
        Test.case "snapshot uses bootstrap Float.to_string" test_snapshot_uses_bootstrap_float_to_string;
        Test.case "prepare_snapshot hydrates module typings from store" test_prepare_snapshot_hydrates_module_typings_from_store;
        Test.case "prepare_snapshot emits structured events" test_prepare_snapshot_emits_structured_events;
        Test.case
          "prepare_snapshot store hydration emits structured events"
          test_prepare_snapshot_store_hydration_emits_structured_events;
        Test.case
          "prepare_snapshot missing requirements emit structured events"
          test_prepare_snapshot_missing_requirements_emit_structured_events;
        Test.case "prepare_snapshot reports match coverage diagnostics" test_prepare_snapshot_reports_match_coverage_diagnostics;
        Test.case "prepare_snapshot includes interface sibling dependencies" test_prepare_snapshot_includes_interface_sibling_dependencies;
        Test.case "loaded module typings override store" test_loaded_module_typings_override_store;
        Test.case "snapshot uses sibling source record types" test_snapshot_uses_sibling_source_record_types;
        Test.case "snapshot uses loaded module record types" test_snapshot_uses_loaded_module_record_types;
        Test.case "include reexports loaded module record types" test_include_reexports_loaded_module_record_types;
        Test.case "module alias reexports loaded module record types" test_module_alias_reexports_loaded_module_record_types;
        Test.case "include reexports loaded module typings" test_include_reexports_loaded_module_typings;
        Test.case "include module type of canonicalizes loaded nominal types" test_include_module_type_of_canonicalizes_loaded_nominal_types;
        Test.case "include module type of loaded modules canonicalizes nominal types" test_include_module_type_of_loaded_modules_canonicalizes_nominal_types;
        Test.case
          "loaded module reexports canonicalize dependency result aliases"
          test_loaded_module_reexports_canonicalize_dependency_result_aliases;
        Test.case
          "include module type of stdlib float uses bootstrap module typings"
          test_include_module_type_of_stdlib_float_uses_bootstrap_module_typings;
        Test.case
          "include module type of ocaml stdlib hashtbl uses loaded module typings"
          test_include_module_type_of_ocaml_stdlib_hashtbl_uses_loaded_module_typings;
        Test.case
          "ocaml stdlib root operators typecheck"
          test_ocaml_stdlib_root_operators_typecheck;
        Test.case
          "ocaml stdlib sys signal behavior typechecks"
          test_ocaml_stdlib_sys_signal_behavior_typechecks;
        Test.case
          "ocaml unix stats errors and commands typecheck"
          test_ocaml_unix_stats_errors_and_commands_typecheck;
        Test.case
          "ocaml unix socket and addr_info typecheck"
          test_ocaml_unix_socket_and_addr_info_typecheck;
        Test.case
          "ocaml unix terminal process and labels typecheck"
          test_ocaml_unix_terminal_process_and_labels_typecheck;
        Test.case "source analysis with loaded modules canonicalizes nominal types" test_source_analysis_with_loaded_modules_canonicalizes_nominal_types;
        Test.case "source analysis with opened loaded module canonicalizes nominal types" test_source_analysis_with_opened_loaded_module_canonicalizes_nominal_types;
        Test.case "snapshot exports opened loaded nominal types from implementation" test_snapshot_exports_opened_loaded_nominal_types_from_implementation;
        Test.case "snapshot type declarations use opened sibling nominal types" test_snapshot_type_decls_use_opened_sibling_nominal_types;
        Test.case "prepare_snapshot type declarations use opened sibling nominal types" test_prepare_snapshot_type_decls_use_opened_sibling_nominal_types;
        Test.case "prepare_snapshot type declarations use opened loaded nominal types" test_prepare_snapshot_type_decls_use_opened_loaded_nominal_types;
        Test.case
          "prepare_snapshot type declarations use opened loaded nominal types with underscored module name"
          test_prepare_snapshot_type_decls_use_opened_loaded_nominal_types_with_underscored_module_name;
        Test.case
          "prepare_snapshot canonicalizes sibling structural polyvariant exports"
          test_prepare_snapshot_polyvariant_exports_canonicalize_sibling_structural_types;
        Test.case
          "prepare_snapshot canonicalizes sibling structural polyvariant exports inside arrows"
          test_prepare_snapshot_polyvariant_exports_canonicalize_sibling_structural_types_inside_arrows;
        Test.case "module aliases reexport loaded module typings" test_module_alias_reexports_loaded_module_typings;
        Test.case
          "loaded module alias chains preserve interface declared values"
          test_loaded_module_alias_chain_preserves_interface_declared_values;
        Test.case "module aliases reexport same-named local modules" test_module_alias_reexports_same_named_local_modules;
        Test.case "loaded module typings preserve nested same-named alias exports" test_loaded_module_typings_preserve_nested_same_named_alias_exports;
        Test.case
          "paired loaded module typings preserve nested alias exports across include chains"
          test_paired_loaded_module_typings_preserve_nested_alias_exports_across_include_chain;
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
        Test.case "prepare_snapshot canonicalizes missing requirements" test_prepare_snapshot_canonicalizes_missing_requirements;
        Test.case "prepare_snapshot sorts missing modules by name" test_prepare_snapshot_sorts_missing_modules_by_name;
        Test.case "check_source recovers when rooted preparation reports missing module summaries" test_check_source_recovers_when_snapshot_preparation_reports_missing_module_summaries;
        Test.case "match guards typecheck in pattern scope" test_match_guards_typecheck_in_pattern_scope;
        Test.case "optional arguments can be omitted and reordered" test_optional_arguments_can_be_omitted_and_reordered;
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
