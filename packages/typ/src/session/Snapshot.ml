open Std
open Infer
open Model

type analysis_slot = {
  source_id: SourceId.t;
  source: Source.t;
  config: TypConfig.t;
  mutable base_analysis: SourceAnalysis.t option;
  mutable analysis: SourceAnalysis.t option;
}

module SharedCaches = struct
  type t = {
    analysis_mode_module_result_cache: (string, ModulePairing.t) Collections.HashMap.t;
    analysis_mode_source_cache: (string, SourceAnalysis.t) Collections.HashMap.t;
  }

  let create = fun () ->
    {
      analysis_mode_module_result_cache = Collections.HashMap.with_capacity 128;
      analysis_mode_source_cache = Collections.HashMap.with_capacity 256
    }
end

type t = {
  revision: int;
  roots: SourceId.t list;
  analyses: analysis_slot list;
  qualified_typings_cache: (string, ModuleTypings.t list) Collections.HashMap.t;
  module_results_cache: (string, (string * ModulePairing.t) list) Collections.HashMap.t;
  module_result_cache: (string, ModulePairing.t) Collections.HashMap.t;
  base_module_result_cache: (string, ModulePairing.t) Collections.HashMap.t;
  analysis_mode_module_result_cache: (string, ModulePairing.t) Collections.HashMap.t;
  analysis_mode_source_cache: (string, SourceAnalysis.t) Collections.HashMap.t;
}

let export_status_of_file_summary = fun summary ->
  match summary.FileSummary.export_result with
  | FileSummary.TrustedExport _ -> Event.TrustedExport
  | FileSummary.ErroredExport _ -> Event.ErroredExport
  | FileSummary.NoExport -> Event.MissingExport

let export_status_of_module_typings = fun module_typings ->
  match ModuleTypings.export_result module_typings with
  | FileSummary.TrustedExport _ -> Event.TrustedExport
  | FileSummary.ErroredExport _ -> Event.ErroredExport
  | FileSummary.NoExport -> Event.MissingExport

let signature_mismatch_subject = function
  | Diagnostic.MissingValue { name }
  | Diagnostic.ValueTypeMismatch { name; _ } -> "value " ^ name
  | Diagnostic.MissingTypeDeclaration { name }
  | Diagnostic.TypeDeclarationMismatch { name; _ } -> "type " ^ name

let signature_mismatch_message = fun mismatch -> Diagnostic.signature_mismatch_message mismatch

let make_with_shared_caches = fun ~revision ~roots ~config ~sources ~shared_caches ->
  let analyses =
    sources
    |> List.map
      (fun (source: Source.t) ->
        {
          source_id = source.source_id;
          source;
          config;
          base_analysis = None;
          analysis = None;
        })
  in
  {
    revision;
    roots;
    analyses;
    qualified_typings_cache = Collections.HashMap.with_capacity 8;
    module_results_cache = Collections.HashMap.with_capacity 8;
    module_result_cache = Collections.HashMap.with_capacity 32;
    base_module_result_cache = Collections.HashMap.with_capacity 16;
    analysis_mode_module_result_cache = shared_caches.SharedCaches.analysis_mode_module_result_cache;
    analysis_mode_source_cache = shared_caches.SharedCaches.analysis_mode_source_cache;
  }

let make = fun ~revision ~roots ~config ~sources ->
  make_with_shared_caches ~revision ~roots ~config ~sources ~shared_caches:(SharedCaches.create ())

let type_decl_key = fun (type_decl: FileSummary.type_decl) ->
  IdentPath.append_name type_decl.scope_path type_decl.declaration.type_name

let local_type_decl_index = fun type_decls ->
  let by_path = Collections.HashMap.with_capacity (List.length type_decls) in
  let () =
    List.iter
      (fun (type_decl: FileSummary.type_decl) ->
        let _ = Collections.HashMap.insert by_path (type_decl_key type_decl) type_decl in
        ())
      type_decls
  in
  by_path

let qualify_local_name = fun local_types module_name name ->
  match Collections.HashMap.get local_types name with
  | Some _ -> IdentPath.prepend_name module_name name
  | None -> name

let rec qualify_type = fun local_types module_name ty ->
  let ty = TypeRepr.prune ty in
  match TypeRepr.view ty with
  | TypeRepr.Int
  | TypeRepr.Float
  | TypeRepr.Bool
  | TypeRepr.String
  | TypeRepr.Char
  | TypeRepr.Unit
  | TypeRepr.Hole _
  | TypeRepr.Var _ ->
      ty
  | TypeRepr.Option element ->
      let element' = qualify_type local_types module_name element in
      if Std.Ptr.equal element element' then
        ty
      else
        TypeRepr.option element'
  | TypeRepr.Result (ok_ty, error_ty) ->
      let ok_ty' = qualify_type local_types module_name ok_ty in
      let error_ty' = qualify_type local_types module_name error_ty in
      if Std.Ptr.equal ok_ty ok_ty' && Std.Ptr.equal error_ty error_ty' then
        ty
      else
        TypeRepr.result ok_ty' error_ty'
  | TypeRepr.Array element ->
      let element' = qualify_type local_types module_name element in
      if Std.Ptr.equal element element' then
        ty
      else
        TypeRepr.array element'
  | TypeRepr.List element ->
      let element' = qualify_type local_types module_name element in
      if Std.Ptr.equal element element' then
        ty
      else
        TypeRepr.list element'
  | TypeRepr.Seq element ->
      let element' = qualify_type local_types module_name element in
      if Std.Ptr.equal element element' then
        ty
      else
        TypeRepr.seq element'
  | TypeRepr.Named { head; arguments } ->
      let arguments' = List.map (qualify_type local_types module_name) arguments in
      let head' = { head with name = qualify_local_name local_types module_name head.name } in
      if Std.Ptr.equal head head' && List.for_all2 Std.Ptr.equal arguments arguments' then
        ty
      else
        TypeRepr.named ~head:head' ~arguments:arguments'
  | TypeRepr.PolyVariant { bound; tags; inherited } ->
      let tags' =
        tags
        |> List.map
          (fun (tag: TypeRepr.poly_variant_tag) ->
            match tag.payload_type with
            | Some payload_type ->
                let payload_type' = qualify_type local_types module_name payload_type in
                if Std.Ptr.equal payload_type payload_type' then
                  tag
                else
                  { tag with payload_type = Some payload_type' }
            | None -> tag)
      in
      let inherited' = List.map (qualify_type local_types module_name) inherited in
      if List.for_all2 Std.Ptr.equal tags tags' && List.for_all2 Std.Ptr.equal inherited inherited' then
        ty
      else
        TypeRepr.poly_variant ~bound ~tags:tags' ~inherited:inherited'
  | TypeRepr.Tuple members ->
      let members' = List.map (qualify_type local_types module_name) members in
      if List.for_all2 Std.Ptr.equal members members' then
        ty
      else
        TypeRepr.tuple members'
  | TypeRepr.Arrow { label; lhs; rhs } ->
      let lhs' = qualify_type local_types module_name lhs in
      let rhs' = qualify_type local_types module_name rhs in
      if Std.Ptr.equal lhs lhs' && Std.Ptr.equal rhs rhs' then
        ty
      else
        TypeRepr.arrow ~label ~lhs:lhs' ~rhs:rhs'
  | TypeRepr.Package signature ->
      let values' =
        signature.values
        |> List.map
          (fun (value: TypeRepr.package_value) ->
            let scheme' = qualify_type local_types module_name value.scheme in
            if Std.Ptr.equal value.scheme scheme' then
              value
            else
              { value with scheme = scheme' })
      in
      if List.for_all2 Std.Ptr.equal signature.values values' then
        ty
      else
        TypeRepr.package ~values:values'

let qualify_scheme = fun local_types module_name scheme ->
  let quantified, body = TypeScheme.to_explicit scheme in
  let body' = qualify_type local_types module_name body in
  if Std.Ptr.equal body body' then
    scheme
  else
    TypeScheme.of_explicit ~quantified body'

let qualify_inline_record_labels = fun local_types module_name labels ->
  labels |> List.map
    (fun (label: TypeDecl.label) ->
      let field_type' = qualify_type local_types module_name label.field_type in
      if Std.Ptr.equal label.field_type field_type' then
        label
      else
        { label with field_type = field_type' })

let qualify_exports = fun module_name type_decls exports ->
  let module_path = IdentPath.of_name module_name in
  let local_types = local_type_decl_index type_decls in
  List.map
    (fun (name, scheme) ->
      (
        IdentPath.append_path module_path (IdentPath.of_string name),
        qualify_scheme local_types module_name scheme
      ))
    exports

let qualify_type_decls = fun module_name type_decls ->
  let local_types = local_type_decl_index type_decls in
  List.map
    (fun (type_decl: FileSummary.type_decl) ->
      let declaration = type_decl.declaration in
      let manifest =
        match declaration.manifest with
        | None -> None
        | Some (TypeDecl.Alias manifest_type) -> Some (TypeDecl.Alias (qualify_type
          local_types
          module_name
          manifest_type))
        | Some (TypeDecl.PolyVariant { bound; tags; inherited }) ->
            Some (
              TypeDecl.PolyVariant {
                bound;
                tags =
                  tags |> List.map
                    (fun (tag: TypeDecl.poly_variant_tag) ->
                      match tag.payload_type with
                      | Some payload_type -> {
                        tag
                        with payload_type = Some (qualify_type local_types module_name payload_type)
                      }
                      | None -> tag);
                inherited = List.map (qualify_type local_types module_name) inherited;
              }
            )
      in
      let constructors = declaration.constructors
      |> List.map
        (fun (constructor: TypeDecl.constructor) ->
          {
            constructor
            with scheme = qualify_scheme local_types module_name constructor.scheme;
            inline_record_labels = constructor.inline_record_labels
            |> Option.map (qualify_inline_record_labels local_types module_name)
          }) in
      let labels =
        declaration.labels
        |> List.map
          (fun (label: TypeDecl.label) ->
            let field_type' = qualify_type local_types module_name label.field_type in
            if Std.Ptr.equal label.field_type field_type' then
              label
            else
              { label with field_type = field_type' })
      in
      {
        FileSummary.scope_path = IdentPath.prepend_name module_name type_decl.scope_path;
        declaration = { declaration with manifest; constructors; labels }
      })
    type_decls

let module_names_of_slots = fun slots ->
  let rec loop order seen = function
    | [] -> List.rev order
    | (slot: analysis_slot) :: tail ->
        let module_name = Source.module_name slot.source in
        if List.mem module_name seen then
          loop order seen tail
        else
          loop (module_name :: order) (module_name :: seen) tail
  in
  loop [] [] slots

let slot_of_source_id = fun snapshot source_id ->
  snapshot.analyses |> List.find_opt
    (fun (slot: analysis_slot) ->
      SourceId.equal slot.source_id source_id)

let module_slots = fun snapshot module_name ->
  snapshot.analyses |> List.filter
    (fun (slot: analysis_slot) ->
      String.equal module_name (Source.module_name slot.source))

let rooted_slots = fun snapshot ->
  snapshot.analyses
  |> List.filter
    (fun (slot: analysis_slot) -> snapshot.roots |> List.exists (SourceId.equal slot.source_id))

let rooted_module_names = fun snapshot -> rooted_slots snapshot |> module_names_of_slots

let loaded_ambient_env_for = fun (slot: analysis_slot) ->
  let current_module_name = Source.module_name slot.source in
  slot.config.loaded_modules
  |> List.filter
    (fun typings -> not (String.equal (ModuleTypings.module_name typings) current_module_name))
  |> List.map
    (fun typings ->
      qualify_exports
        (ModuleTypings.module_name typings)
        (ModuleTypings.type_decls typings)
        (ModuleTypings.exports typings))
  |> List.flatten

let loaded_ambient_type_decls_for = fun (slot: analysis_slot) ->
  let current_module_name = Source.module_name slot.source in
  slot.config.loaded_modules
  |> List.filter
    (fun typings -> not (String.equal (ModuleTypings.module_name typings) current_module_name))
  |> List.map
    (fun typings ->
      ModuleTypings.type_decls typings |> qualify_type_decls (ModuleTypings.module_name typings))
  |> List.flatten

let module_dependencies_of_source = fun (source: Source.t) ->
  match Syn.Deps.of_parse_result source.parse_result with
  | Ok deps -> Syn.Deps.modules deps
  | Error _ -> []

let required_local_module_names = fun (slot: analysis_slot) ->
  let names = module_dependencies_of_source slot.source
  @ (slot.source.implicit_opens |> List.map IdentPath.to_string) in
  names |> List.sort_uniq String.compare

let split_internal_module_name = fun module_name ->
  let rec find_separator index =
    if index + 1 >= String.length module_name then
      None
    else if module_name.[index] = '_' && module_name.[index + 1] = '_' then
      Some index
    else
      find_separator (index + 1)
  in
  let rec loop start acc =
    if start >= String.length module_name then
      List.rev acc
    else
      match find_separator start with
      | Some index ->
          let segment = String.sub module_name start (index - start) in
          loop (index + 2) (segment :: acc)
      | None ->
          let segment = String.sub module_name start (String.length module_name - start) in
          List.rev (segment :: acc)
  in
  loop 0 []

let dedupe_preserving_order = fun names ->
  let seen = Collections.HashSet.with_capacity (List.length names + 1) in
  names |> List.filter
    (fun name ->
      if Collections.HashSet.contains seen name then
        false
      else
        let _ = Collections.HashSet.insert seen name in
        true)

let module_name_suffix_aliases = fun module_name ->
  let segments = module_name
  |> String.split_on_char '.'
  |> List.filter (fun segment -> not (String.equal segment "")) in
  let rec loop aliases = function
    | [] -> List.rev aliases
    | _ :: rest as current -> loop (String.concat "." current :: aliases) rest
  in
  loop [] segments |> dedupe_preserving_order

let local_module_aliases_of_internal_module_name = fun module_name ->
  match split_internal_module_name module_name with
  | [] -> []
  | [ _root ] -> []
  | _root :: local_segments -> module_name_suffix_aliases (String.concat "." local_segments)

let preferred_local_module_alias = fun module_name ->
  match local_module_aliases_of_internal_module_name module_name |> List.rev with
  | alias :: _ -> Some alias
  | [] -> None

let ambient_module_names_of_local_module_name = fun module_name ->
  match preferred_local_module_alias module_name with
  | Some alias -> [ alias ]
  | None -> [ module_name ]

let matches_required_local_module = fun ~required_module_name candidate_module_name ->
  String.equal candidate_module_name required_module_name
  || List.mem
    required_module_name
    (local_module_aliases_of_internal_module_name candidate_module_name)

let visiting_key = fun visiting ->
  visiting
  |> List.map SourceId.to_int
  |> List.sort Int.compare
  |> List.map Int.to_string
  |> String.concat ","

let module_result_cache_key = fun visiting module_name -> visiting_key visiting ^ "|" ^ module_name

let loaded_modules_cache_key = fun (snapshot: t) ->
  match snapshot.analyses with
  | [] -> ""
  | slot :: _ -> slot.config.loaded_modules
  |> List.map
    (fun module_typings ->
      ModuleTypings.module_name module_typings
      ^ ":"
      ^ (ModuleTypings.source_hash module_typings |> Crypto.Digest.hex))
  |> List.sort String.compare
  |> String.concat "|"

let shared_cache_namespace = fun (snapshot: t) ->
  let capture_traces =
    match snapshot.analyses with
    | [] -> "notraces"
    | slot :: _ ->
        if slot.config.capture_traces then
          "traces"
        else
          "notraces"
  in
  capture_traces ^ "|" ^ loaded_modules_cache_key snapshot

let module_result_analysis_mode_cache_key = fun (snapshot: t) visiting module_name ->
  let slot_modes =
    module_slots snapshot module_name
    |> List.map
      (fun (slot: analysis_slot) ->
        Int.to_string (SourceId.to_int slot.source_id)
        ^ ":"
        ^ Int.to_string slot.source.revision
        ^ ":"
        ^ if List.exists (SourceId.equal slot.source_id) visiting then
          "base"
        else
          "snapshot")
  in
  shared_cache_namespace snapshot ^ "|" ^ module_name ^ "|" ^ String.concat "," slot_modes

let local_module_names_for = fun (snapshot: t) (slot: analysis_slot) ->
  let current_module_name = Source.module_name slot.source in
  let required_local_modules = required_local_module_names slot in
  snapshot.analyses
  |> module_names_of_slots
  |> List.filter
    (fun candidate_module_name ->
      not (String.equal current_module_name candidate_module_name)
      && List.exists
        (fun required_module_name -> matches_required_local_module ~required_module_name candidate_module_name)
        required_local_modules)

let source_analysis_mode_cache_key = fun (snapshot: t) visiting (slot: analysis_slot) ->
  let local_module_keys = local_module_names_for snapshot slot
  |> List.sort_uniq String.compare
  |> List.map (module_result_analysis_mode_cache_key snapshot visiting) in
  shared_cache_namespace snapshot
  ^ "|"
  ^ Int.to_string (SourceId.to_int slot.source_id)
  ^ ":"
  ^ Int.to_string slot.source.revision
  ^ "|"
  ^ String.concat "," local_module_keys

let has_cached_snapshot_analysis = fun (snapshot: t) visiting (slot: analysis_slot) ->
  Option.is_some slot.analysis
  || Option.is_some
    (Collections.HashMap.get
      snapshot.analysis_mode_source_cache
      (source_analysis_mode_cache_key snapshot visiting slot))

let is_generated_local_module = fun (snapshot: t) module_name ->
  match module_slots snapshot module_name with
  | [] -> false
  | slots ->
      slots |> List.for_all
        (fun (slot: analysis_slot) ->
          match slot.source.kind with
          | Source.Generated -> true
          | Source.File
          | Source.Fragment -> false)

let visiting_module_names = fun (snapshot: t) visiting ->
  visiting
  |> List.filter_map (slot_of_source_id snapshot)
  |> List.map (fun (slot: analysis_slot) -> Source.module_name slot.source)
  |> List.sort_uniq String.compare

let local_module_names_depend_on_visiting = fun (snapshot: t) visiting local_module_names ->
  let visiting_module_names = visiting_module_names snapshot visiting in
  local_module_names |> List.exists
    (fun module_name ->
      List.mem module_name visiting_module_names)

let analysis_depends_on_visiting = fun (snapshot: t) visiting (slot: analysis_slot) ->
  local_module_names_for snapshot slot
  |> List.filter (fun module_name -> not (is_generated_local_module snapshot module_name))
  |> local_module_names_depend_on_visiting snapshot visiting

let module_result_depends_on_visiting = fun (snapshot: t) visiting module_name ->
  module_slots snapshot module_name |> List.exists (analysis_depends_on_visiting snapshot visiting)

let cached_local_module_typings_for = fun (snapshot: t) (slot: analysis_slot) ->
  local_module_names_for snapshot slot
  |> List.filter_map
    (fun module_name ->
      Collections.HashMap.get snapshot.module_result_cache (module_result_cache_key [] module_name)
      |> Option.map
        (fun (result: ModulePairing.t) -> (module_name, result.ModulePairing.module_typings)))

let cached_local_ambient_env_for = fun snapshot (slot: analysis_slot) ->
  cached_local_module_typings_for snapshot slot |> List.map
    (fun (module_name, typings) ->
      let type_decls = ModuleTypings.type_decls typings in
      let exports = ModuleTypings.exports typings in
      ambient_module_names_of_local_module_name module_name
      |> List.map (fun alias -> qualify_exports alias type_decls exports)
      |> List.flatten) |> List.flatten

let cached_local_ambient_type_decls_for = fun snapshot (slot: analysis_slot) ->
  cached_local_module_typings_for snapshot slot |> List.map
    (fun (module_name, typings) ->
      let type_decls = ModuleTypings.type_decls typings in
      ambient_module_names_of_local_module_name module_name
      |> List.map (fun alias -> qualify_type_decls alias type_decls)
      |> List.flatten) |> List.flatten

let force_base_analysis = fun (snapshot: t) (slot: analysis_slot) ->
  match slot.base_analysis with
  | Some analysis -> analysis
  | None ->
      let ambient = cached_local_ambient_env_for snapshot slot @ loaded_ambient_env_for slot in
      let ambient_type_decls = cached_local_ambient_type_decls_for snapshot slot
      @ loaded_ambient_type_decls_for slot in
      let config = slot.config
      |> TypConfig.with_ambient ~ambient
      |> TypConfig.with_ambient_type_decls ~ambient_type_decls in
      let () =
        TypConfig.emit_event slot.config
          (fun () ->
            Event.SourceAnalysisStarted {
              source_id = slot.source_id;
              module_name = Source.module_name slot.source;
              mode = Event.BaseAnalysis;
              loaded_module_count = List.length slot.config.loaded_modules;
              ambient_binding_count = List.length ambient;
              ambient_type_decl_count = List.length ambient_type_decls;
            })
      in
      let analysis = SourceAnalysis.analyze ~config slot.source in
      let () =
        TypConfig.emit_event slot.config
          (fun () ->
            Event.SourceAnalysisFinished {
              source_id = slot.source_id;
              module_name = Source.module_name slot.source;
              mode = Event.BaseAnalysis;
              parse_diagnostic_count = List.length analysis.parse_diagnostics;
              lowering_diagnostic_count = List.length analysis.lowering_diagnostics;
              typing_diagnostic_count = List.length analysis.typing_diagnostics;
              parse_diagnostics = analysis.parse_diagnostics;
              lowering_diagnostics = analysis.lowering_diagnostics;
              typing_diagnostics = analysis.typing_diagnostics;
              export_status = export_status_of_file_summary analysis.file_summary;
              export_count = List.length (FileSummary.exports analysis.file_summary);
              type_decl_count = List.length (FileSummary.type_decls analysis.file_summary);
            })
      in
      let () =
        slot.base_analysis <- Some analysis
      in
      analysis

let rec module_results_for = fun (snapshot: t) visiting ->
  let key = visiting_key visiting in
  match Collections.HashMap.get snapshot.module_results_cache key with
  | Some results -> results
  | None ->
      let results = snapshot.analyses
      |> module_names_of_slots
      |> List.map (fun module_name -> (module_name, module_result_for snapshot visiting module_name)) in
      let _ = Collections.HashMap.insert snapshot.module_results_cache key results in
      results

and qualified_typings = fun (snapshot: t) ?(visiting = []) () ->
  let key = visiting_key visiting in
  match Collections.HashMap.get snapshot.qualified_typings_cache key with
  | Some typings -> typings
  | None ->
      let typings = module_results_for snapshot visiting
      |> List.map (fun (_module_name, result) -> result.ModulePairing.module_typings) in
      let _ = Collections.HashMap.insert snapshot.qualified_typings_cache key typings in
      typings

and ambient_env_for = fun (snapshot: t) visiting (slot: analysis_slot) ->
  let local_modules =
    local_module_names_for snapshot slot
    |> List.map (fun module_name -> (module_name, module_result_for snapshot visiting module_name))
    |> List.map
      (fun (module_name, result) ->
        let type_decls = ModuleTypings.type_decls result.ModulePairing.module_typings in
        let exports = ModuleTypings.exports result.ModulePairing.module_typings in
        ambient_module_names_of_local_module_name module_name
        |> List.map (fun alias -> qualify_exports alias type_decls exports)
        |> List.flatten)
    |> List.flatten
  in
  local_modules @ loaded_ambient_env_for slot

and ambient_type_decls_for = fun (snapshot: t) visiting (slot: analysis_slot) ->
  let local_type_decls =
    local_module_names_for snapshot slot
    |> List.map (fun module_name -> (module_name, module_result_for snapshot visiting module_name))
    |> List.map
      (fun (module_name, result) ->
        let type_decls = ModuleTypings.type_decls result.ModulePairing.module_typings in
        ambient_module_names_of_local_module_name module_name
        |> List.map (fun alias -> qualify_type_decls alias type_decls)
        |> List.flatten)
    |> List.flatten
  in
  local_type_decls @ loaded_ambient_type_decls_for slot

and force_analysis = fun (snapshot: t) ?(visiting = []) (slot: analysis_slot) ->
  match slot.analysis with
  | Some analysis -> analysis
  | None ->
      let cache_result = not (analysis_depends_on_visiting snapshot visiting slot) in
      let analysis_cache_key = source_analysis_mode_cache_key snapshot visiting slot in
      match Collections.HashMap.get snapshot.analysis_mode_source_cache analysis_cache_key with
      | Some analysis ->
          let () =
            if cache_result then
              slot.analysis <- Some analysis
          in
          analysis
      | None ->
          let visiting = slot.source_id :: visiting in
          let ambient = ambient_env_for snapshot visiting slot in
          let ambient_type_decls = ambient_type_decls_for snapshot visiting slot in
          let config = slot.config
          |> TypConfig.with_ambient ~ambient
          |> TypConfig.with_ambient_type_decls ~ambient_type_decls in
          let () =
            TypConfig.emit_event slot.config
              (fun () ->
                Event.SourceAnalysisStarted {
                  source_id = slot.source_id;
                  module_name = Source.module_name slot.source;
                  mode = Event.SnapshotAnalysis;
                  loaded_module_count = List.length slot.config.loaded_modules;
                  ambient_binding_count = List.length ambient;
                  ambient_type_decl_count = List.length ambient_type_decls;
                })
          in
          let analysis = SourceAnalysis.analyze ~config slot.source in
          let () =
            TypConfig.emit_event slot.config
              (fun () ->
                Event.SourceAnalysisFinished {
                  source_id = slot.source_id;
                  module_name = Source.module_name slot.source;
                  mode = Event.SnapshotAnalysis;
                  parse_diagnostic_count = List.length analysis.parse_diagnostics;
                  lowering_diagnostic_count = List.length analysis.lowering_diagnostics;
                  typing_diagnostic_count = List.length analysis.typing_diagnostics;
                  parse_diagnostics = analysis.parse_diagnostics;
                  lowering_diagnostics = analysis.lowering_diagnostics;
                  typing_diagnostics = analysis.typing_diagnostics;
                  export_status = export_status_of_file_summary analysis.file_summary;
                  export_count = List.length (FileSummary.exports analysis.file_summary);
                  type_decl_count = List.length (FileSummary.type_decls analysis.file_summary);
                })
          in
          let _ = Collections.HashMap.insert snapshot.analysis_mode_source_cache analysis_cache_key analysis in
          let () =
            if cache_result then
              slot.analysis <- Some analysis
          in
          analysis

and module_result_for = fun (snapshot: t) visiting module_name ->
  let cache_key = module_result_cache_key visiting module_name in
  let global_cache_key = module_result_cache_key [] module_name in
  let analysis_mode_cache_key = module_result_analysis_mode_cache_key snapshot visiting module_name in
  let cache_result = not (module_result_depends_on_visiting snapshot visiting module_name) in
  let module_is_visiting =
    module_slots snapshot module_name
    |> List.exists
      (fun (slot: analysis_slot) ->
        List.exists (SourceId.equal slot.source_id) visiting)
  in
  match Collections.HashMap.get snapshot.module_result_cache cache_key with
  | Some result -> result
  | None -> (
      match Collections.HashMap.get snapshot.module_result_cache global_cache_key with
      | Some result ->
          let _ = Collections.HashMap.insert snapshot.module_result_cache cache_key result in
          result
      | None -> (
          match Collections.HashMap.get snapshot.analysis_mode_module_result_cache analysis_mode_cache_key with
          | Some result ->
              let _ = Collections.HashMap.insert snapshot.module_result_cache cache_key result in
              result
          | None -> (
              match () with
              | _ when module_is_visiting -> (
                  match Collections.HashMap.get snapshot.base_module_result_cache module_name with
                  | Some result ->
                      let _ = Collections.HashMap.insert snapshot.module_result_cache cache_key result in
                      result
                  | None ->
                      let slots = module_slots snapshot module_name in
                      let source_ids = slots
                      |> List.map (fun (slot: analysis_slot) -> slot.source_id) in
                      let () =
                        match slots with
                        | [] -> ()
                        | slot :: _ -> TypConfig.emit_event
                          slot.config
                          (fun () -> Event.ModulePairingStarted { module_name; source_ids })
                      in
                      let sources, used_base, stable_analyses =
                        slots
                        |> List.fold_left
                          (fun (sources, used_base, stable_analyses) (slot: analysis_slot) ->
                            let analysis, used_base_for_slot, stable_analysis =
                              if List.exists (SourceId.equal slot.source_id) visiting then
                                (force_base_analysis snapshot slot, true, true)
                              else
                                let analysis = force_analysis snapshot ~visiting slot in
                                (
                                  analysis,
                                  false,
                                  has_cached_snapshot_analysis snapshot visiting slot
                                )
                            in
                            (
                              (slot.source, analysis) :: sources,
                              used_base && used_base_for_slot,
                              stable_analyses && stable_analysis
                            ))
                          ([], true, true)
                      in
                      let result = ModulePairing.of_sources ~module_name (List.rev sources) in
                      let _ = Collections.HashMap.insert snapshot.module_result_cache cache_key result in
                      let () =
                        if used_base then
                          ignore
                            (Collections.HashMap.insert
                              snapshot.base_module_result_cache
                              module_name
                              result)
                      in
                      let () =
                        if stable_analyses then
                          ignore
                            (Collections.HashMap.insert
                              snapshot.analysis_mode_module_result_cache
                              analysis_mode_cache_key
                              result)
                      in
                      let () =
                        match slots with
                        | [] -> ()
                        | slot :: _ ->
                            TypConfig.emit_event slot.config
                              (fun () ->
                                Event.ModulePairingFinished {
                                  module_name;
                                  source_ids;
                                  export_status = export_status_of_module_typings
                                    result.ModulePairing.module_typings;
                                  export_count = List.length
                                    (ModuleTypings.exports result.ModulePairing.module_typings);
                                  type_decl_count = List.length
                                    (ModuleTypings.type_decls result.ModulePairing.module_typings);
                                  mismatch_count = List.length result.ModulePairing.signature_mismatches;
                                  mismatch_subjects = result.ModulePairing.signature_mismatches
                                  |> List.map signature_mismatch_subject
                                  |> List.sort_uniq String.compare;
                                  mismatch_messages = result.ModulePairing.signature_mismatches
                                  |> List.map signature_mismatch_message;
                                })
                      in
                      result
                )
              | _ when cache_result ->
                  let slots = module_slots snapshot module_name in
                  let source_ids = slots |> List.map (fun (slot: analysis_slot) -> slot.source_id) in
                  let () =
                    match slots with
                    | [] -> ()
                    | slot :: _ -> TypConfig.emit_event
                      slot.config
                      (fun () -> Event.ModulePairingStarted { module_name; source_ids })
                  in
                  let sources, stable_analyses =
                    slots
                    |> List.fold_left
                      (fun (sources, stable_analyses) (slot: analysis_slot) ->
                        let analysis = force_analysis snapshot ~visiting slot in
                        (
                          (slot.source, analysis) :: sources,
                          stable_analyses && has_cached_snapshot_analysis snapshot visiting slot
                        ))
                      ([], true)
                  in
                  let result = ModulePairing.of_sources ~module_name (List.rev sources) in
                  let _ = Collections.HashMap.insert snapshot.module_result_cache cache_key result in
                  let () = ignore
                    (Collections.HashMap.insert snapshot.module_result_cache global_cache_key result) in
                  let () =
                    if stable_analyses then
                      ignore
                        (Collections.HashMap.insert
                          snapshot.analysis_mode_module_result_cache
                          analysis_mode_cache_key
                          result)
                  in
                  let () =
                    match slots with
                    | [] -> ()
                    | slot :: _ ->
                        TypConfig.emit_event slot.config
                          (fun () ->
                            Event.ModulePairingFinished {
                              module_name;
                              source_ids;
                              export_status = export_status_of_module_typings result.ModulePairing.module_typings;
                              export_count = List.length
                                (ModuleTypings.exports result.ModulePairing.module_typings);
                              type_decl_count = List.length
                                (ModuleTypings.type_decls result.ModulePairing.module_typings);
                              mismatch_count = List.length result.ModulePairing.signature_mismatches;
                              mismatch_subjects = result.ModulePairing.signature_mismatches
                              |> List.map signature_mismatch_subject
                              |> List.sort_uniq String.compare;
                              mismatch_messages = result.ModulePairing.signature_mismatches
                              |> List.map signature_mismatch_message;
                            })
                  in
                  result
              | _ ->
                  let slots = module_slots snapshot module_name in
                  let source_ids = slots |> List.map (fun (slot: analysis_slot) -> slot.source_id) in
                  let () =
                    match slots with
                    | [] -> ()
                    | slot :: _ -> TypConfig.emit_event
                      slot.config
                      (fun () -> Event.ModulePairingStarted { module_name; source_ids })
                  in
                  let sources, reused_cached_analyses, stable_analyses =
                    slots
                    |> List.fold_left
                      (fun (sources, reused_cached_analyses, stable_analyses) (slot: analysis_slot) ->
                        let had_cached_base_analysis = Option.is_some slot.base_analysis in
                        let had_cached_snapshot_analysis = has_cached_snapshot_analysis
                          snapshot
                          visiting
                          slot in
                        let analysis, reused_cached_analysis, stable_analysis =
                          if List.exists (SourceId.equal slot.source_id) visiting then
                            (force_base_analysis snapshot slot, had_cached_base_analysis, true)
                          else
                            let analysis = force_analysis snapshot ~visiting slot in
                            (analysis, had_cached_snapshot_analysis, Option.is_some slot.analysis)
                        in
                        (
                          (slot.source, analysis) :: sources,
                          reused_cached_analyses && reused_cached_analysis,
                          stable_analyses && stable_analysis
                        ))
                      ([], true, true)
                  in
                  let result = ModulePairing.of_sources ~module_name (List.rev sources) in
                  let _ = Collections.HashMap.insert snapshot.module_result_cache cache_key result in
                  let () =
                    if reused_cached_analyses || stable_analyses then
                      ignore
                        (Collections.HashMap.insert snapshot.module_result_cache global_cache_key result)
                  in
                  let () =
                    if stable_analyses then
                      ignore
                        (Collections.HashMap.insert
                          snapshot.analysis_mode_module_result_cache
                          analysis_mode_cache_key
                          result)
                  in
                  let () =
                    match slots with
                    | [] -> ()
                    | slot :: _ ->
                        TypConfig.emit_event slot.config
                          (fun () ->
                            Event.ModulePairingFinished {
                              module_name;
                              source_ids;
                              export_status = export_status_of_module_typings result.ModulePairing.module_typings;
                              export_count = List.length
                                (ModuleTypings.exports result.ModulePairing.module_typings);
                              type_decl_count = List.length
                                (ModuleTypings.type_decls result.ModulePairing.module_typings);
                              mismatch_count = List.length result.ModulePairing.signature_mismatches;
                              mismatch_subjects = result.ModulePairing.signature_mismatches
                              |> List.map signature_mismatch_subject
                              |> List.sort_uniq String.compare;
                              mismatch_messages = result.ModulePairing.signature_mismatches
                              |> List.map signature_mismatch_message;
                            })
                  in
                  result
            )
        )
    )

let revision = fun snapshot -> snapshot.revision

let roots = fun snapshot -> snapshot.roots

let module_result_of_source = fun snapshot source_id ->
  slot_of_source_id snapshot source_id |> Option.map
    (fun (slot: analysis_slot) ->
      let module_name = Source.module_name slot.source in
      (module_name, Some (module_result_for snapshot [] module_name)))

let loaded_module_typings = fun snapshot ->
  match snapshot.analyses with
  | [] -> []
  | slot :: _ -> slot.config.loaded_modules

let is_root = fun snapshot source_id -> snapshot.roots |> List.exists (SourceId.equal source_id)

let analyses = fun snapshot ->
  rooted_slots snapshot |> List.filter_map
    (fun (slot: analysis_slot) ->
      match module_results_for snapshot [] |> List.assoc_opt (Source.module_name slot.source) with
      | Some result -> List.assoc_opt slot.source_id result.ModulePairing.analyses_by_source
      | None -> None)

let file_summaries = fun snapshot ->
  analyses snapshot |> List.map (fun (analysis: SourceAnalysis.t) -> analysis.file_summary)

let module_typings = fun snapshot ->
  let rooted_module_names = rooted_module_names snapshot in
  rooted_module_names
  |> List.filter_map
    (fun module_name ->
      module_results_for snapshot []
      |> List.assoc_opt module_name
      |> Option.map (fun result -> result.ModulePairing.module_typings))

let find_module_typings = fun snapshot source_id ->
  if not (is_root snapshot source_id) then
    None
  else
    match module_result_of_source snapshot source_id with
    | Some (_module_name, Some result) -> Some result.ModulePairing.module_typings
    | Some (_, None)
    | None -> None

let find_module_typings_by_name = fun snapshot module_name ->
  match module_results_for snapshot [] |> List.assoc_opt module_name with
  | Some result -> Some result.ModulePairing.module_typings
  | None ->
      loaded_module_typings snapshot |> List.find_opt
        (fun typings ->
          String.equal module_name (ModuleTypings.module_name typings))

let find_analysis = fun snapshot source_id ->
  if not (is_root snapshot source_id) then
    None
  else
    match module_result_of_source snapshot source_id with
    | Some (_module_name, Some result) -> List.assoc_opt source_id result.ModulePairing.analyses_by_source
    | Some (_, None)
    | None -> None
