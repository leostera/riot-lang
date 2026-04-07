open Std
open Typ
open Typ.Analysis
open Typ.Infer
open Typ.Model

module Std_env = Std.Env

let int_to_string_scheme =
  TypeScheme.of_type
    (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.int ~rhs:TypeRepr.string)

let rgb_blend_scheme =
  TypeScheme.of_type
    (TypeRepr.arrow
      ~label:TypeRepr.Nolabel
      ~lhs:TypeRepr.int
      ~rhs:
        (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.int ~rhs:TypeRepr.int))

let make_ident =
  let next_local_id = ref 0 in
  fun name ->
    let local_id = !next_local_id in
    let () = next_local_id := local_id + 1 in
    Env.Binding.make_ident ~local_id ~name

let binding_path = fun binding ->
  Env.Binding.path binding |> IdentPath.to_string

let binding_paths = fun bindings ->
  bindings |> List.map binding_path |> List.sort String.compare

let type_decl_paths = fun type_decls ->
  type_decls |> List.map
    (fun (type_decl: FileSummary.type_decl) ->
      let scope =
        if IdentPath.is_empty type_decl.scope_path then
          type_decl.declaration.type_name
        else
          IdentPath.append_name type_decl.scope_path type_decl.declaration.type_name
          |> IdentPath.to_string
      in
      scope)
  |> List.sort String.compare

let lookup_binding_path = fun lookup env path ->
  lookup env (IdentPath.of_string path) |> Option.map binding_path

let lookup_binding_name = fun lookup env path ->
  lookup env (IdentPath.of_string path) |> Option.map Env.Binding.name

let nested_shade_type_decl = {
  FileSummary.scope_path = IdentPath.of_name "Colors";
  declaration = {
    TypeDecl.type_constructor_id = TypeConstructorId.of_int (-9000);
    type_name = "shade";
    param_ids = [];
    param_variances = [];
    constructors = [];
    labels = [];
    manifest = None;
  };
}

let make_env = fun () ->
  Env.bind
    (Env.of_entries
      ~make_ident
      ~provenance:Env.Binding.Ambient
      [
        (IdentPath.of_string "Colors.to_string", int_to_string_scheme);
        (IdentPath.of_string "Colors.RGB.blend", rgb_blend_scheme);
      ])
    (Env.of_type_decls [ nested_shade_type_decl ])
  |> fun env -> Env.with_local_open env (IdentPath.of_name "Colors")

let alpha_scheme =
  TypeScheme.of_type
    (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.int ~rhs:TypeRepr.int)

let beta_scheme =
  TypeScheme.of_type
    (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.int ~rhs:TypeRepr.bool)

let infer_exports = fun source_text ->
  let source_id = SourceId.of_int 0 in
  let filename = Path.v "env2_fixture.ml" in
  let origin = Source.Path filename in
  let parse_result = Syn.parse ~filename source_text in
  let cst = Syn.build_cst parse_result in
  let source = Source.make_prepared
    ~source_id
    ~kind:Source.File
    ~origin
    ~revision:0
    ~source_hash:(Source.hash_text ~kind:Source.File ~origin ~text:source_text)
    ~parse_result
    ~cst
  in
  match cst with
  | Ok cst ->
      let semantic_tree = Lower.lower_source_file ~source cst in
      let inferred = Infer.infer_file ~config:Config.default semantic_tree in
      inferred.exports |> List.map fst
  | Error _ -> []

let test_summary2_roundtrip = fun _ctx ->
  let env = make_env () in
  let roundtripped = Env.env_of_summary (Env.summary_snapshot env) in
  let expected_bindings = binding_paths (Env.bindings env) in
  let actual_bindings = binding_paths (Env.bindings roundtripped) in
  let expected_types = type_decl_paths (Env.type_decls env) in
  let actual_types = type_decl_paths (Env.type_decls roundtripped) in
  if not (expected_bindings = actual_bindings) then
    Error ("summary2 roundtrip changed bindings: expected ["
    ^ String.concat ", " expected_bindings
    ^ "] but got ["
    ^ String.concat ", " actual_bindings
    ^ "]")
  else if not (expected_types = actual_types) then
    Error ("summary2 roundtrip changed type decls: expected ["
    ^ String.concat ", " expected_types
    ^ "] but got ["
    ^ String.concat ", " actual_types
    ^ "]")
  else
    Ok ()

let test_env_replay_matches_lookup = fun _ctx ->
  let env = make_env () in
  let replayed = Env.env_of_summary (Env.summary_snapshot env) in
  let expected_to_string = lookup_binding_path Env.lookup env "to_string" in
  let actual_to_string = lookup_binding_path Env.lookup replayed "to_string" in
  let expected_blend = lookup_binding_name Env.lookup env "RGB.blend" in
  let actual_blend = lookup_binding_name Env.lookup replayed "RGB.blend" in
  let replayed_shade =
    Env.lookup_type replayed (IdentPath.of_string "Colors.shade")
    |> Option.map (fun (type_decl: FileSummary.type_decl) ->
      (IdentPath.to_string type_decl.scope_path, type_decl.declaration.type_name))
  in
  if not (expected_to_string = actual_to_string) then
    Error ("env2 lookup mismatch for to_string: expected "
    ^ Option.unwrap_or ~default:"<none>" expected_to_string
    ^ " but got "
    ^ Option.unwrap_or ~default:"<none>" actual_to_string)
  else if not (expected_blend = actual_blend) then
    Error ("env2 lookup mismatch for RGB.blend name: expected "
    ^ Option.unwrap_or ~default:"<none>" expected_blend
    ^ " but got "
    ^ Option.unwrap_or ~default:"<none>" actual_blend)
  else if not (replayed_shade = Some ("Colors", "shade")) then
    Error "expected Env2 to replay Colors.shade type decl"
  else
    Ok ()

let test_bind_in_scope_keeps_local_module_names = fun _ctx ->
  let env =
    Env.empty
    |> fun env ->
    Env.bind_in_scope env ~scope_path:(IdentPath.of_name "Helpers")
      (Env.singleton
        ~make_ident
        ~name:"id"
        ~scheme:alpha_scheme
        ~provenance:Env.Binding.Ambient)
    |> fun env ->
    Env.bind_in_scope env ~scope_path:(IdentPath.of_name "Helpers")
      (Env.singleton
        ~make_ident
        ~name:"wrap"
        ~scheme:beta_scheme
        ~provenance:Env.Binding.Ambient)
  in
  let actual = Env.names env in
  let expected = [ "Helpers.id"; "Helpers.wrap" ] in
  if actual = expected then
    Ok ()
  else
    Error ("expected scoped bindings ["
    ^ String.concat ", " expected
    ^ "] but got ["
    ^ String.concat ", " actual
    ^ "]")

let test_include_entries_strip_module_prefix_once = fun _ctx ->
  let env =
    Env.empty
    |> fun env ->
    Env.bind_in_scope env ~scope_path:(IdentPath.of_name "Helpers")
      (Env.singleton
        ~make_ident
        ~name:"id"
        ~scheme:alpha_scheme
        ~provenance:Env.Binding.Ambient)
    |> fun env ->
    Env.bind_in_scope env ~scope_path:(IdentPath.of_name "Helpers")
      (Env.singleton
        ~make_ident
        ~name:"wrap"
        ~scheme:beta_scheme
        ~provenance:Env.Binding.Ambient)
  in
  let actual =
    Env.entries_for_include env (IdentPath.of_name "Helpers")
    |> Env.names
  in
  let expected = [ "id"; "wrap" ] in
  if actual = expected then
    Ok ()
  else
    Error ("expected include entries ["
    ^ String.concat ", " expected
    ^ "] but got ["
    ^ String.concat ", " actual
    ^ "]")

let test_module_alias_entries_prefix_once = fun _ctx ->
  let env =
    Env.empty
    |> fun env ->
    Env.bind_in_scope env ~scope_path:(IdentPath.of_name "Helpers")
      (Env.singleton
        ~make_ident
        ~name:"id"
        ~scheme:alpha_scheme
        ~provenance:Env.Binding.Ambient)
    |> fun env ->
    Env.bind_in_scope env ~scope_path:(IdentPath.of_name "Helpers")
      (Env.singleton
        ~make_ident
        ~name:"wrap"
        ~scheme:beta_scheme
        ~provenance:Env.Binding.Ambient)
  in
  let actual =
    Env.entries_for_module_alias env ~alias_name:"Util" ~module_path:(IdentPath.of_name "Helpers")
    |> Env.names
  in
  let expected = [ "Util.id"; "Util.wrap" ] in
  if actual = expected then
    Ok ()
  else
    Error ("expected alias entries ["
    ^ String.concat ", " expected
    ^ "] but got ["
    ^ String.concat ", " actual
    ^ "]")

let test_item_scope_replay_keeps_module_paths_stable = fun _ctx ->
  let scope_path = IdentPath.of_name "Helpers" in
  let introduced_id =
    Env.singleton
      ~make_ident
      ~name:"id"
      ~scheme:alpha_scheme
      ~provenance:Env.Binding.Ambient
  in
  let export_state = Env.bind_in_scope Env.empty ~scope_path introduced_id in
  let scope = Env.register_entries Env.empty_item_scope ~scope_path introduced_id in
  let item_env = Env.for_item_scope export_state scope ~scope_path in
  let env_after_item =
    Env.extend item_env
      [
        Env.Binding.make
          ~ident:(make_ident "wrap")
          ~path:(IdentPath.of_name "wrap")
          ~scheme:beta_scheme
          ~provenance:Env.Binding.Ambient;
      ]
  in
  let introduced_wrap = Env.introduced_entries item_env env_after_item in
  let final_env = Env.bind_in_scope export_state ~scope_path introduced_wrap in
  let actual = Env.names final_env in
  let expected = [ "Helpers.id"; "Helpers.wrap" ] in
  if actual = expected then
    Ok ()
  else
    Error ("expected replayed scoped env ["
    ^ String.concat ", " expected
    ^ "] but got ["
    ^ String.concat ", " actual
    ^ "]")

let test_export_render_keeps_nested_module_paths_stable = fun _ctx ->
  let env =
    Env.empty
    |> fun env ->
    Env.bind_in_scope env ~scope_path:(IdentPath.of_name "Helpers")
      (Env.singleton
        ~make_ident
        ~name:"id"
        ~scheme:alpha_scheme
        ~provenance:Env.Binding.Ambient)
    |> fun env ->
    Env.bind_in_scope env ~scope_path:(IdentPath.of_name "Helpers")
      (Env.singleton
        ~make_ident
        ~name:"wrap"
        ~scheme:beta_scheme
        ~provenance:Env.Binding.Ambient)
  in
  let config = {
    Config.default with
    prelude = [];
    loaded_modules = [];
    ambient = [];
    ambient_type_decls = [];
  } in
  let actual =
    Env.export config env
    |> Env.render
    |> List.map fst
  in
  let expected = [ "Helpers.id"; "Helpers.wrap" ] in
  if actual = expected then
    Ok ()
  else
    Error ("expected exported bindings ["
    ^ String.concat ", " expected
    ^ "] but got ["
    ^ String.concat ", " actual
    ^ "]")

let test_direct_infer_keeps_include_module_paths_stable = fun _ctx ->
  let actual = infer_exports
    "module Helpers = struct\n  let id x = x\n  let wrap value = Some value\nend\n\ninclude Helpers\n\nlet answer = wrap (id 1)\n" in
  let expected = [ "answer"; "id"; "wrap"; "Helpers.id"; "Helpers.wrap" ] in
  if actual = expected then
    Ok ()
  else
    Error ("expected direct infer exports ["
    ^ String.concat ", " expected
    ^ "] but got ["
    ^ String.concat ", " actual
    ^ "]")

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests = [
        Test.case "summary2 roundtrips env summaries" test_summary2_roundtrip;
        Test.case "env replay matches nested module lookups" test_env_replay_matches_lookup;
        Test.case "bind_in_scope keeps local module names" test_bind_in_scope_keeps_local_module_names;
        Test.case "include entries strip module prefix once" test_include_entries_strip_module_prefix_once;
        Test.case "module alias entries prefix once" test_module_alias_entries_prefix_once;
        Test.case "item scope replay keeps module paths stable"
          test_item_scope_replay_keeps_module_paths_stable;
        Test.case "export render keeps nested module paths stable"
          test_export_render_keeps_nested_module_paths_stable;
        Test.case "direct infer keeps include module paths stable"
          test_direct_infer_keeps_include_module_paths_stable;
      ] in
      Test.Cli.main ~name:"typ:env2" ~tests ~args)
    ~args:Std_env.args
    ()
