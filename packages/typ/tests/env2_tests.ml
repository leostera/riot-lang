open Std
open Typ
open Typ.Analysis
open Typ.Infer
open Typ.Model
module Std_env = Std.Env

let int_to_string_scheme = TypeScheme.of_type
  (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.int ~rhs:TypeRepr.string)

let rgb_blend_scheme = TypeScheme.of_type
  (TypeRepr.arrow
    ~label:TypeRepr.Nolabel
    ~lhs:TypeRepr.int
    ~rhs:(TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.int ~rhs:TypeRepr.int))

let make_id =
  let next_local_id = ref 0 in
  fun path ->
    let local_id = !next_local_id in
    (next_local_id := local_id + 1);
    BindingId.local ~stamp:local_id ~name:(SurfacePath.to_string path)

let binding_path = fun binding -> Env.Binding.surface_path binding |> SurfacePath.to_string

let binding_paths = fun bindings -> bindings |> List.map binding_path |> List.sort String.compare

let type_decl_paths = fun type_decls ->
  type_decls |> List.map
    (fun (type_decl: FileSummary.type_decl) ->
      if SurfacePath.is_empty type_decl.scope_path then
        type_decl.declaration.type_name
      else
        SurfacePath.append_name type_decl.scope_path type_decl.declaration.type_name |> SurfacePath.to_string) |> List.sort
    String.compare

let lookup_binding_path = fun lookup env path ->
  lookup env (EntityId.of_string path) |> Option.map binding_path

let lookup_binding_name = fun lookup env path ->
  lookup env (EntityId.of_string path) |> Option.map Env.Binding.name

let nested_shade_type_decl = {
  FileSummary.scope_path = SurfacePath.of_name "Colors";
  declaration =
    {
      TypeDecl.type_constructor_id = TypeConstructorId.make ~owner:"env2-test" ~local_id:(-9_000);
      type_name = "shade";
      nonrec_ = false;
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
      ~make_id
      ~provenance:Env.Binding.Ambient [
        (SurfacePath.of_string "Colors.to_string", int_to_string_scheme);
        (SurfacePath.of_string "Colors.RGB.blend", rgb_blend_scheme);
      ])
    (Env.of_type_decls [ nested_shade_type_decl ])
  |> fun env ->
    Env.with_local_open env (SurfacePath.of_name "Colors")

let alpha_scheme = TypeScheme.of_type
  (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.int ~rhs:TypeRepr.int)

let beta_scheme = TypeScheme.of_type
  (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.int ~rhs:TypeRepr.bool)

let join_names = fun names ->
  String.concat ", " names

let show_optional_name = fun value -> Option.unwrap_or ~default:"<none>" value

let expected_names_error = fun label expected actual ->
  Error (format
    Format.[
      str label;
      str " [";
      str (join_names expected);
      str "] but got [";
      str (join_names actual);
      str "]";
    ])

let expected_optional_error = fun label expected actual ->
  Error (format
    Format.[
      str label;
      str " ";
      str (show_optional_name expected);
      str " but got ";
      str (show_optional_name actual);
    ])

let infer_exports = fun source_text ->
  let source_id = SourceId.of_int 0 in
  let filename = Path.v "env2_fixture.ml" in
  let origin = Source.Path filename in
  let parse_result = Syn.parse ~filename source_text in
  let cst =
    match Syn.build_cst parse_result with
    | Ok cst -> cst
    | Error (Syn.Parse_diagnostics diagnostics) -> panic
      (format
        Format.[
          str "expected successful CST for env2_fixture.ml but parser reported diagnostics: ";
          str (String.concat "; " (List.map Syn.Diagnostic.to_string diagnostics));
        ])
    | Error (Syn.Cst_builder_error error) -> panic
      (format
        Format.[
          str "expected successful CST for env2_fixture.ml but CST build failed: ";
          str error.message;
        ])
  in
  let implicit_opens = [] in
  let source = Source.make_prepared
    ~source_id
    ~kind:Source.File
    ~module_name:(Source.infer_module_name origin)
    ~implicit_opens
    ~origin
    ~revision:0
    ~source_hash:(Source.hash ~implicit_opens ~cst)
    ~parse_result
    ~cst in
  let semantic_tree = Lower.lower_source_file ~source cst in
  let inferred = Infer.infer_file ~config:Config.default ~source semantic_tree in
  inferred.exports |> List.map (fun (name, _scheme) -> SurfacePath.to_string name)

let infer_export_scheme = fun source_text name ->
  let source_id = SourceId.of_int 0 in
  let filename = Path.v "env2_fixture.ml" in
  let origin = Source.Path filename in
  let parse_result = Syn.parse ~filename source_text in
  let cst =
    match Syn.build_cst parse_result with
    | Ok cst -> cst
    | Error (Syn.Parse_diagnostics diagnostics) -> panic
      (format
        Format.[
          str "expected successful CST for env2_fixture.ml but parser reported diagnostics: ";
          str (String.concat "; " (List.map Syn.Diagnostic.to_string diagnostics));
        ])
    | Error (Syn.Cst_builder_error error) -> panic
      (format
        Format.[
          str "expected successful CST for env2_fixture.ml but CST build failed: ";
          str error.message;
        ])
  in
  let implicit_opens = [] in
  let source = Source.make_prepared
    ~source_id
    ~kind:Source.File
    ~module_name:(Source.infer_module_name origin)
    ~implicit_opens
    ~origin
    ~revision:0
    ~source_hash:(Source.hash ~implicit_opens ~cst)
    ~parse_result
    ~cst in
  let semantic_tree = Lower.lower_source_file ~source cst in
  let inferred = Infer.infer_file ~config:Config.default ~source semantic_tree in
  inferred.exports |> List.find_map
    (fun (export_name, scheme) ->
      if SurfacePath.equal export_name (SurfacePath.of_string name) then
        Some (TypePrinter.scheme_to_string scheme)
      else
        None)

let test_summary2_roundtrip = fun _ctx ->
  let env = make_env () in
  let roundtripped = Env.env_of_summary (Env.summary_snapshot env) in
  let expected_bindings = binding_paths (Env.bindings env) in
  let actual_bindings = binding_paths (Env.bindings roundtripped) in
  let expected_types = type_decl_paths (Env.type_decls env) in
  let actual_types = type_decl_paths (Env.type_decls roundtripped) in
  if not (expected_bindings = actual_bindings) then
    expected_names_error "summary2 roundtrip changed bindings: expected" expected_bindings actual_bindings
  else if not (expected_types = actual_types) then
    expected_names_error "summary2 roundtrip changed type decls: expected" expected_types actual_types
  else
    Ok ()

let test_env_replay_matches_lookup = fun _ctx ->
  let env = make_env () in
  let replayed = Env.env_of_summary (Env.summary_snapshot env) in
  let expected_to_string = lookup_binding_path Env.lookup env "to_string" in
  let actual_to_string = lookup_binding_path Env.lookup replayed "to_string" in
  let expected_blend = lookup_binding_name Env.lookup env "RGB.blend" in
  let actual_blend = lookup_binding_name Env.lookup replayed "RGB.blend" in
  let replayed_shade = Env.lookup_type replayed (SurfacePath.of_string "Colors.shade")
  |> Option.map
    (fun (type_decl: FileSummary.type_decl) ->
      (SurfacePath.to_string type_decl.scope_path, type_decl.declaration.type_name)) in
  if not (expected_to_string = actual_to_string) then
    expected_optional_error "env2 lookup mismatch for to_string: expected" expected_to_string actual_to_string
  else if not (expected_blend = actual_blend) then
    expected_optional_error "env2 lookup mismatch for RGB.blend name: expected" expected_blend actual_blend
  else if not (replayed_shade = Some ("Colors", "shade")) then
    Error "expected Env2 to replay Colors.shade type decl"
  else
    Ok ()

let test_builtin_type_constructors_only_expose_syntax_backed_names = fun _ctx ->
  let forbidden = [
    "result";
    "option";
    "bytes";
    "int32";
    "int64";
    "nativeint";
    "lazy_t";
    "ref";
    "in_channel";
    "out_channel";
  ]
  in
  match List.find_opt
    (fun name -> Option.is_some (BuiltinTypeConstructors.head_of_path (SurfacePath.of_name name)))
    forbidden with
  | None -> Ok ()
  | Some name -> Error (format
    Format.[ str "expected bare "; str name; str " to require an explicit dependency" ])

let test_bind_in_scope_keeps_local_module_names = fun _ctx ->
  let env = Env.empty
  |> fun env ->
    Env.bind_in_scope
      env
      ~scope_path:(SurfacePath.of_name "Helpers")
      (Env.singleton ~make_id ~name:"id" ~scheme:alpha_scheme ~provenance:Env.Binding.Ambient)
    |> fun env ->
      Env.bind_in_scope
        env
        ~scope_path:(SurfacePath.of_name "Helpers")
        (Env.singleton ~make_id ~name:"wrap" ~scheme:beta_scheme ~provenance:Env.Binding.Ambient) in
  let actual = Env.names env in
  let expected = [ "Helpers.id"; "Helpers.wrap" ] in
  if actual = expected then
    Ok ()
  else
    expected_names_error "expected scoped bindings" expected actual

let test_include_entries_strip_module_prefix_once = fun _ctx ->
  let env = Env.empty
  |> fun env ->
    Env.bind_in_scope
      env
      ~scope_path:(SurfacePath.of_name "Helpers")
      (Env.singleton ~make_id ~name:"id" ~scheme:alpha_scheme ~provenance:Env.Binding.Ambient)
    |> fun env ->
      Env.bind_in_scope
        env
        ~scope_path:(SurfacePath.of_name "Helpers")
        (Env.singleton ~make_id ~name:"wrap" ~scheme:beta_scheme ~provenance:Env.Binding.Ambient) in
  let actual = Env.entries_for_include env (SurfacePath.of_name "Helpers") |> Env.names in
  let expected = [ "id"; "wrap" ] in
  if actual = expected then
    Ok ()
  else
    expected_names_error "expected include entries" expected actual

let test_module_alias_entries_prefix_once = fun _ctx ->
  let env = Env.empty
  |> fun env ->
    Env.bind_in_scope
      env
      ~scope_path:(SurfacePath.of_name "Helpers")
      (Env.singleton ~make_id ~name:"id" ~scheme:alpha_scheme ~provenance:Env.Binding.Ambient)
    |> fun env ->
      Env.bind_in_scope
        env
        ~scope_path:(SurfacePath.of_name "Helpers")
        (Env.singleton ~make_id ~name:"wrap" ~scheme:beta_scheme ~provenance:Env.Binding.Ambient) in
  let actual = Env.entries_for_module_alias
    env
    ~alias_name:"Util"
    ~module_path:(SurfacePath.of_name "Helpers")
  |> Env.names in
  let expected = [ "Util.id"; "Util.wrap" ] in
  if actual = expected then
    Ok ()
  else
    expected_names_error "expected alias entries" expected actual

let test_item_scope_replay_keeps_module_paths_stable = fun _ctx ->
  let scope_path = SurfacePath.of_name "Helpers" in
  let introduced_id = Env.singleton
    ~make_id
    ~name:"id"
    ~scheme:alpha_scheme
    ~provenance:Env.Binding.Ambient in
  let export_state = Env.bind_in_scope Env.empty ~scope_path introduced_id in
  let scope = Env.register_entries Env.empty_item_scope ~scope_path introduced_id in
  let item_env = Env.for_item_scope export_state scope ~scope_path in
  let env_after_item = Env.extend
    item_env
    [
      Env.Binding.make
        ~id:(make_id (SurfacePath.of_name "wrap"))
        ~surface_path:(SurfacePath.of_name "wrap")
        ~scheme:beta_scheme
        ~provenance:Env.Binding.Ambient;
    ] in
  let introduced_wrap = Env.introduced_entries item_env env_after_item in
  let final_env = Env.bind_in_scope export_state ~scope_path introduced_wrap in
  let actual = Env.names final_env in
  let expected = [ "Helpers.id"; "Helpers.wrap" ] in
  if actual = expected then
    Ok ()
  else
    expected_names_error "expected replayed scoped env" expected actual

let test_export_render_keeps_nested_module_paths_stable = fun _ctx ->
  let env = Env.empty
  |> fun env ->
    Env.bind_in_scope
      env
      ~scope_path:(SurfacePath.of_name "Helpers")
      (Env.singleton ~make_id ~name:"id" ~scheme:alpha_scheme ~provenance:Env.Binding.Ambient)
    |> fun env ->
      Env.bind_in_scope
        env
        ~scope_path:(SurfacePath.of_name "Helpers")
        (Env.singleton ~make_id ~name:"wrap" ~scheme:beta_scheme ~provenance:Env.Binding.Ambient) in
  let config = {
    Config.default
    with prelude = [];
    loaded_modules = [];
    ambient = [];
    ambient_type_decls = []
  } in
  let actual = Env.export config env |> Env.render |> List.map fst in
  let expected = [ "Helpers.id"; "Helpers.wrap" ] in
  if actual = expected then
    Ok ()
  else
    expected_names_error "expected exported bindings" expected actual

let test_direct_infer_keeps_include_module_paths_stable = fun _ctx ->
  let actual = infer_exports "module Helpers = struct\n  let id x = x\n  let wrap value = Some value\nend\n\ninclude Helpers\n\nlet answer = wrap (id 1)\n" in
  let expected = [ "answer"; "id"; "wrap"; "Helpers.id"; "Helpers.wrap" ] in
  if actual = expected then
    Ok ()
  else
    expected_names_error "expected direct infer exports" expected actual

let test_direct_infer_rebinding_replaces_visible_export = fun _ctx ->
  let actual = infer_export_scheme "let value = 1\nlet value = true\n" "value" in
  let expected = Some "bool" in
  if actual = expected then
    Ok ()
  else
    expected_optional_error "expected shadowed export scheme" expected actual

let test_direct_infer_rebinding_replaces_visible_nested_export = fun _ctx ->
  let actual = infer_export_scheme
    "module Helpers = struct\n  let value = 1\n  let value = true\nend\n"
    "Helpers.value" in
  let expected = Some "bool" in
  if actual = expected then
    Ok ()
  else
    expected_optional_error "expected shadowed nested export scheme" expected actual

let test_direct_infer_poly_variant_expression_uses_named_alias = fun _ctx ->
  let actual = infer_export_scheme
    "type rgb = [ `rgb of int * int * int ]\nlet blue = `rgb (0, 0, 255)\n"
    "blue" in
  let expected = Some "rgb" in
  if actual = expected then
    Ok ()
  else
    expected_optional_error "expected polyvariant expression scheme" expected actual

let test_direct_infer_anonymous_poly_variant_expression_keeps_structural_type = fun _ctx ->
  let actual = infer_export_scheme "let blue = `rgb (0, 0, 255)\n" "blue" in
  let expected = Some "[ > `rgb of int * int * int ]" in
  if actual = expected then
    Ok ()
  else
    expected_optional_error "expected anonymous polyvariant expression scheme" expected actual

let test_direct_infer_poly_variant_parameter_uses_named_alias = fun _ctx ->
  let actual = infer_export_scheme
    "type ansi = [ `ansi of int ]\nlet ansi_value = fun (`ansi i) -> i\n"
    "ansi_value" in
  let expected = Some "ansi -> int" in
  if actual = expected then
    Ok ()
  else
    expected_optional_error "expected polyvariant parameter scheme" expected actual

let test_direct_infer_poly_variant_match_uses_common_named_alias = fun _ctx ->
  let actual = infer_export_scheme
    {ocaml|
type ansi = [ `ansi of int ]
type rgb = [ `rgb of int * int * int ]
type color = [ ansi | rgb ]
let first_channel = fun value ->
  match value with
  | `ansi i -> i
  | `rgb (r, _, _) -> r
|ocaml}
    "first_channel"
  in
  let expected = Some "color -> int" in
  if actual = expected then
    Ok ()
  else
    expected_optional_error "expected polyvariant match scheme" expected actual

let test_direct_infer_poly_variant_match_prefers_widest_visible_alias = fun _ctx ->
  let actual = infer_export_scheme
    {ocaml|
type ansi = [ `ansi of int ]
type rgb = [ `rgb of int * int * int ]
type lrgb = [ `lrgb of float * float * float ]
type color = [ ansi | rgb ]
let classify = fun value ->
  match value with
  | `ansi _ -> 0
  | `rgb _ -> 1
  | `lrgb _ -> 2
|ocaml}
    "classify"
  in
  let expected = Some "color -> int" in
  if actual = expected then
    Ok ()
  else
    expected_optional_error "expected polyvariant widest-alias scheme" expected actual

let test_direct_infer_explicit_poly_variant_coercion_uses_target_alias = fun _ctx ->
  let actual = infer_export_scheme
    {ocaml|
type ansi = [ `ansi of int ]
type rgb = [ `rgb of int * int * int ]
type color = [ ansi | rgb ]
let midpoint = `rgb (0, 0, 255)
let as_color = (midpoint :> color)
|ocaml}
    "as_color"
  in
  let expected = Some "color" in
  if actual = expected then
    Ok ()
  else
    expected_optional_error "expected explicit polyvariant coercion scheme" expected actual

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests = [
        Test.case "summary2 roundtrips env summaries" test_summary2_roundtrip;
        Test.case "env replay matches nested module lookups" test_env_replay_matches_lookup;
        Test.case "builtin type constructors only expose syntax backed names" test_builtin_type_constructors_only_expose_syntax_backed_names;
        Test.case "bind_in_scope keeps local module names" test_bind_in_scope_keeps_local_module_names;
        Test.case "include entries strip module prefix once" test_include_entries_strip_module_prefix_once;
        Test.case "module alias entries prefix once" test_module_alias_entries_prefix_once;
        Test.case "item scope replay keeps module paths stable" test_item_scope_replay_keeps_module_paths_stable;
        Test.case "export render keeps nested module paths stable" test_export_render_keeps_nested_module_paths_stable;
        Test.case "direct infer keeps include module paths stable" test_direct_infer_keeps_include_module_paths_stable;
        Test.case "direct infer rebinding replaces visible export" test_direct_infer_rebinding_replaces_visible_export;
        Test.case "direct infer rebinding replaces visible nested export" test_direct_infer_rebinding_replaces_visible_nested_export;
        Test.case "direct infer polyvariant expression uses named alias" test_direct_infer_poly_variant_expression_uses_named_alias;
        Test.case "direct infer anonymous polyvariant expression keeps structural type" test_direct_infer_anonymous_poly_variant_expression_keeps_structural_type;
        Test.case "direct infer polyvariant parameter uses named alias" test_direct_infer_poly_variant_parameter_uses_named_alias;
        Test.case "direct infer polyvariant match uses common named alias" test_direct_infer_poly_variant_match_uses_common_named_alias;
        Test.case "direct infer polyvariant match prefers widest visible alias" test_direct_infer_poly_variant_match_prefers_widest_visible_alias;
        Test.case "direct infer explicit polyvariant coercion uses target alias" test_direct_infer_explicit_poly_variant_coercion_uses_target_alias;
      ]
      in
      Test.Cli.main ~name:"typ:env2" ~tests ~args)
    ~args:Std_env.args
    ()
