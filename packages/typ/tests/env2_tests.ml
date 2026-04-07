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

let make_legacy_env = fun () ->
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

let test_summary2_roundtrip = fun _ctx ->
  let legacy_env = make_legacy_env () in
  let legacy_summary = Env.summary_snapshot legacy_env in
  let roundtripped =
    Summary2.of_legacy_summary legacy_summary
    |> Summary2.to_legacy_summary
    |> Env.env_of_summary
  in
  let expected_bindings = binding_paths (Env.bindings legacy_env) in
  let actual_bindings = binding_paths (Env.bindings roundtripped) in
  let expected_types = type_decl_paths (Env.type_decls legacy_env) in
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

let test_env2_replay_matches_legacy_lookup = fun _ctx ->
  let legacy_env = make_legacy_env () in
  let env2 = Env2.env_of_legacy_summary (Env.summary_snapshot legacy_env) in
  let expected_to_string = lookup_binding_path Env.lookup legacy_env "to_string" in
  let actual_to_string = lookup_binding_path Env2.lookup env2 "to_string" in
  let expected_blend = lookup_binding_name Env.lookup legacy_env "RGB.blend" in
  let actual_blend = lookup_binding_name Env2.lookup env2 "RGB.blend" in
  let env2_shade =
    Env2.lookup_type env2 (IdentPath.of_string "Colors.shade")
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
  else if not (env2_shade = Some ("Colors", "shade")) then
    Error "expected Env2 to replay Colors.shade type decl"
  else
    Ok ()

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests = [
        Test.case "summary2 roundtrips legacy env summaries" test_summary2_roundtrip;
        Test.case "env2 replay matches legacy nested module lookups" test_env2_replay_matches_legacy_lookup;
      ] in
      Test.Cli.main ~name:"typ:env2" ~tests ~args)
    ~args:Std_env.args
    ()
