open Std
open Std.Data

let ( let* ) = Result.and_then

let fixture_root = Path.v "packages/riot-fix/tests/autofix_fixtures"

let keep_ml = fun path ->
  match Path.extension path with
  | Some ".ml" -> `keep
  | _ -> `skip

let approved_snapshot_path = fun path ->
  match Path.extension path with
  | Some ext -> Some (Path.add_extension path ~ext:((ext ^ ".expected")))
  | None -> Some (Path.add_extension path ~ext:"expected")

let split_on_double_underscore = fun value ->
  let rec find idx =
    if idx + 1 >= String.length value then
      None
    else if value.[idx] = '_' && value.[idx + 1] = '_' then
      Some idx
    else
      find (idx + 1)
  in
  match find 0 with
  | Some idx -> Some (
    String.sub value 0 idx,
    String.sub value (idx + 2) (String.length value - idx - 2)
  )
  | None -> None

let rule_id_of_fixture = fun path ->
  let basename = Path.basename path in
  let stem =
    match Path.extension path with
    | Some ext -> String.sub basename 0 (String.length basename - String.length ext)
    | None -> basename
  in
  match split_on_double_underscore stem with
  | Some (rule_id, _) when not (String.equal rule_id "") -> Ok rule_id
  | _ -> Error ("invalid autofix fixture name: " ^ basename)

let find_rule = fun rule_id ->
  Riot_fix.Pipeline.default_rules () |> List.find_opt
    (fun rule ->
      String.equal (Riot_fix.Rule.id rule) rule_id) |> Result.of_option
    ~error:(("unknown rule fixture id: " ^ rule_id))

let result_to_json = fun result ->
  Json.obj
    [
      (
        "diagnostics",
        Json.array (List.map Riot_fix.Diagnostic.to_json result.Fixme.Rule_test.initial.diagnostics)
      );
      (
        "fixed_source",
        match result.fixed_source with
        | Some source -> Json.String source
        | None -> Json.Null
      );
      ("applied_fixes", Json.array (List.map Riot_fix.Fix.to_json result.applied_fixes));
      (
        "after_diagnostics",
        match result.after with
        | Some after -> Json.array (List.map Riot_fix.Diagnostic.to_json after.diagnostics)
        | None -> Json.array []
      );
      (
        "after_parse_diagnostics",
        match result.after with
        | Some after -> Json.array (List.map Syn.Diagnostic.to_json after.parse_diagnostics)
        | None -> Json.array []
      );
    ]

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let* rule_id = rule_id_of_fixture ctx.fixture_path in
  let* rule = find_rule rule_id in
  let* source = Fs.read ctx.fixture_path |> Result.map_error IO.error_message in
  let* result = Fixme.Rule_test.run_rule ~rule ~filename:ctx.fixture_path source in
  let* () =
    match result.fixed_source with
    | Some _ -> Ok ()
    | None -> Error ("autofix fixture did not apply a fix for rule " ^ rule_id)
  in
  let* () =
    match result.after with
    | Some after when List.is_empty after.parse_diagnostics -> Ok ()
    | Some _ -> Error ("autofix fixture rewrote to invalid OCaml for rule " ^ rule_id)
    | None -> Error ("autofix fixture did not produce a post-fix analysis for rule " ^ rule_id)
  in
  Test.Snapshot.assert_text
    ~ctx:ctx.test
    ~actual:((Json.to_string_pretty (result_to_json result) ^ "\n"))

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixture_root
          ~filter:keep_ml
          ~snapshot_path:approved_snapshot_path
          ~run:(fun ctx -> test_fixture ~ctx)
      in
      Test.Cli.main ~name:"riot-fix autofix fixtures" ~tests ~args)
    ~args:Env.args
    ()
