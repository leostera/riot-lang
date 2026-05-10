open Std
open Std.Data
open Std.Result.Syntax

let fixture_root = Path.v "packages/riot-fix/tests/autofix_fixtures"

let keep_ml = fun path ->
  match Path.extension path with
  | Some ".ml" -> Test.FixtureRunner.Keep
  | _ -> Test.FixtureRunner.Skip

let disabled_rule_marker = "Rule disabled while Syn Ast migration is in progress"

let append_snapshot_suffix = fun path suffix ->
  Path.to_string path ^ suffix
  |> Path.from_string
  |> Result.expect ~msg:"snapshot path should stay valid UTF-8"

let approved_snapshot_path = fun path -> Some (append_snapshot_suffix path ".expected")

let split_on_double_underscore = fun value ->
  let rec find idx =
    if idx + 1 >= String.length value then
      None
    else if String.get value ~at:idx = Some '_' && String.get value ~at:(idx + 1) = Some '_' then
      Some idx
    else
      find (idx + 1)
  in
  match find 0 with
  | Some idx ->
      Some (
        String.sub value ~offset:0 ~len:idx,
        String.sub value ~offset:(idx + 2) ~len:(String.length value - idx - 2)
      )
  | None -> None

let rule_id_of_fixture = fun path ->
  let basename = Path.basename path in
  let stem =
    match Path.extension path with
    | Some ext -> String.sub basename ~offset:0 ~len:(String.length basename - String.length ext)
    | None -> basename
  in
  match split_on_double_underscore stem with
  | Some (rule_id, _) when not (String.equal rule_id "") ->
      Ok (Riot_fix.Rule_id.from_string rule_id)
  | _ -> Error ("invalid autofix fixture name: " ^ basename)

let find_rule = fun rule_id ->
  Riot_fix.Pipeline.default_rules ()
  |> List.find ~fn:(fun rule -> Riot_fix.Rule_id.equal (Riot_fix.Rule.id rule) rule_id)
  |> Option.ok_or ~error:("unknown rule fixture id: " ^ Riot_fix.Rule_id.to_string rule_id)

let rule_is_temporarily_disabled = fun rule ->
  String.contains
    (Riot_fix.Rule.description rule)
    disabled_rule_marker

let keep_enabled_rule_fixture = fun path ->
  match keep_ml path with
  | Test.FixtureRunner.Skip -> Test.FixtureRunner.Skip
  | Test.FixtureRunner.Keep ->
      match rule_id_of_fixture path with
      | Error _ -> Test.FixtureRunner.Keep
      | Ok rule_id ->
          match find_rule rule_id with
          | Ok rule when rule_is_temporarily_disabled rule -> Test.FixtureRunner.Skip
          | Ok _
          | Error _ -> Test.FixtureRunner.Keep

let result_to_json = fun result ->
  Json.obj
    [
      (
        "diagnostics",
        Json.array
          (List.map result.Fixme.Rule_test.initial.diagnostics ~fn:Riot_fix.Diagnostic.to_json)
      );
      ("fixed_source", match result.fixed_source with
      | Some source -> Json.String source
      | None -> Json.Null);
      ("applied_fixes", Json.array (List.map result.applied_fixes ~fn:Riot_fix.Fix.to_json));
      ("after_diagnostics", match result.after with
      | Some after -> Json.array (List.map after.diagnostics ~fn:Riot_fix.Diagnostic.to_json)
      | None -> Json.array []);
      ("after_parse_diagnostics", match result.after with
      | Some after -> Json.array (List.map after.parse_diagnostics ~fn:Syn.Diagnostic.to_json)
      | None -> Json.array []);
    ]

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let* rule_id = rule_id_of_fixture ctx.fixture_path in
  let* rule = find_rule rule_id in
  let* source =
    Fs.read ctx.fixture_path
    |> Result.map_err ~fn:IO.error_message
  in
  let* result = Fixme.Rule_test.run_rule ~rule ~filename:ctx.fixture_path source in
  let* () =
    match result.fixed_source with
    | Some _ -> Ok ()
    | None ->
        Error ("autofix fixture did not apply a fix for rule " ^ Riot_fix.Rule_id.to_string rule_id)
  in
  let* () =
    match result.after with
    | Some after when List.is_empty after.parse_diagnostics -> Ok ()
    | Some _ ->
        Error ("autofix fixture rewrote to invalid OCaml for rule "
        ^ Riot_fix.Rule_id.to_string rule_id)
    | None ->
        Error ("autofix fixture did not produce a post-fix analysis for rule "
        ^ Riot_fix.Rule_id.to_string rule_id)
  in
  Test.Snapshot.assert_text
    ~ctx:ctx.test
    ~actual:(Json.to_string_pretty (result_to_json result) ^ "\n")

let main ~args =
  let tests =
    Test.FixtureRunner.cases
      ()
      ~dir:fixture_root
      ~filter:keep_enabled_rule_fixture
      ~snapshot_path:approved_snapshot_path
      ~run:(fun ctx -> test_fixture ~ctx)
  in
  Test.Cli.main ~name:"riot-fix autofix fixtures" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
