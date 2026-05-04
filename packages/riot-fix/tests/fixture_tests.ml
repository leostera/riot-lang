open Std
open Std.Data
open Std.Collections

module Fixture_no_stdlib = struct
  module Api = Fixme

  let package_name = "std"

  let package_rule_id = Api.Rule_id.from_string (package_name ^ ":no-stdlib")

  let rule_description =
    "Detect direct Stdlib, Unix, Sys, and Pervasives usage from the Std package boundary"

  let unix_body =
    {|
Direct calls into Unix bypass Riot's scheduling and portability boundaries.

In Riot, a blocking Unix call is not just a local implementation detail. It can
stall the cooperative scheduler, delay unrelated actors, and leak host-specific
behavior into packages that are supposed to stay portable.

Keep real operating-system boundaries down in packages that are meant to own
them, like `kernel`. Everywhere else, prefer the Riot-owned abstraction that
already exists, or add one deliberately if the boundary is genuinely missing.
|}

  let sys_body =
    {|
Direct Sys usage reaches into process-global runtime state instead of going through Riot-owned boundaries.

`Sys` exposes host and runtime details directly from OCaml. Once those calls
spread through ordinary packages, portability decisions and process-global
policy leak into places that should not own them.

Prefer Riot wrappers for system information and runtime behavior. If something
truly belongs at the process or platform boundary, keep it in a package that is
explicitly responsible for that boundary instead of reaching for `Sys`
everywhere.
|}

  let stdlib_body =
    {|
Code outside the runtime boundary should go through Riot's Std layer instead of referencing Stdlib directly.

Riot is trying to offer one coherent standard surface, not a mixture of
package-local conventions plus ad hoc direct references into `Stdlib`.
Routing code through `Std` gives the ecosystem one owned API surface and leaves
room for shared defaults, portability adjustments, and package-wide style.

When `Std` already owns an API, use it. When it does not, that is usually a
signal to extend `Std` deliberately instead of bypassing it forever.
|}

  let pervasives_body =
    {|
Pervasives is the historical pre-Stdlib module and should not appear in modern Riot code.

`Pervasives` is legacy OCaml surface area. Riot code should point at the
current owned surface, not an old compatibility layer that survives mostly for
historical reasons.

Replace direct `Pervasives` references with `Std`.
|}

  let rule_explain = String.concat "\n\n" [ unix_body; sys_body; stdlib_body; pervasives_body; ]

  let explanation =
    Api.Explanation.{ rule_id = package_rule_id; body = rule_explain; message = rule_description }

  let explanations = fun () -> [ explanation ]

  let forbidden_modules = [ "Stdlib"; "Pervasives"; "Unix"; "Sys"; ]

  let replacement_for = fun __tmp1 ->
    match __tmp1 with
    | "Stdlib"
    | "Pervasives" -> Some "Std"
    | _ -> None

  let make_message = fun text ->
    match replacement_for text with
    | Some replacement ->
        "Direct usage of " ^ text ^ " is discouraged. Use " ^ replacement ^ " instead."
    | None ->
        "Direct usage of " ^ text ^ " is discouraged. Use package-owned Riot abstractions instead."

  let make_suggestion = fun text ->
    match replacement_for text with
    | Some replacement -> Some ("Replace " ^ text ^ " with " ^ replacement)
    | None -> None

  let make_fix = fun token replacement ->
    Api.Fix.make
      ~title:("Replace " ^ Syn.Ast.Token.text token ^ " with " ^ replacement)
      ~operations:[ Api.Fix.replace_token_with_text ~target:token ~text:replacement ]

  let make_diagnostic = fun token ->
    let text = Syn.Ast.Token.text token in
    let suggestion = make_suggestion text in
    let fix =
      replacement_for text
      |> Option.map ~fn:(make_fix token)
    in
    let kind = Api.Diagnostic.Known { rule_id = package_rule_id; message = make_message text } in
    Api.Diagnostic.make
      ~severity:Warning
      ~kind
      ~span:(Syn.Span.make
        ~start:(Syn.Ast.Token.span_start token)
        ~end_:(Syn.Ast.Token.span_end token))
      ?suggestion
      ?fix
      ()

  let dedupe_diagnostics = fun diagnostics ->
    let seen = HashMap.create () in
    List.filter
      diagnostics
      ~fn:(fun diag ->
        let span = Api.Diagnostic.span diag in
        let key =
          Api.Rule_id.to_string (Api.Diagnostic.rule_id diag)
          ^ ":"
          ^ Int.to_string span.start
          ^ ":"
          ^ Int.to_string span.end_
        in
        match HashMap.get seen ~key with
        | Some _ -> false
        | None ->
            let _ = HashMap.insert seen ~key ~value:true in
            true)

  let check_tree = fun (_ctx: Api.Rule.context) root ->
    Api.Traversal.find_tokens
      (fun token -> List.contains forbidden_modules ~value:(Syn.Ast.Token.text token))
      root
    |> List.map ~fn:make_diagnostic
    |> dedupe_diagnostics

  let rule = fun () ->
    Api.Rule.make
      ~id:package_rule_id
      ~description:rule_description
      ~explain:rule_explain
      ~run:check_tree
      ()
end

module Fixture_std_provider = struct
  let name = "std"

  let rules = fun () -> [ Fixture_no_stdlib.rule () ]

  let explanations = fun () -> Fixture_no_stdlib.explanations ()
end

let tests_dir = Path.v "packages/riot-fix/tests/fixtures"

let is_digit = fun __tmp1 ->
  match __tmp1 with
  | '0' .. '9' -> true
  | _ -> false

let fixture_filter = fun path ->
  match Path.extension path with
  | Some ".ml" ->
      let name = Path.basename path in
      if String.length name >= 4
      && (
        String.get name ~at:0
        |> Option.is_some_and ~fn:is_digit
      )
      && (
        String.get name ~at:1
        |> Option.is_some_and ~fn:is_digit
      )
      && (
        String.get name ~at:2
        |> Option.is_some_and ~fn:is_digit
      )
      && (
        String.get name ~at:3
        |> Option.is_some_and ~fn:is_digit
      ) then
        Test.FixtureRunner.Keep
      else
        Test.FixtureRunner.Skip
  | _ -> Test.FixtureRunner.Skip

let append_snapshot_suffix = fun path suffix ->
  Path.to_string path ^ suffix
  |> Path.from_string
  |> Result.expect ~msg:"snapshot path should stay valid UTF-8"

let approved_snapshot_path = fun path -> append_snapshot_suffix path ".expected"

let relativize_path = fun ~workspace_root path ->
  match Path.strip_prefix path ~prefix:workspace_root with
  | Ok relpath -> relpath
  | Error _ -> path

let file_result_to_json = fun ~workspace_root result ->
  let open Json in
  Object [
    (
      "file",
      String (Path.to_string (relativize_path ~workspace_root Riot_fix.Runner.(result.file)))
    );
    ("changed", Bool Riot_fix.Runner.(result.changed));
    ("error", match Riot_fix.Runner.(result.error) with
    | Some err -> String err
    | None -> Null);
    (
      "applied_fixes",
      Array (List.map Riot_fix.Runner.(result.applied_fixes) ~fn:Riot_fix.Fix.to_json)
    );
    (
      "parse_diagnostics",
      Array (List.map Riot_fix.Runner.(result.parse_diagnostics) ~fn:Syn.Diagnostic.to_json)
    );
    (
      "diagnostics",
      Array (List.map Riot_fix.Runner.(result.diagnostics) ~fn:Riot_fix.Diagnostic.to_json)
    );
  ]

let run_result_to_json = fun ~workspace_root result ->
  Json.Object [
    ("summary", Riot_fix.Runner.summary_to_json Riot_fix.Runner.(result.summary));
    (
      "files",
      Json.Array (List.map Riot_fix.Runner.(result.files) ~fn:(file_result_to_json ~workspace_root))
    );
  ]

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let workspace_root =
    ctx.test.Test.workspace_root
    |> Option.expect ~msg:"fixture snapshots require a workspace root"
  in
  let () =
    Riot_fix.Provider_registry.register_providers
      [ (module Fixture_std_provider : Riot_fix.Provider.S); ]
  in
  let result = Riot_fix.Runner.run_files ~mode:Check [ ctx.fixture_path ] in
  let actual_json = run_result_to_json ~workspace_root result in
  Test.Snapshot.assert_with
    ~ctx:ctx.test
    ~render:(fun json -> Json.to_string_pretty json ^ "\n")
    ~actual:actual_json

let main ~args =
  let tests =
    Test.FixtureRunner.cases
      ()
      ~dir:tests_dir
      ~filter:fixture_filter
      ~snapshot_path:(fun path -> Some (approved_snapshot_path path))
      ~run:(fun ctx -> test_fixture ~ctx)
  in
  Test.Cli.main ~name:"riot-fix:fixtures" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
