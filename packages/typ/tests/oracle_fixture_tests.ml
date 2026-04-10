open Std
open Typ
open Typ.Analysis
open Typ.Diagnostics
open Typ.Model
open Typ.Session

let fixtures_dir = Path.v "packages/typ/tests/fixtures/oracle"

let append_snapshot_suffix = fun path suffix ->
  Path.to_string path ^ suffix |> Path.of_string |> Result.expect ~msg:"snapshot path should stay valid UTF-8"

let approved_snapshot_path = fun path -> append_snapshot_suffix path ".expected"

let fixture_filter = fun path ->
  match Path.extension path with
  | Some ".ml" -> `keep
  | _ -> `skip

let stable_fixture_filename = fun (ctx: Test.FixtureRunner.ctx) ->
  Path.join fixtures_dir ctx.fixture_relpath

let parse_failure_report = fun ~filename ->
  fun parse_result ->
    fun error ->
      let source_id = SourceId.of_int 0 in
      let (parse_diagnostics, lowering_diagnostics) =
        match error with
        | Syn.Parse_diagnostics diagnostics -> (diagnostics, [])
        | Syn.Cst_builder_error builder_error -> (
          parse_result.Syn.Parser.diagnostics,
          [ Diagnostic.CstBuilderError { builder_error } ]
        )
      in
      {
        Check_result.source_id;
        filename;
        parse_diagnostics;
        item_tree = None;
        body_arena = None;
        origin_map = None;
        semantic_tree = None;
        lowering_diagnostics;
        typing_diagnostics = [];
        file_summary = FileSummary.missing ~source_id ();
        type_index = TypeIndex.empty;
        exports = [];
        item_traces = [];
        expr_traces = [];
      }

let check_source_text = fun ~filename text ->
  let parse_result = Syn.parse ~filename text in
  match Syn.build_cst parse_result with
  | Ok cst -> Check.check_source ~filename ~parse_result ~cst
  | Error error -> parse_failure_report ~filename parse_result error

let path_exists = fun path -> Fs.exists path |> Result.unwrap_or ~default:false

let split_nonempty_lines = fun text ->
  text
  |> String.split_on_char '\n'
  |> List.filter (fun line -> not (String.equal (String.trim line) ""))

let contains_substring = fun ~needle haystack ->
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec loop index =
    if needle_length = 0 then
      true
    else if index + needle_length > haystack_length then
      false
    else if String.sub haystack index needle_length = needle then
      true
    else
      loop (index + 1)
  in
  loop 0

let replace_all = fun ~needle ~replacement haystack ->
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec find_from index =
    if needle_length = 0 then
      Some index
    else if index + needle_length > haystack_length then
      None
    else if String.sub haystack index needle_length = needle then
      Some index
    else
      find_from (index + 1)
  in
  let rec loop start acc =
    match find_from start with
    | Some index ->
        let prefix = String.sub haystack start (index - start) in
        loop (index + needle_length) (replacement :: prefix :: acc)
    | None ->
        let suffix = String.sub haystack start (haystack_length - start) in
        List.rev (suffix :: acc) |> String.concat ""
  in
  if needle_length = 0 then
    haystack
  else
    loop 0 []

let oracle_host_tokens = fun () ->
  let host = System.host_triplet in
  let base = [ host.architecture; host.vendor; host.os ] in
  match host.abi with
  | Some abi -> base @ [ abi ]
  | None -> base

let toolchain_ocamlc_candidates = fun () ->
  let home_dir = Env.home_dir () |> Option.expect ~msg:"HOME should exist to locate riot toolchains" in
  let toolchains_root = Path.join (Path.join home_dir (Path.v ".riot")) (Path.v "toolchains") in
  if not (path_exists toolchains_root) then
    []
  else
    let output =
      Command.make
        "find"
        ~args:[ Path.to_string toolchains_root; "-name"; "ocamlc"; "-print" ]
      |> Command.output
      |> Result.expect ~msg:"expected toolchain oracle find command to spawn"
    in
    if output.status != 0 then
      []
    else
      output.stdout
      |> split_nonempty_lines
      |> List.filter_map (fun path -> Path.of_string path |> Result.to_option)

let preferred_toolchain_ocamlc = fun ocamlc_paths ->
  let host_tokens =
    oracle_host_tokens ()
    |> List.filter (fun token -> not (String.equal token "") && not (String.equal token "unknown"))
  in
  let score_path path =
    let rendered = Path.to_string path in
    host_tokens
    |> List.filter (fun token -> contains_substring ~needle:token rendered)
    |> List.length
  in
  let best_score =
    ocamlc_paths
    |> List.map score_path
    |> List.fold_left max 0
  in
  match ocamlc_paths |> List.filter (fun path -> Int.equal (score_path path) best_score) with
  | [ ocamlc_path ] -> Some ocamlc_path
  | _ -> None

let oracle_ocamlc_path = fun () ->
  match Env.var Env.String ~name:"TYP_OCAMLC_ORACLE" with
  | Some path ->
      Path.of_string path |> Result.expect ~msg:"TYP_OCAMLC_ORACLE must be a valid UTF-8 path"
  | None ->
      let ocamlc_paths = toolchain_ocamlc_candidates () in
      if List.is_empty ocamlc_paths then
        panic "expected exactly one riot-managed ocamlc oracle, found none";
      match preferred_toolchain_ocamlc ocamlc_paths with
      | Some ocamlc_path -> ocamlc_path
      | None -> panic
        (format
          Format.[
            str "expected exactly one riot-managed ocamlc oracle, found ";
            int (List.length ocamlc_paths);
          ])

type oracle_command_result = {
  output: Command.output;
  source_path: Path.t;
}

let run_oracle_command = fun ~fixture_filename:_ ~source_text ~args ->
  let ocamlc_path = oracle_ocamlc_path () in
  Fs.with_tempdir
    ~prefix:"typ_oracle"
    (fun tmpdir ->
      let oracle_filename = "Oracle_fixture.ml" in
      let source_path = Path.join tmpdir (Path.v oracle_filename) in
      Fs.write source_text source_path |> Result.expect ~msg:"oracle fixture temp source should be writable";
      let output =
        Command.make
          (Path.to_string ocamlc_path)
          ~args:(args @ [ Path.to_string source_path ])
        |> Command.output
        |> Result.expect ~msg:"expected ocamlc oracle invocation to spawn"
      in
      { output; source_path })
  |> Result.expect ~msg:"oracle tempdir should be creatable"

let strip_identifier_stamps = fun text -> text

type oracle_value_export = {
  name: string;
  scheme: string;
}

type oracle_interface = {
  lines: string list;
  value_exports: oracle_value_export list;
  type_names: string list;
}

let split_once = fun line ch ->
  match String.index_opt line ch with
  | Some index ->
      let left = String.sub line 0 index in
      let right = String.sub line (index + 1) (String.length line - index - 1) in
      Some (left, right)
  | None -> None

let parse_val_line = fun line ->
  if not (String.starts_with ~prefix:"val " line) then
    None
  else
    match split_once line ':' with
    | Some (left, right) ->
        Some {
          name = String.trim (String.sub left 4 (String.length left - 4));
          scheme = String.trim right;
        }
    | None -> None

let parse_type_name = fun line ->
  if not (String.starts_with ~prefix:"type " line) then
    None
  else
    let signature =
      match split_once line '=' with
      | Some (left, _right) -> left
      | None -> line
    in
    let tokens =
      String.sub signature 5 (String.length signature - 5)
      |> String.split_on_char ' '
      |> List.filter (fun token -> not (String.equal token ""))
    in
    match List.rev tokens with
    | type_name :: _ -> Some type_name
    | [] -> None

let compare_value_export = fun left right ->
  match String.compare left.name right.name with
  | 0 -> String.compare left.scheme right.scheme
  | order -> order

let run_interface_oracle = fun ~fixture_filename ~source_text ->
  let result =
    run_oracle_command
      ~fixture_filename
      ~source_text
      ~args:[ "-nopervasives"; "-nostdlib"; "-i" ]
  in
  let output = result.output in
  if output.status != 0 then
    panic
      (format
        Format.[
          str "ocamlc -i failed for ";
          str (Path.to_string fixture_filename);
          str "\nstderr:\n";
          str output.stderr;
          str "\nstdout:\n";
          str output.stdout;
        ]);
  let lines = split_nonempty_lines output.stdout in
  {
    lines;
    value_exports = lines |> List.filter_map parse_val_line |> List.sort compare_value_export;
    type_names = lines |> List.filter_map parse_type_name |> List.sort String.compare;
  }

let run_typedtree_oracle = fun ~fixture_filename ~source_text ->
  let result =
    run_oracle_command
      ~fixture_filename
      ~source_text
      ~args:[ "-nopervasives"; "-nostdlib"; "-dtypedtree"; "-c" ]
  in
  let output = result.output in
  if output.status != 0 then
    panic
      (format
        Format.[
          str "ocamlc -dtypedtree failed for ";
          str (Path.to_string fixture_filename);
          str "\nstderr:\n";
          str output.stderr;
          str "\nstdout:\n";
          str output.stdout;
        ]);
  output.stderr
  |> replace_all ~needle:(Path.to_string result.source_path) ~replacement:(Path.to_string fixture_filename)
  |> strip_identifier_stamps
  |> split_nonempty_lines

let normalize_typ_scheme = fun scheme ->
  match split_once scheme '.' with
  | Some (left, right) when String.starts_with ~prefix:"'" (String.trim left) -> String.trim right
  | _ -> scheme

let typ_value_exports = fun (report: Check_result.t) ->
  FileSummary.exports report.file_summary
  |> List.map (fun (name, scheme) ->
    {
      name;
      scheme = TypePrinter.scheme_to_string scheme |> normalize_typ_scheme;
    })
  |> List.sort compare_value_export

let typ_type_names = fun (report: Check_result.t) ->
  FileSummary.type_decls report.file_summary
  |> List.map (fun ({ declaration; _ }: FileSummary.type_decl) -> declaration.type_name)
  |> List.sort String.compare

let completeness_to_string = function
  | FileSummary.Complete -> "complete"
  | FileSummary.Partial -> "partial"

let export_to_json = fun (export: oracle_value_export) ->
  Data.Json.Object [
    ("name", Data.Json.String export.name);
    ("scheme", Data.Json.String export.scheme);
  ]

let report_json = fun (report: Check_result.t) interface typedtree_lines ->
  Data.Json.Object [
    (
      "comparison",
      Data.Json.Object [
        (
          "exports",
          Data.Json.Object [
            (
              "ocamlc",
              Data.Json.Array (List.map export_to_json interface.value_exports)
            );
            (
              "typ",
              Data.Json.Array (List.map export_to_json (typ_value_exports report))
            );
          ]
        );
        (
          "type_names",
          Data.Json.Object [
            (
              "ocamlc",
              Data.Json.Array (List.map (fun name -> Data.Json.String name) interface.type_names)
            );
            (
              "typ",
              Data.Json.Array (List.map (fun name -> Data.Json.String name) (typ_type_names report))
            );
          ]
        );
      ]
    );
    (
      "ocamlc",
      Data.Json.Object [
        (
          "interface_text",
          Data.Json.String (String.concat "\n" interface.lines)
        );
        (
          "interface_lines",
          Data.Json.Array (List.map (fun line -> Data.Json.String line) interface.lines)
        );
        (
          "exports",
          Data.Json.Array (List.map export_to_json interface.value_exports)
        );
        (
          "type_names",
          Data.Json.Array (List.map (fun name -> Data.Json.String name) interface.type_names)
        );
        (
          "typedtree_lines",
          Data.Json.Array (List.map (fun line -> Data.Json.String line) typedtree_lines)
        );
      ]
    );
    ("typ", Report.to_json report);
    (
      "typ_summary",
      Data.Json.Object [
        ("completeness", Data.Json.String (FileSummary.completeness report.file_summary |> completeness_to_string));
        (
          "exports",
          Data.Json.Array (List.map export_to_json (typ_value_exports report))
        );
        (
          "type_names",
          Data.Json.Array (List.map (fun name -> Data.Json.String name) (typ_type_names report))
        );
      ]
    );
  ]

let assert_no_diagnostics = fun (report: Check_result.t) ->
  Test.assert_equal ~expected:[] ~actual:report.parse_diagnostics;
  Test.assert_equal ~expected:[] ~actual:report.lowering_diagnostics;
  Test.assert_equal ~expected:[] ~actual:report.typing_diagnostics;
  Test.assert_equal ~expected:FileSummary.Complete ~actual:(FileSummary.completeness report.file_summary)

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let fixture_filename = stable_fixture_filename ctx in
  let source_text = Fs.read ctx.fixture_path |> Result.expect ~msg:"oracle fixture should exist" in
  let report = check_source_text ~filename:fixture_filename source_text in
  let interface = run_interface_oracle ~fixture_filename ~source_text in
  let typedtree_lines = run_typedtree_oracle ~fixture_filename ~source_text in
  let actual_json = report_json report interface typedtree_lines in
  match Test.Snapshot.assert_json ~ctx:ctx.test ~actual:actual_json with
  | Error _ as err -> err
  | Ok () ->
      assert_no_diagnostics report;
      Test.assert_equal ~expected:interface.value_exports ~actual:(typ_value_exports report);
      Test.assert_equal ~expected:interface.type_names ~actual:(typ_type_names report);
      Ok ()

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixtures_dir
          ~filter:fixture_filter
          ~snapshot_path:(fun path -> Some (approved_snapshot_path path))
          ~run:(fun ctx -> test_fixture ~ctx)
      in
      Test.Cli.main ~name:"typ:oracle_fixtures" ~tests ~args)
    ~args:Std.Env.args
    ()
