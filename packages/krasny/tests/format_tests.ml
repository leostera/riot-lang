open Std

let sample_ml = Path.v "sample.ml"
let workspace_files =
  [
    Path.v "packages/syn/src/token_cursor.mli";
    Path.v "packages/std/src/int.ml";
    Path.v "packages/std/src/bool.ml";
    Path.v "packages/std/src/option.ml";
    Path.v "packages/std/src/result.ml";
  ]

let parse_ml source = Syn.parse ~filename:sample_ml source

let parse_file path =
  let source = Fs.read path |> Result.expect ~msg:"fixture file should exist" in
  Syn.parse ~filename:path source

let with_tempdir prefix fn =
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let capture_json_event ~root event =
  let buffer = IO.Buffer.create 128 in
  let writer =
    let module Write = struct
      type t = IO.Buffer.t
      type err = unit

      let write buffer ~buf =
        IO.Buffer.add_string buffer buf;
        Ok (String.length buf)

      let write_owned_vectored _buffer ~bufs:_ = unimplemented ()
      let flush _buffer = Ok ()
    end in
    IO.Writer.of_write_src (module Write) buffer
  in
  Krasny.Report.write_json_event ~writer ~root event
  |> Result.expect ~msg:"failed to serialize json event";
  IO.Buffer.contents buffer |> String.trim

let assert_json_timestamp_field json =
  match Data.Json.get_field "timestamp" json with
  | Some (Data.Json.String timestamp) ->
      Test.assert_true (String.contains timestamp "T");
      Test.assert_true (String.ends_with ~suffix:"Z" timestamp)
  | Some _ -> panic "timestamp field should be a JSON string"
  | None -> panic "timestamp field missing"

let assert_json_duration_ms_field json =
  match Data.Json.get_field "duration_ms" json with
  | Some (Data.Json.Int duration_ms) -> Test.assert_true (duration_ms >= 0)
  | Some _ -> panic "duration_ms field should be a JSON int"
  | None -> panic "duration_ms field missing"

let assert_idempotent ~source ~msg =
  let first =
    parse_ml source |> Krasny.format |> Result.expect ~msg
  in
  let second =
    parse_ml first |> Krasny.format |> Result.expect ~msg:"formatted output should reformat"
  in
  Test.assert_equal ~expected:first ~actual:second

let assert_roundtrip_hash path =
  let parsed = parse_file path in
  let original_hash = Krasny.syntax_hash parsed in
  let formatted =
    Krasny.format parsed |> Result.expect ~msg:"selected repo files should format"
  in
  let reparsed = Syn.parse ~filename:path formatted in
  let reparsed_hash = Krasny.syntax_hash reparsed in
  Test.assert_equal ~expected:original_hash ~actual:reparsed_hash

let tests =
  [
    Test.case "format returns the original source for a simple implementation"
      (fun () ->
        let source = "let x = 1 + 2\n" in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect ~msg:"simple implementations should format"
        in
        Test.assert_equal ~expected:source ~actual;
        Ok ());
    Test.case "format rewrites parameterized let bindings between formatted lets"
      (fun () ->
        let source = "(* intro *)\nlet x = 1 + 2\nlet f x = x + 1\nlet y = 3 + 4\n" in
        let actual =
          parse_ml source |> Krasny.format
          |> Result.expect
               ~msg:"parameterized let bindings should lower through explicit fun syntax"
        in
        Test.assert_equal
          ~expected:"(* intro *)\nlet x = 1 + 2\n\nlet f x = x + 1\n\nlet y = 3 + 4\n"
          ~actual;
        Ok ());
    Test.case "format keeps mixed trivia and unsupported items parseable" (fun () ->
        let source =
          {|open Std
type t =
  | A
  | B
(* keep with x *)
let x = 1 + 2
let y = 3 + 4
|}
        in
        assert_idempotent ~source ~msg:"mixed implementation files should format";
        Ok ());
    Test.case "format keeps tuple/list/array docs idempotent" (fun () ->
        let source =
          {|let tuple_value = (left_side_identifier, right_side_identifier, final_identifier)
let list_value = [first_item_identifier; second_item_identifier; third_item_identifier]
let array_value = [|first_item_identifier; second_item_identifier; third_item_identifier|]
|}
        in
        assert_idempotent ~source ~msg:"collection expressions should stay stable";
        Ok ());
    Test.case "format keeps function and match lowering idempotent" (fun () ->
        let source =
          {|let f = function x, y -> x + y
let g = function 0 -> "zero" | _ -> "other"
let h = fun x -> match x with 0 -> "zero" | _ -> "other"
|}
        in
        assert_idempotent ~source ~msg:"function and match forms should stay stable";
        Ok ());
    Test.case "format keeps let/if/sequence layouts idempotent" (fun () ->
        let source =
          {|let x =
  if a then (
    b;
    c)
  else d

let y =
  let rec f n = if n = 0 then 1 else n * f (n - 1) in
  f 5
|}
        in
        assert_idempotent ~source ~msg:"control-flow layouts should stay stable";
        Ok ());
    Test.case "format keeps typed and labeled bindings idempotent" (fun () ->
        let source =
          {|let delimiter_of_keyword : keyword -> delimiter option = function | Begin -> Some BeginEnd | _ -> None
let label_arg = f ~y
let optional_arg = f ?y
let optional_fun = fun ?(y = 0) -> y + 1
|}
        in
        assert_idempotent ~source ~msg:"typed/labeled forms should stay stable";
        Ok ());
    Test.case "format preserves syntax hash for selected codebase files"
      (fun () ->
        List.iter assert_roundtrip_hash workspace_files;
        Ok ());
    Test.case "runner skips hidden and build directories" (fun () ->
        with_tempdir "krasny_runner_scan" (fun tmpdir ->
            let visible_ml = Path.(tmpdir / Path.v "visible.ml") in
            let nested_dir = Path.(tmpdir / Path.v "nested") in
            let nested_mli = Path.(nested_dir / Path.v "visible.mli") in
            let hidden_dir = Path.(tmpdir / Path.v ".hidden") in
            let build_dir = Path.(tmpdir / Path.v "_build") in
            Fs.create_dir_all nested_dir |> Result.expect ~msg:"create nested";
            Fs.create_dir_all hidden_dir |> Result.expect ~msg:"create hidden";
            Fs.create_dir_all build_dir |> Result.expect ~msg:"create build";
            Fs.write "let x = 1\n" visible_ml |> Result.expect ~msg:"write visible";
            Fs.write "val x : int\n" nested_mli |> Result.expect ~msg:"write nested";
            Fs.write "let hidden = 1\n" Path.(hidden_dir / Path.v "hidden.ml")
            |> Result.expect ~msg:"write hidden";
            Fs.write "let built = 1\n" Path.(build_dir / Path.v "built.ml")
            |> Result.expect ~msg:"write build";
            let files =
              Krasny.Runner.collect_ocaml_files ~roots:[ tmpdir ] ()
              |> List.map Path.to_string
            in
            let expected =
              [ Path.to_string visible_ml; Path.to_string nested_mli ]
              |> List.sort String.compare
            in
            let actual = List.sort String.compare files in
            Test.assert_equal ~expected ~actual;
            Ok ()));
    Test.case "runner reports formatting status and emits json events" (fun () ->
        with_tempdir "krasny_runner_check" (fun tmpdir ->
            let formatted = Path.(tmpdir / Path.v "formatted.ml") in
            let needs = Path.(tmpdir / Path.v "needs.ml") in
            Fs.write "let x = 1 + 2\n" formatted
            |> Result.expect ~msg:"write formatted";
            Fs.write "let x = 1 + 2\nlet f x = x + 1\n" needs
            |> Result.expect ~msg:"write needs";
            let result = Krasny.Runner.run_checks [ formatted; needs ] in
            Test.assert_equal ~expected:2 ~actual:result.summary.total_files;
            Test.assert_equal
              ~expected:1
              ~actual:result.summary.already_formatted;
            Test.assert_equal
              ~expected:1
              ~actual:result.summary.needs_formatting;
            Test.assert_equal ~expected:0 ~actual:result.summary.failed_files;
            let needs_result =
              result.files
              |> List.find_opt (fun file_result ->
                     String.equal (Path.to_string file_result.Krasny.Runner.file)
                       (Path.to_string needs))
              |> Option.expect ~msg:"needs result missing"
            in
            let json =
              capture_json_event ~root:tmpdir (Krasny.Report.File needs_result)
              |> Data.Json.of_string
              |> Result.expect ~msg:"parse event json"
            in
            let open Data.Json in
            Test.assert_equal
              ~expected:(Some (String "file"))
              ~actual:(get_field "type" json);
            assert_json_timestamp_field json;
            assert_json_duration_ms_field json;
            Test.assert_equal
              ~expected:(Some (String "needs.ml"))
              ~actual:(get_field "file" json);
            Test.assert_equal
              ~expected:(Some (String "needs_formatting"))
              ~actual:(get_field "status" json);
            Ok ()));
    Test.case "streaming runner skips ignored files" (fun () ->
        with_tempdir "krasny_runner_ignore" (fun tmpdir ->
            let keep = Path.(tmpdir / Path.v "keep.ml") in
            let fixtures_dir = Path.(tmpdir / Path.v "tests" / Path.v "fixtures") in
            let ignored = Path.(fixtures_dir / Path.v "fixture.ml") in
            Fs.create_dir_all fixtures_dir
            |> Result.expect ~msg:"create fixtures dir";
            Fs.write "let kept = 1\n" keep |> Result.expect ~msg:"write keep";
            Fs.write "let ignored = 1\n" ignored
            |> Result.expect ~msg:"write ignored";
            let seen = cell [] in
            let result =
              Krasny.Runner.run_checks_streaming ~concurrency:1 ~roots:[ tmpdir ]
                ~should_ignore:(fun path ->
                  String.contains (Path.to_string path) "fixtures")
                ~on_result:(fun file_result ->
                  seen := Path.to_string file_result.file :: !seen)
                ()
            in
            Test.assert_equal
              ~expected:[ Path.to_string keep ]
              ~actual:(List.rev !seen);
            Test.assert_equal ~expected:1 ~actual:result.summary.total_files;
            Ok ()));
    Test.case "streaming runner scans roots and streams file results" (fun () ->
        with_tempdir "krasny_runner_stream" (fun tmpdir ->
            let formatted = Path.(tmpdir / Path.v "formatted.ml") in
            let nested_dir = Path.(tmpdir / Path.v "nested") in
            let needs = Path.(nested_dir / Path.v "needs.mli") in
            Fs.create_dir_all nested_dir |> Result.expect ~msg:"create nested";
            Fs.write "let x = 1 + 2\n" formatted
            |> Result.expect ~msg:"write formatted";
            Fs.write "val x : int\n" needs
            |> Result.expect ~msg:"write needs";
            let seen = cell [] in
            let result =
              Krasny.Runner.run_checks_streaming ~concurrency:1 ~roots:[ tmpdir ]
                ~on_result:(fun file_result ->
                  seen := Path.to_string file_result.file :: !seen)
                ()
            in
            let actual = List.sort String.compare !seen in
            let expected =
              [ Path.to_string formatted; Path.to_string needs ]
              |> List.sort String.compare
            in
            Test.assert_equal ~expected ~actual;
            Test.assert_equal ~expected:2 ~actual:result.summary.total_files;
            Test.assert_equal
              ~expected:2
              ~actual:result.summary.already_formatted;
            Test.assert_equal
              ~expected:0
              ~actual:result.summary.needs_formatting;
            Test.assert_equal ~expected:0 ~actual:result.summary.failed_files;
            let start_json =
              capture_json_event ~root:tmpdir (Krasny.Report.Start { concurrency = 3 })
              |> Data.Json.of_string
              |> Result.expect ~msg:"parse start json"
            in
            let open Data.Json in
            Test.assert_equal
              ~expected:(Some (String "start"))
              ~actual:(get_field "type" start_json);
            assert_json_timestamp_field start_json;
            Test.assert_equal
              ~expected:(Some (Int 3))
              ~actual:(get_field "concurrency" start_json);
            Test.assert_equal ~expected:None ~actual:(get_field "total_files" start_json);
            Ok ()));
  ]

let () =
  Miniriot.run ~main:(fun ~args:_ ->
      Test.Cli.main ~name:"krasny:format" ~tests ~args:Env.args)
    ~args:Env.args ()
