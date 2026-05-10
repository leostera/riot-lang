open Std

module H = Blink.Client
module Testing = Blink.Testing
module Recorder = Testing.Recorder
module Recording = Testing.Recording

let request ?body ?(headers = []) ~url () =
  H.Request.make ~method_:H.Request.Post ~url ~headers ?body ()

let expect_ok = fun result ->
  match result with
  | Ok value -> value
  | Error error -> panic error

let expect_recorder_ok = fun result ->
  match result with
  | Ok value -> value
  | Error error -> panic (Recorder.error_to_string error)

let execute = fun client request ->
  match H.execute client request with
  | Ok (response, _telemetry) -> Ok response
  | Error error -> Error (H.error_to_string error)

let test_record_once_writes_and_replays = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"blink-recorder-"
    (fun library_dir ->
      let calls = ref 0 in
      let upstream _request =
        calls := !calls + 1;
        Ok (H.Response.make ~status:200 ~body:"from upstream" ~headers:[ ("x-live", "yes") ] ())
      in
      let recorder = Recorder.make ~library_dir ~upstream_transport:upstream () in
      let name = "openai/responses_create" in
      let req =
        request
          ~url:"https://api.openai.test/v1/responses"
          ~headers:[
            ("Authorization", "Bearer secret-token");
            ("Content-Type", "application/json");
          ]
          ~body:{|{"model":"gpt-4o-mini","input":"hello"}|}
          ()
      in
      let response =
        Recorder.use_recording recorder ~name ~fn:(fun client -> execute client req)
        |> expect_recorder_ok
        |> expect_ok
      in
      Test.assert_equal ~expected:200 ~actual:response.status;
      Test.assert_equal ~expected:"from upstream" ~actual:response.body;
      Test.assert_equal ~expected:1 ~actual:!calls;
      let recording_path = Path.(library_dir / Path.v "openai-responses_create.json") in
      let recording =
        Fs.read recording_path
        |> Result.expect ~msg:"expected recording"
      in
      Test.assert_false (String.contains recording "secret-token");
      Test.assert_true (String.contains recording "<REDACTED>");
      let replay_recorder =
        Recorder.make
          ~library_dir
          ~upstream_transport:(fun _request ->
            Error (Blink.Error.ProtocolError (Blink.Error.ApplicationTransportError "network should not be called")))
          ()
      in
      let replayed =
        Recorder.use_recording replay_recorder ~name ~fn:(fun client -> execute client req)
        |> expect_recorder_ok
        |> expect_ok
      in
      Test.assert_equal ~expected:200 ~actual:replayed.status;
      Test.assert_equal ~expected:"from upstream" ~actual:replayed.body;
      Test.assert_equal ~expected:1 ~actual:!calls;
      Ok ()) with
  | Error error -> Error (IO.error_message error)
  | Ok result -> result

let test_replay_only_misses_without_recording = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"blink-recorder-"
    (fun library_dir ->
      let recorder = Recorder.make ~library_dir ~mode:ReplayOnly () in
      let req = request ~url:"https://example.test/missing" () in
      let result =
        Recorder.use_recording recorder ~name:"missing" ~fn:(fun client -> execute client req)
        |> expect_recorder_ok
      in
      match result with
      | Ok _ -> Error "expected replay-only miss"
      | Error message ->
          Test.assert_true (String.contains message "ReplayOnlyMiss");
          Ok ()) with
  | Error error -> Error (IO.error_message error)
  | Ok result -> result

let test_record_once_existing_recording_rejects_new_request = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"blink-recorder-"
    (fun library_dir ->
      let calls = ref 0 in
      let upstream _request =
        calls := !calls + 1;
        Ok (H.Response.make ~status:200 ~body:"recorded" ())
      in
      let name = "once" in
      let recorder = Recorder.make ~library_dir ~upstream_transport:upstream () in
      let first = request ~url:"https://example.test/one" () in
      Recorder.use_recording recorder ~name ~fn:(fun client -> execute client first)
      |> expect_recorder_ok
      |> expect_ok
      |> ignore;
      Test.assert_equal ~expected:1 ~actual:!calls;
      let second = request ~url:"https://example.test/two" () in
      let replay_recorder = Recorder.make ~library_dir ~upstream_transport:upstream () in
      let result =
        Recorder.use_recording replay_recorder ~name ~fn:(fun client -> execute client second)
        |> expect_recorder_ok
      in
      match result with
      | Ok _ -> Error "expected record-once miss for existing recording"
      | Error message ->
          Test.assert_true (String.contains message "RecordOnceMiss");
          Test.assert_equal ~expected:1 ~actual:!calls;
          Ok ()) with
  | Error error -> Error (IO.error_message error)
  | Ok result -> result

let test_record_all_replaces_existing_recording = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"blink-recorder-"
    (fun library_dir ->
      let response_body = ref "first" in
      let calls = ref 0 in
      let upstream _request =
        calls := !calls + 1;
        Ok (H.Response.make ~status:200 ~body:!response_body ())
      in
      let recorder = Recorder.make ~library_dir ~upstream_transport:upstream () in
      let name = "replace" in
      let req = request ~url:"https://example.test/replace" () in
      Recorder.use_recording recorder ~name ~fn:(fun client -> execute client req)
      |> expect_recorder_ok
      |> expect_ok
      |> ignore;
      Test.assert_equal ~expected:1 ~actual:!calls;
      response_body := "second";
      let replace_recorder =
        Recorder.make ~library_dir ~mode:RecordAll ~upstream_transport:upstream ()
      in
      let replaced =
        Recorder.use_recording replace_recorder ~name ~fn:(fun client -> execute client req)
        |> expect_recorder_ok
        |> expect_ok
      in
      Test.assert_equal ~expected:"second" ~actual:replaced.body;
      Test.assert_equal ~expected:2 ~actual:!calls;
      let replay_recorder =
        Recorder.make
          ~library_dir
          ~upstream_transport:(fun _request ->
            Error (Blink.Error.ProtocolError (Blink.Error.ApplicationTransportError "network should not be called")))
          ()
      in
      let replayed =
        Recorder.use_recording replay_recorder ~name ~fn:(fun client -> execute client req)
        |> expect_recorder_ok
        |> expect_ok
      in
      Test.assert_equal ~expected:"second" ~actual:replayed.body;
      Test.assert_equal ~expected:2 ~actual:!calls;
      Ok ()) with
  | Error error -> Error (IO.error_message error)
  | Ok result -> result

let test_recording_name_sanitization = fun _ctx ->
  Test.assert_equal ~expected:"secret" ~actual:(Recording.sanitize_name "../secret");
  Test.assert_equal
    ~expected:"openai-responses-create"
    ~actual:(Recording.sanitize_name "openai/responses create");
  Test.assert_equal ~expected:"recording" ~actual:(Recording.sanitize_name " ... ");
  Ok ()

let tests =
  Test.[
    case "record once writes and replays" test_record_once_writes_and_replays;
    case "replay only misses without recording" test_replay_only_misses_without_recording;
    case
      "record once existing recording rejects new request"
      test_record_once_existing_recording_rejects_new_request;
    case "record all replaces existing recording" test_record_all_replaces_existing_recording;
    case "recording name sanitization" test_recording_name_sanitization;
  ]

let main ~args = Test.Cli.main ~name:"blink_recorder_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
