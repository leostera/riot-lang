open Std

module H = Blink.Client

module DiscardWriter = struct
  type t = unit

  let write = fun () ~from:_ -> Ok 0

  let write_vectored = fun () ~from:_ -> Ok 0

  let flush = fun () -> Ok ()
end

let discard_writer = IO.Writer.from_sink (module DiscardWriter) ()

let request () =
  H.Request.make
    ~method_:H.Request.Get
    ~url:"https://example.test/data"
    ~deadline:(Time.Duration.from_secs 5)
    ()

let test_connection_await_keeps_fixed_length_body = fun _ctx ->
  let response = "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: keep-alive\r\n\r\nok" in
  let uri =
    Net.Uri.from_string "http://example.test/data"
    |> Result.expect ~msg:"invalid test uri"
  in
  let conn =
    Blink.Connection.make
      ~reader:(IO.Reader.from_string response)
      ~writer:discard_writer
      ~from_io_error:Blink.Error.from_io_error
      ~uri
  in
  match Blink.Connection.await conn with
  | Error error -> Error (Blink.Error.to_string error)
  | Ok (response, body) ->
      Test.assert_equal
        ~expected:200
        ~actual:(Net.Http.Status.to_int (Net.Http.Response.status response));
      Test.assert_equal ~expected:"ok" ~actual:body;
      Ok ()

let test_status_classification = fun _ctx ->
  Test.assert_equal ~expected:H.Response.Success ~actual:(H.Response.status_class 204);
  Test.assert_equal ~expected:H.Response.RateLimited ~actual:(H.Response.status_class 429);
  Test.assert_equal ~expected:H.Response.ServerError ~actual:(H.Response.status_class 503);
  Ok ()

let test_retries_retryable_statuses = fun _ctx ->
  let calls = ref 0 in
  let transport _request =
    calls := !calls + 1;
    if !calls = 1 then
      Ok (H.Response.make ~status:503 ~body:"try later" ())
    else
      Ok (H.Response.make ~status:200 ~body:"ok" ())
  in
  let config = H.Config.make
    ~retry_policy:(H.RetryPolicy.make ~max_attempts:3 ())
    ~transport
    ()
  in
  let client = H.make ~config () in
  match H.execute client (request ()) with
  | Error error -> Error (H.error_to_string error)
  | Ok (response, telemetry) ->
      Test.assert_equal ~expected:200 ~actual:response.status;
      Test.assert_equal ~expected:2 ~actual:(List.length telemetry.attempts);
      Test.assert_true
        (List.exists
          (fun (entry: H.Telemetry.attempt) -> Option.is_some H.Telemetry.(entry.planned_backoff))
          telemetry.attempts);
      Ok ()

let test_rate_budget_blocks = fun _ctx ->
  let client =
    let transport _request = Ok (H.Response.make ~status:200 ~body:"ok" ()) in
    let budget_policy = H.Budget.policy ~capacity:10 ~window:(Time.Duration.from_secs 60) in
    H.make ~config:(H.Config.make ~transport ~budget_policy ()) ()
  in
  let rec consume remaining =
    if remaining <= 0 then
      Ok ()
    else
      match H.execute client (request ()) with
      | Ok _ -> consume (remaining - 1)
      | Error error -> Error (H.error_to_string error)
  in
  consume 10
  |> Result.and_then
    ~fn:(fun () ->
      match H.execute client (request ()) with
      | Ok _ -> Error "expected geoblock budget exhaustion"
      | Error error ->
          Test.assert_equal ~expected:H.Response.RateLimitedByBudget ~actual:error.class_;
          Ok ())

let test_connection_policy_telemetry = fun _ctx ->
  let observed = ref None in
  let config =
    H.Config.make
      ~connection_policy:H.Config.ReuseConnection
      ~transport:(fun _request -> Ok (H.Response.make ~status:200 ~body:"ok" ()))
      ~telemetry:(fun telemetry -> observed := Some telemetry)
      ()
  in
  let client = H.make ~config () in
  match H.execute client (request ()) with
  | Error error -> Error (H.error_to_string error)
  | Ok (_response, telemetry) ->
      Test.assert_equal ~expected:"reuse_connection" ~actual:telemetry.connection_policy;
      Test.assert_equal ~expected:"transport_may_reuse_connection" ~actual:telemetry.close_behavior;
      (
        match !observed with
        | Some callback_telemetry ->
            Test.assert_equal
              ~expected:telemetry.connection_policy
              ~actual:callback_telemetry.connection_policy;
            Ok ()
        | None -> Error "expected telemetry callback"
      )

let test_failure_telemetry_callback = fun _ctx ->
  let observed = ref None in
  let config =
    H.Config.make
      ~retry_policy:(H.RetryPolicy.make ~max_attempts:1 ())
      ~transport:(fun _request -> Error "connect failed: timeout")
      ~telemetry:(fun telemetry -> observed := Some telemetry)
      ()
  in
  let client = H.make ~config () in
  match H.execute client (request ()) with
  | Ok _ -> Error "expected managed request failure"
  | Error error ->
      Test.assert_equal ~expected:H.Response.ConnectFailed ~actual:error.class_;
      Test.assert_equal
        ~expected:(Some H.Response.ConnectFailed)
        ~actual:error.telemetry.final_error_class;
      Test.assert_equal ~expected:1 ~actual:(List.length error.telemetry.attempts);
      match !observed with
      | Some telemetry ->
          Test.assert_equal
            ~expected:error.telemetry.final_error_class
            ~actual:telemetry.final_error_class;
          Ok ()
      | None -> Error "expected failure telemetry callback"

let test_budget_remaining_tracks_execute = fun _ctx ->
  let client =
    let transport _request = Ok (H.Response.make ~status:200 ~body:"ok" ()) in
    let budget_policy = H.Budget.policy ~capacity:2 ~window:(Time.Duration.from_secs 60) in
    H.make ~config:(H.Config.make ~transport ~budget_policy ()) ()
  in
  Test.assert_equal ~expected:2 ~actual:(H.budget_remaining client);
  match H.execute client (request ()) with
  | Error error -> Error (H.error_to_string error)
  | Ok _ ->
      Test.assert_equal ~expected:1 ~actual:(H.budget_remaining client);
      (
        match H.execute client (request ()) with
        | Error error -> Error (H.error_to_string error)
        | Ok _ ->
            Test.assert_equal ~expected:0 ~actual:(H.budget_remaining client);
            match H.execute client (request ()) with
            | Ok _ -> Error "expected budget exhaustion"
            | Error error ->
                Test.assert_equal ~expected:H.Response.RateLimitedByBudget ~actual:error.class_;
                Ok ()
      )

let test_pool_config_clamps_negative_idle_limit = fun _ctx ->
  let pool = H.Config.pool ~max_idle_per_endpoint:(-10) () in
  Test.assert_equal ~expected:0 ~actual:pool.max_idle_per_endpoint;
  Ok ()

let budgetless_client = fun () ->
  let budget_policy = H.Budget.policy ~capacity:0 ~window:(Time.Duration.from_secs 10) in
  H.make ~config:(H.Config.make ~budget_policy ()) ()

let test_connect_budget_blocks = fun _ctx ->
  let uri =
    Net.Uri.from_string "https://example.test"
    |> Result.expect ~msg:"invalid test uri"
  in
  let client = budgetless_client () in
  match H.connect client uri with
  | Ok conn ->
      H.close client conn;
      Error "expected managed connect budget exhaustion"
  | Error (Blink.Error.ProtocolError message) ->
      Test.assert_true (String.contains message "budget");
      Ok ()
  | Error _ -> Error "expected budget protocol error"

let test_websocket_connect_budget_blocks = fun _ctx ->
  let uri =
    Net.Uri.from_string "wss://example.test/ws"
    |> Result.expect ~msg:"invalid test uri"
  in
  let client = budgetless_client () in
  match H.WebSocket.connect client uri with
  | Ok conn ->
      H.WebSocket.close client conn;
      Error "expected managed websocket connect budget exhaustion"
  | Error (Blink.Error.ProtocolError message) ->
      Test.assert_true (String.contains message "budget");
      Ok ()
  | Error _ -> Error "expected budget protocol error"

let tests =
  Test.[
    case
      "connection await keeps fixed-length response body"
      test_connection_await_keeps_fixed_length_body;
    case "status classification" test_status_classification;
    case "retries retryable statuses" test_retries_retryable_statuses;
    case "rate budget blocks" test_rate_budget_blocks;
    case "connection policy telemetry" test_connection_policy_telemetry;
    case "failure telemetry callback" test_failure_telemetry_callback;
    case "budget remaining tracks execute" test_budget_remaining_tracks_execute;
    case "pool config clamps negative idle limit" test_pool_config_clamps_negative_idle_limit;
    case "connect budget blocks" test_connect_budget_blocks;
    case "websocket connect budget blocks" test_websocket_connect_budget_blocks;
  ]

let main ~args = Test.Cli.main ~name:"blink_client_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
