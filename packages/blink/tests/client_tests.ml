open Std

module H = Blink.Client

module DiscardWriter = struct
  type t = unit

  let write = fun () ~from:_ -> Ok 0

  let write_vectored = fun () ~from:_ -> Ok 0

  let flush = fun () -> Ok ()
end

let discard_writer = IO.Writer.from_sink (module DiscardWriter) ()

module ThrottledReader = struct
  type t = {
    payload: string;
    max_chunk: int;
    mutable offset: int;
  }

  let remaining state = String.length state.payload - state.offset

  let read state ~into =
    if remaining state <= 0 then
      Ok 0
    else
      let writable = IO.Buffer.writable_bytes into in
      let writable =
        if writable <= 0 then
          state.max_chunk
        else
          writable
      in
      let count = Int.min (remaining state) (Int.min state.max_chunk writable) in
      match IO.Buffer.append_substring into state.payload ~off:state.offset ~len:count with
      | Error _ -> Error IO.Buffer_full
      | Ok () ->
          state.offset <- state.offset + count;
          Ok count

  let read_vectored state ~into:_ = read state ~into:(IO.Buffer.create ~size:state.max_chunk)

  let is_read_vectored _state = false
end

let throttled_reader payload ~max_chunk =
  IO.Reader.from_source (module ThrottledReader) { ThrottledReader.payload; max_chunk; offset = 0 }

let request () =
  H.Request.make
    ~method_:H.Request.Get
    ~url:"https://example.test/data"
    ~deadline:(Time.Duration.from_secs 5)
    ()

let repeat_x count =
  let rec loop acc remaining =
    if remaining <= 0 then
      String.concat "" acc
    else
      loop ("x" :: acc) (remaining - 1)
  in
  loop [] count

let first_data_chunk messages =
  let rec loop messages =
    match messages with
    | [] -> None
    | message :: rest -> (
        match message with
        | Blink.Connection.Data chunk -> Some chunk
        | Blink.Connection.Done
        | Blink.Connection.Headers _
        | Blink.Connection.Status _ -> loop rest
      )
  in
  loop messages

let test_connection_await_keeps_fixed_length_body = fun _ctx ->
  let response = "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: keep-alive\r\n\r\nok" in
  let uri =
    Net.Uri.from_string "http://example.test/data"
    |> Result.expect ~msg:"invalid test uri"
  in
  let conn =
    Blink.Connection.make ~reader:(IO.Reader.from_string response) ~writer:discard_writer ~uri ()
  in
  match Blink.Connection.await conn with
  | Error error -> Error (Blink.Error.to_string error)
  | Ok (response, body) ->
      Test.assert_equal
        ~expected:200
        ~actual:(Net.Http.Status.to_int (Net.Http.Response.status response));
      Test.assert_equal ~expected:"ok" ~actual:body;
      Ok ()

let test_connection_streams_partial_chunked_body = fun _ctx ->
  let body = repeat_x 10_000 in
  let response =
    "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nConnection: close\r\n\r\n"
    ^ "2710\r\n"
    ^ body
    ^ "\r\n0\r\n\r\n"
  in
  let uri =
    Net.Uri.from_string "http://example.test/data"
    |> Result.expect ~msg:"invalid test uri"
  in
  let conn =
    Blink.Connection.make
      ~reader:(throttled_reader response ~max_chunk:64)
      ~writer:discard_writer
      ~uri
      ()
  in
  match Blink.Connection.stream conn with
  | Error error -> Error (Blink.Error.to_string error)
  | Ok head ->
      Test.assert_true
        (
          List.exists
            (fun message ->
              match message with
              | Blink.Connection.Status status -> Net.Http.Status.to_int status = 200
              | Blink.Connection.Data _
              | Blink.Connection.Done
              | Blink.Connection.Headers _ -> false)
            head
        );
      (
        match Blink.Connection.stream conn with
        | Error error -> Error (Blink.Error.to_string error)
        | Ok messages -> (
            match first_data_chunk messages with
            | None -> Error "expected partial chunk data"
            | Some chunk ->
                Test.assert_true (String.length chunk > 0);
                Test.assert_true (String.length chunk < String.length body);
                Ok ()
          )
      )

let test_connection_stream_rejects_invalid_content_length = fun _ctx ->
  let response = "HTTP/1.1 200 OK\r\nContent-Length: nope\r\n\r\n" in
  let uri =
    Net.Uri.from_string "http://example.test/data"
    |> Result.expect ~msg:"invalid test uri"
  in
  let conn =
    Blink.Connection.make ~reader:(IO.Reader.from_string response) ~writer:discard_writer ~uri ()
  in
  match Blink.Connection.stream conn with
  | Error (Blink.Error.ParseError (Http.Http1.Common.InvalidContentLength _)) -> Ok ()
  | Error error -> Error ("expected invalid content-length, got " ^ Blink.Error.to_string error)
  | Ok _ -> Error "expected invalid content-length error"

let test_connection_close_invokes_close_callback = fun _ctx ->
  let closed = ref false in
  let uri =
    Net.Uri.from_string "http://example.test/data"
    |> Result.expect ~msg:"invalid test uri"
  in
  let conn =
    Blink.Connection.make
      ~reader:(IO.Reader.from_string "")
      ~writer:discard_writer
      ~on_close:(fun () -> closed := true)
      ~uri
      ()
  in
  Blink.Connection.close conn;
  Test.assert_true !closed;
  Ok ()

let test_status_classification = fun _ctx ->
  Test.assert_equal ~expected:H.Response.Success ~actual:(H.Response.status_class 204);
  Test.assert_equal ~expected:H.Response.RateLimited ~actual:(H.Response.status_class 429);
  Test.assert_equal ~expected:H.Response.ServerError ~actual:(H.Response.status_class 503);
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

let test_http_429_is_external_rate_limit = fun _ctx ->
  let client =
    let transport _request = Ok (H.Response.make ~status:429 ~body:"slow down" ()) in
    H.make ~config:(H.Config.make ~transport ()) ()
  in
  match H.execute client (request ()) with
  | Ok _ -> Error "expected external rate limit error"
  | Error error ->
      Test.assert_equal ~expected:H.Response.RateLimitedResponse ~actual:error.class_;
      Ok ()

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
      ~transport:(fun _request -> Error (Blink.Error.NetError Net.Connection_refused))
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

let test_transport_exception_becomes_error = fun _ctx ->
  let client =
    H.make
      ~config:(H.Config.make ~transport:(fun _request -> raise (Failure "TLS handshake failed")) ())
      ()
  in
  match H.execute client (request ()) with
  | Ok _ -> Error "expected transport exception to become managed error"
  | Error error ->
      Test.assert_equal ~expected:H.Response.UnknownError ~actual:error.class_;
      Test.assert_true (String.contains error.message "TLS handshake failed");
      Test.assert_equal
        ~expected:(Some H.Response.UnknownError)
        ~actual:error.telemetry.final_error_class;
      Ok ()

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
  | Error (Blink.Error.ProtocolError Blink.Error.RequestBudgetExhausted) -> Ok ()
  | Error _ -> Error "expected budget protocol error"

let test_websocket_connect_budget_blocks = fun _ctx ->
  let uri =
    Net.Uri.from_string "wss://example.test/ws"
    |> Result.expect ~msg:"invalid test uri"
  in
  let client = budgetless_client () in
  match H.WebSocket.connect client uri with
  | Ok conn -> (
      match H.WebSocket.close client conn with
      | Error error -> Error (Blink.Error.to_string error)
      | Ok () -> Error "expected managed websocket connect budget exhaustion"
    )
  | Error (Blink.Error.ProtocolError Blink.Error.RequestBudgetExhausted) -> Ok ()
  | Error _ -> Error "expected budget protocol error"

let tests =
  Test.[
    case
      "connection await keeps fixed-length response body"
      test_connection_await_keeps_fixed_length_body;
    case
      "connection stream returns partial chunked body data"
      test_connection_streams_partial_chunked_body;
    case
      "connection stream rejects invalid content length"
      test_connection_stream_rejects_invalid_content_length;
    case "connection close invokes close callback" test_connection_close_invokes_close_callback;
    case "status classification" test_status_classification;
    case "rate budget blocks" test_rate_budget_blocks;
    case "http 429 is external rate limit" test_http_429_is_external_rate_limit;
    case "connection policy telemetry" test_connection_policy_telemetry;
    case "failure telemetry callback" test_failure_telemetry_callback;
    case "transport exception becomes managed error" test_transport_exception_becomes_error;
    case "budget remaining tracks execute" test_budget_remaining_tracks_execute;
    case "pool config clamps negative idle limit" test_pool_config_clamps_negative_idle_limit;
    case "connect budget blocks" test_connect_budget_blocks;
    case "websocket connect budget blocks" test_websocket_connect_budget_blocks;
  ]

let main ~args = Test.Cli.main ~name:"blink_client_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
