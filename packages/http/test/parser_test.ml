open Std
open Http

let test_request_parser () =
  Log.info "Testing HTTP Request Parser...";

  (* Test 1: Simple GET request *)
  let req = "GET /path HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  (match Parser.Request.parse req with
  | Done (parsed, _) ->
      assert (parsed.method_ = "GET");
      assert (parsed.path = "/path");
      assert (parsed.version = "HTTP/1.1");
      assert (List.length parsed.headers = 1);
      assert (List.assoc "Host" parsed.headers = "example.com");
      Log.info "✓ Simple GET request parsed correctly"
  | Need_more -> Log.error "✗ Unexpected Need_more"
  | Error e -> Log.error "✗ Parse error: %s" e);

  (* Test 2: POST request with multiple headers *)
  let req =
    "POST /api/data HTTP/1.1\r\n\
     Host: api.example.com\r\n\
     Content-Type: application/json\r\n\
     Content-Length: 17\r\n\
     \r\n\
     {\"key\":\"value\"}"
  in
  (match Parser.Request.parse req with
  | Done (parsed, _) ->
      assert (parsed.method_ = "POST");
      assert (parsed.path = "/api/data");
      assert (List.length parsed.headers = 3);
      assert (List.assoc "Content-Type" parsed.headers = "application/json");
      assert (parsed.body_start = "{\"key\":\"value\"}");
      Log.info "✓ POST request with headers parsed correctly"
  | Need_more -> Log.error "✗ Unexpected Need_more"
  | Error e -> Log.error "✗ Parse error: %s" e);

  (* Test 3: Incomplete request *)
  let req = "GET /path HTTP/1.1\r\nHost: exa" in
  (match Parser.Request.parse req with
  | Need_more -> Log.info "✓ Correctly detected incomplete request"
  | Done _ -> Log.error "✗ Should have returned Need_more"
  | Error e -> Log.error "✗ Unexpected error: %s" e);

  Log.info "Request parser tests complete"

let test_response_parser () =
  Log.info "Testing HTTP Response Parser...";

  (* Test 1: Simple 200 response *)
  let resp = "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nHello" in
  (match Parser.Response.parse resp with
  | Done (parsed, _) ->
      assert (parsed.status_code = 200);
      assert (parsed.reason = "OK");
      assert (parsed.version = "HTTP/1.1");
      assert (List.assoc "Content-Length" parsed.headers = "5");
      assert (parsed.body_start = "Hello");
      Log.info "✓ 200 OK response parsed correctly"
  | Need_more -> Log.error "✗ Unexpected Need_more"
  | Error e -> Log.error "✗ Parse error: %s" e);

  (* Test 2: 404 response *)
  let resp = "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\n\r\n" in
  (match Parser.Response.parse resp with
  | Done (parsed, _) ->
      assert (parsed.status_code = 404);
      assert (parsed.reason = "Not Found");
      Log.info "✓ 404 Not Found response parsed correctly"
  | Need_more -> Log.error "✗ Unexpected Need_more"
  | Error e -> Log.error "✗ Parse error: %s" e);

  Log.info "Response parser tests complete"

let test_chunk_parser () =
  Log.info "Testing Chunked Encoding Parser...";

  (* Test 1: Single chunk *)
  let chunk = "5\r\nHello\r\n" in
  (match Parser.Chunk.parse chunk with
  | Done (data, rest) ->
      assert (data = "Hello");
      assert (rest = "");
      Log.info "✓ Single chunk parsed correctly"
  | Need_more -> Log.error "✗ Unexpected Need_more"
  | Error e -> Log.error "✗ Parse error: %s" e);

  (* Test 2: Last chunk (size 0) *)
  let chunk = "0\r\n\r\n" in
  (match Parser.Chunk.parse chunk with
  | Done (data, rest) ->
      assert (data = "");
      Log.info "✓ Last chunk parsed correctly"
  | Need_more -> Log.error "✗ Unexpected Need_more"
  | Error e -> Log.error "✗ Parse error: %s" e);

  (* Test 3: Hex chunk size *)
  let chunk = "1a\r\nabcdefghijklmnopqrstuvwxyz\r\n" in
  (match Parser.Chunk.parse chunk with
  | Done (data, rest) ->
      assert (String.length data = 26);
      (* 0x1a = 26 *)
      Log.info "✓ Hex chunk size parsed correctly"
  | Need_more -> Log.error "✗ Unexpected Need_more"
  | Error e -> Log.error "✗ Parse error: %s" e);

  Log.info "Chunk parser tests complete"

let test_sse_parser () =
  Log.info "Testing SSE Parser...";

  (* Test 1: data line *)
  (match Parser.SSE.parse_line "data: Hello World" with
  | Some event ->
      assert (event.data = "Hello World");
      assert (event.event_type = None);
      Log.info "✓ SSE data line parsed correctly"
  | None -> Log.error "✗ Failed to parse data line");

  (* Test 2: event type *)
  (match Parser.SSE.parse_line "event: message" with
  | Some event ->
      assert (event.event_type = Some "message");
      Log.info "✓ SSE event type parsed correctly"
  | None -> Log.error "✗ Failed to parse event type");

  (* Test 3: empty line *)
  (match Parser.SSE.parse_line "" with
  | None -> Log.info "✓ Empty line ignored correctly"
  | Some _ -> Log.error "✗ Should have ignored empty line");

  (* Test 4: comment *)
  (match Parser.SSE.parse_line ": this is a comment" with
  | None -> Log.info "✓ Comment ignored correctly"
  | Some _ -> Log.error "✗ Should have ignored comment");

  Log.info "SSE parser tests complete"

let () =
  Log.set_level Log.Info;

  test_request_parser ();
  print_newline ();

  test_response_parser ();
  print_newline ();

  test_chunk_parser ();
  print_newline ();

  test_sse_parser ();
  print_newline ();

  Log.info "All parser tests passed! ✓"
