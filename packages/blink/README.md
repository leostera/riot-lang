# Blink - HTTP Client for Riot

Lightweight, streaming HTTP client built on Riot's process model with support for HTTP/1.1, chunked transfer encoding, and incremental response processing.

## Features

- **Streaming responses** - Process data as it arrives without buffering
- **Chunked transfer encoding** - Full support for streaming APIs
- **Message-based API** - Receive status, headers, and data as separate messages
- **Three abstraction levels** - Low-level streaming, batch processing, or buffered responses
- **Managed client controls** - Configure request budgets, telemetry, and connection reuse in `Blink.Client`
- **Built on Std** - Uses `Net.TcpStream`, `IO.Reader/Writer`, and HTTP parsers from `packages/http`

## Quick Start

### Simple Request/Response

```ocaml
open Std


let () = start ~apps:[] @@ fun () ->
  let open Result.Syntax in

  (* Parse URI *)
  let* uri = Net.Uri.from_string "http://example.com" in

  (* Connect *)
  let* conn = Blink.connect uri in

  (* Create and send request *)
  let req = Net.Http.Request.create Net.Http.Method.Get uri in
  let* () = Blink.request conn req () in

  (* Get full response *)
  let* (response, body) = Blink.await conn in

  Log.info "Status: %a" Net.Http.Status.pp (Net.Http.Response.status response);
  Log.info "Body length: %d bytes" (String.length body);

  Blink.close conn;
  Ok ()
```

### Streaming Response

Process chunks as they arrive without buffering the entire response:

```ocaml
let () = start ~apps:[] @@ fun () ->
  let open Result.Syntax in

  let* uri = Net.Uri.from_string "http://example.com/large-file" in
  let* conn = Blink.connect uri in

  let req = Net.Http.Request.create Net.Http.Method.Get uri in
  let* () = Blink.request conn req () in

  (* Stream chunks incrementally *)
  let rec process_stream () =
    match Blink.stream conn with
    | Error e -> Error e
    | Ok messages ->
        List.for_each messages ~fn:(function
          | `Status status ->
              Log.info "Status: %a" Net.Http.Status.pp status
          | `Headers headers ->
              Log.info "Received %d headers" (Net.Http.Header.length headers)
          | `Data chunk ->
              (* Process chunk immediately - no buffering *)
              write_to_file chunk
          | `Done ->
              Log.info "Stream complete"
        );

        if List.mem `Done messages then Ok ()
        else process_stream ()
  in

  let* () = process_stream () in
  Blink.close conn;
  Ok ()
```

### Progress Monitoring

```ocaml
let download_with_progress url =
  let open Result.Syntax in

  let* uri = Net.Uri.from_string url in
  let* conn = Blink.connect uri in

  let req = Net.Http.Request.create Net.Http.Method.Get uri in
  let* () = Blink.request conn req () in

  let total_bytes = ref 0 in
  let on_progress msgs =
    let bytes = List.fold_left (fun acc -> function
      | `Data chunk -> acc + String.length chunk
      | _ -> acc
    ) 0 msgs in
    total_bytes := !total_bytes + bytes;
    if bytes > 0 then
      Log.info "Downloaded: %d bytes" !total_bytes
  in

  let* (response, body) = Blink.await ~on_message:on_progress conn in
  Blink.close conn;
  Ok (response, body)
```

### POST Request with Body

```ocaml
let post_json url data =
  let open Result.Syntax in

  let* uri = Net.Uri.from_string url in
  let* conn = Blink.connect uri in

  let body = Data.Json.to_string data in
  let req = Net.Http.Request.create Net.Http.Method.Post uri
    |> Net.Http.Request.add_header "content-type" "application/json"
  in

  let* () = Blink.request conn req ~body () in
  let* (response, response_body) = Blink.await conn in

  Blink.close conn;
  Ok (response, response_body)
```

### Managed Client

Use `Blink.Client` when callers should share request budgets, telemetry, and connection reuse policy.

```ocaml
let pool = Blink.Client.Config.pool ~max_idle_per_endpoint:4 () in
let config =
  Blink.Client.Config.make
    ~connection_policy:(Blink.Client.Config.Pool pool)
    ()
in
let client = Blink.Client.make ~config () in

let req =
  Blink.Client.Request.make
    ~method_:Blink.Client.Request.Get
    ~url:"https://example.com"
    ()
in

match Blink.Client.execute client req with
| Ok (response, telemetry) ->
    Log.info "Status: %d events=%d" response.status (List.length telemetry.lifecycle)
| Error error ->
    Log.error "Request failed: %s" (Blink.Client.error_to_string error)
```

`Blink.Client` also exposes the same connection-oriented surface as the top-level module:
`connect`, `request`, `stream`, `messages`, `await`, and `close`. Use this path when raw
HTTP streams should still share the client's budget and pooling configuration.
SSE and WebSocket helpers are available through `Blink.Client.SSE` and
`Blink.Client.WebSocket`.

Examples:

- `examples/managed_client.ml` - buffered managed HTTP
- `examples/managed_sse.ml` - managed connection plus SSE iterator
- `examples/managed_websocket.ml` - managed WebSocket connection

Validation:

```sh
riot build -p blink --all --json
riot test -p blink --json
riot bench -p blink --json
```

## API Reference

### Connection Management

#### `connect : Net.Uri.t -> (Connection.t, error) result`

Establish TCP connection to the URI's host and port.

**Example:**
```ocaml
let* uri = Net.Uri.from_string "http://api.example.com:8080" in
let* conn = Blink.connect uri in
(* conn is ready for requests *)
```

#### `close : Connection.t -> unit`

Close the TCP connection.

**Example:**
```ocaml
Blink.close conn
```

### Request/Response

#### `request : Connection.t -> Net.Http.Request.t -> ?body:string -> unit -> (unit, error) result`

Send HTTP request over the connection. The connection must be used to read the response afterwards.

**Parameters:**
- `conn` - Active connection
- `req` - HTTP request with method, headers, version
- `body` - Optional request body (for POST, PUT, etc)

**Example:**
```ocaml
let req = Net.Http.Request.create Net.Http.Method.Post uri
  |> Net.Http.Request.add_header "content-type" "application/json"
in
let* () = Blink.request conn req ~body:"{\"key\":\"value\"}" () in
(* now read response *)
```

### Streaming API (Low-Level)

#### `stream : Connection.t -> (message list, error) result`

Read next chunk of response stream. Returns list of messages representing parts of the HTTP response.

**Messages:**
- `` `Status of Net.Http.Status.t`` - HTTP status code
- `` `Headers of Net.Http.Header.t`` - Response headers
- `` `Data of string`` - Chunk of body data
- `` `Done`` - Response complete

**Example:**
```ocaml
match Blink.stream conn with
| Ok messages ->
    List.for_each messages ~fn:(function
      | `Status s -> handle_status s
      | `Headers h -> handle_headers h
      | `Data chunk -> process_chunk chunk
      | `Done -> finish ()
    )
| Error e -> handle_error e
```

**Use cases:**
- Streaming large files
- Processing data incrementally
- Real-time data feeds
- Memory-efficient downloads

### Batch API (Mid-Level)

#### `messages : ?on_message:(message list -> unit) -> Connection.t -> (message list, error) result`

Stream entire response and collect all messages. Calls `stream` repeatedly until `` `Done`` received.

**Parameters:**
- `on_message` - Optional callback invoked for each batch of messages (for progress tracking)

**Example:**
```ocaml
let on_batch msgs =
  Log.debug "Received batch of %d messages" (List.length msgs)
in

match Blink.messages ~on_message:on_batch conn with
| Ok all_messages ->
    (* Process complete list of messages *)
    List.for_each all_messages ~fn:handle_message
| Error e -> handle_error e
```

**Use cases:**
- Collecting response in batches
- Progress reporting
- Metrics collection

### Buffered API (High-Level)

#### `await : ?on_message:(message list -> unit) -> Connection.t -> (Net.Http.Response.t * string, error) result`

Stream full response, buffer body, and build complete Response object. Most convenient for simple request/response cycles.

**Parameters:**
- `on_message` - Optional progress callback

**Returns:**
- `Response.t` - Complete HTTP response object with status, headers, version
- `string` - Full response body

**Example:**
```ocaml
match Blink.await conn with
| Ok (response, body) ->
    let status = Net.Http.Response.status response in
    let headers = Net.Http.Response.headers response in
    Log.info "Status: %a" Net.Http.Status.pp status;
    Log.info "Body: %s" body
| Error e -> handle_error e
```

**Use cases:**
- Simple request/response
- Small responses that fit in memory
- APIs returning complete JSON/XML documents

## Error Handling

```ocaml
type error =
  [ Net.error              (* `Connection_refused | `Closed | `System_error *)
  | `Parse_error of string (* HTTP parsing failed *)
  | `Protocol_error of string (* Protocol violation *)
  | `Eof ]                 (* Unexpected end of stream *)
```

**Error handling example:**
```ocaml
match Blink.connect uri with
| Error `Connection_refused -> Log.error "Server refused connection"
| Error `Closed -> Log.error "Connection closed"
| Error (`System_error msg) -> Log.error "System error: %s" msg
| Error (`Parse_error msg) -> Log.error "Parse error: %s" msg
| Error `Eof -> Log.error "Unexpected EOF"
| Ok conn -> (* proceed *)
```

## Choosing the Right API Level

### Use `stream` when:
- Processing large files that don't fit in memory
- Implementing streaming protocols (SSE, chunked responses)
- Need maximum control over memory usage
- Real-time data processing

### Use `messages` when:
- Need to collect response in batches
- Implementing progress bars or metrics
- Processing complete response but want progress updates

### Use `await` when:
- Simple request/response patterns
- Response fits comfortably in memory
- Don't need incremental processing
- Want convenience over control

## Implementation Details

### Architecture

```
User Code
    ↓
Blink API (blink.ml)
    ↓
Connection (connection.ml)
    ↓
Net.TcpStream ← IO.Reader/Writer
    ↓
Kernel.Net (syscalls)
```

### Streaming Design

Blink uses a state machine to handle HTTP responses incrementally:

1. **Waiting_for_headers** - Parsing HTTP status and headers
2. **Reading_fixed_body** - Reading body with Content-Length
3. **Reading_chunked_body** - Reading chunked transfer encoding
4. **Complete** - Response fully received

Each call to `stream` advances the state machine and returns available messages.

### Chunked Transfer Encoding

Fully supported via `Http.Http1.Chunk` parser. Blink automatically detects `Transfer-Encoding: chunked` header and parses chunk frames:

```
[chunk-size]\r\n
[chunk-data]\r\n
[chunk-size]\r\n
[chunk-data]\r\n
0\r\n
\r\n
```

### Memory Efficiency

- Streaming API processes data without buffering entire response
- Internal buffer (4KB) for HTTP parsing only
- User controls memory usage by handling chunks immediately
- Chunked encoding parsed incrementally

## Examples

See `packages/blink/test/` for complete working examples:

- `simple_get.ml` - Basic GET request
- `streaming_large_file.ml` - Streaming download
- `post_json.ml` - POST with JSON body
- `chunked_response.ml` - Handling chunked encoding
- `progress_bar.ml` - Download with progress tracking

## Limitations

- HTTP/1.1 only (no HTTP/2 yet)
- No automatic redirect following
- Managed connection pooling is exact-URL scoped
- Low-level APIs leave connection lifecycle to the caller

## Future Work

- [ ] HTTP/2 support via `Http.Http2` parser
- [ ] WebSocket upgrade support
- [ ] Automatic redirect following
- [ ] Cookie management
- [ ] Compression support (gzip, deflate)
