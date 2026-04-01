module Web_config = Config
open Std

type parse_state =
  | WaitingForHeaders
  | WaitingForBody of {
      http_req: Net.Http.Request.t;
      expected_length: int;
      accumulated_body: string;
    }

type state = {
  config: Web_config.t;
  handler: Http_handler.t;
  is_keep_alive: bool;
  requests_processed: int;
  sniffed_data: string;
  parse_state: parse_state;
}

type error =
[
  `ParseError of string
  | `ExcessBodyRead
  | `IoError of string
]

let to_string_error = function
  | `ParseError msg -> "Parse error: " ^ msg
  | `ExcessBodyRead -> "Excess body read"
  | `IoError msg -> "I/O error: " ^ msg

let make_handler = fun ~config ~handler ?(sniffed_data = "") () ->
  {
    config;
    handler;
    sniffed_data;
    is_keep_alive = false;
    requests_processed = 0;
    parse_state = WaitingForHeaders;
  }

let handle_close = fun _conn _state -> ()

let handle_connection = fun _conn state -> Socket_pool.Handler.Continue state

let should_keep_alive = fun (req: Request.t) ->
  match (Request.version req, Net.Http.Header.get (Request.headers req) "connection") with
  | _, Some "close" -> false
  | _, Some "keep-alive" -> true
  | Http11, _ -> true
  | _, _ -> false

let send_response = fun conn (res: Response.t) ->
  let headers = Net.Http.Header.add res.headers "vary" "accept-encoding" in
  let body_len = String.length res.body |> Int.to_string in
  let headers =
    match res.status with
    | NoContent
    | NotModified -> Net.Http.Header.remove headers "content-length"
    | _ when not (Net.Http.Header.has headers "content-length") -> Net.Http.Header.set
      headers
      "content-length"
      body_len
    | _ -> headers
  in
  let status_line = (Net.Http.Version.to_string res.version)
  ^ " "
  ^ (Int.to_string (Net.Http.Status.to_int res.status))
  ^ " "
  ^ (Net.Http.Status.to_string res.status)
  ^ "\r\n" in
  let header_lines = Net.Http.Header.to_list headers
  |> List.map (fun ((k, v)) -> k ^ ": " ^ v ^ "\r\n")
  |> String.concat "" in
  let response_bytes = status_line ^ header_lines ^ "\r\n" ^ res.body in
  match Socket_pool.Connection.send conn response_bytes with
  | Ok () -> Ok ()
  | Error `Closed -> Error (`IoError "Connection closed")

let compute_websocket_accept = fun key ->
  let magic = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" in
  let concat = key ^ magic in
  let hash = Crypto.Sha1.hash_string concat in
  let hash_bytes = Kernel.Crypto.Hash.to_bytes hash in
  Data.Base64.encode_bytes hash_bytes

(* Bridge Channel.Handler.t to Socket_pool.Handler.t for WebSocket connections *)

let websocket_to_socket_pool_handler : Channel.Handler.t -> Socket_pool.Handler.t = fun ws_handler ->
  let handler = {
    Socket_pool.Handler.to_string_error = (fun (`Unknown_opcode code) ->
      "Unknown WebSocket opcode: " ^ string_of_int code);
    handle_close = (fun _conn _state -> ());
    handle_connection =
      (fun conn ws_handler ->
        (* Initialize the WebSocket handler *)
        Log.info "WebSocket bridge: handle_connection called, initializing Channel handler";
        match Channel.Handler.init ws_handler (Socket_pool.Connection.stream conn) with
        | `continue (_stream, new_handler) ->
            Log.info "WebSocket bridge: Channel handler initialized successfully";
            Socket_pool.Handler.Continue new_handler
        | `error (_stream, err) ->
            Log.error "WebSocket bridge: Channel handler initialization failed";
            Socket_pool.Handler.Error (ws_handler, err));
    handle_data =
      (fun data conn ws_handler ->
        (* Parse WebSocket frames from incoming data *)
        let stream = Socket_pool.Connection.stream conn in
        match Http.Ws.Parser.parse data with
        | Done { value=frame; remaining=_ } -> (* Process frame through the Channel handler *)
          (
            match Channel.Handler.handle_frame ws_handler frame stream with
            | `continue (_stream, new_handler) ->
                Socket_pool.Handler.Continue new_handler
            | `push (out_frames, new_handler) ->
                (* Serialize and send response frames *)
                let frame_data = out_frames
                |> List.map Http.Ws.Serializer.serialize
                |> String.concat "" in
                (
                  match Socket_pool.Connection.send conn frame_data with
                  | Ok () -> Socket_pool.Handler.Continue new_handler
                  | Error `Closed -> Socket_pool.Handler.Close new_handler
                )
            | `error (_stream, err) ->
                Socket_pool.Handler.Error (ws_handler, err)
          )
        | Need_more ->
            (* Need more data - keep waiting *)
            Socket_pool.Handler.Continue ws_handler
        | Error msg ->
            (* Frame parse error - close connection *)
            Log.error ("WebSocket frame parse error: " ^ msg);
            Socket_pool.Handler.Close ws_handler);
    handle_error = (fun err _conn state -> Socket_pool.Handler.Error (state, err));
    handle_shutdown = (fun _conn state -> Socket_pool.Handler.Close state);
    handle_message =
      (fun msg conn ws_handler ->
        let stream = Socket_pool.Connection.stream conn in
        match Channel.Handler.handle_message ws_handler msg stream with
        | `continue (_stream, new_handler) ->
            Socket_pool.Handler.Continue new_handler
        | `push (frames, new_handler) ->
            (* Serialize and send frames *)
            let frame_data = frames |> List.map Http.Ws.Serializer.serialize |> String.concat "" in
            (
              match Socket_pool.Connection.send conn frame_data with
              | Ok () -> Socket_pool.Handler.Continue new_handler
              | Error `Closed -> Socket_pool.Handler.Close new_handler
            )
        | `error (_stream, err) ->
            Socket_pool.Handler.Error (ws_handler, err));
  }
  in
  Socket_pool.Handler.H {handler;state = ws_handler;}

let handle_websocket_upgrade = fun state socket_conn req ws_handler ->
  (* Check for required WebSocket headers *)
  let headers = Request.headers req in
  match Net.Http.Header.get headers "sec-websocket-key" with
  | None ->
      let res = Response.bad_request ~body:"Missing Sec-WebSocket-Key header" () in
      let _ = send_response socket_conn res in
      Socket_pool.Handler.Close state
  | Some key ->
      (* Compute accept key *)
      let accept_key = compute_websocket_accept key in
      (* Send 101 Switching Protocols response *)
      let response_headers =
        Net.Http.Header.empty
        |> (fun h ->
          Net.Http.Header.set h "Upgrade" "websocket")
        |> (fun h ->
          Net.Http.Header.set h "Connection" "Upgrade")
        |> (fun h ->
          Net.Http.Header.set h "Sec-WebSocket-Accept" accept_key)
      in
      let status_line = "HTTP/1.1 101 Switching Protocols\r\n" in
      let header_lines = Net.Http.Header.to_list response_headers
      |> List.map (fun ((k, v)) -> k ^ ": " ^ v ^ "\r\n")
      |> String.concat "" in
      let response_bytes = status_line ^ header_lines ^ "\r\n" in
      Log.info
        ("Sending WebSocket upgrade response (" ^ string_of_int (String.length response_bytes) ^ " bytes)");
      match Socket_pool.Connection.send socket_conn response_bytes with
      | Ok () ->
          Log.info "WebSocket upgrade response sent successfully, switching protocols";
          (* Switch to WebSocket handler *)
          let socket_pool_handler = websocket_to_socket_pool_handler ws_handler in
          Socket_pool.Handler.Switch socket_pool_handler
      | Error `Closed ->
          Log.error "Failed to send WebSocket upgrade response - connection closed";
          Socket_pool.Handler.Close state

let handle_request = fun state socket_conn (req: Request.t) ->
  match state.handler socket_conn req with
  | Http_handler.Response res -> (* Normal HTTP response *)
    (
      match send_response socket_conn res with
      | Ok () ->
          let is_keep_alive = should_keep_alive req in
          let requests_processed = state.requests_processed + 1 in
          let new_state = {
            state
            with is_keep_alive;
            requests_processed;
            parse_state = WaitingForHeaders;
          } in
          if is_keep_alive then
            Socket_pool.Handler.Continue new_state
          else
            Socket_pool.Handler.Close new_state
      | Error err -> Socket_pool.Handler.Error (state, err)
    )
  | Http_handler.Upgrade (Http_handler.WebSocket (_opts, ws_handler)) ->
      (* WebSocket upgrade *)
      Log.info "Http1_handler.handle_request: Matched WebSocket upgrade, calling handle_websocket_upgrade";
      handle_websocket_upgrade state socket_conn req ws_handler

let get_content_length = fun http_req ->
  match Net.Http.Request.get_header http_req "content-length" with
  | Some len_str -> (
      match int_of_string_opt len_str with
      | Some len when len >= 0 -> len
      | _ -> 0
    )
  | None -> 0

let handle_data_waiting_headers = fun full_data conn state ->
  match Http.Http1.Request.parse
    ~max_request_line:state.config.max_request_line_length
    ~max_headers:state.config.max_header_count
    ~max_header_length:state.config.max_header_length
    full_data with
  | Done { value=http_req; remaining } ->
      let expected_length = get_content_length http_req in
      let body_received = String.length remaining in
      if body_received >= expected_length then
        let req = Request.of_http ~body:remaining http_req in
        handle_request {state with parse_state = WaitingForHeaders;sniffed_data = "";} conn req
      else
        (* Need to read more body data - transition to WaitingForBody state *)
        Socket_pool.Handler.Continue {
          state
          with sniffed_data = "";
          parse_state = WaitingForBody {http_req;expected_length;accumulated_body = remaining;};
        }
  | Need_more ->
      Socket_pool.Handler.Continue {state with sniffed_data = full_data;}
  | Error msg ->
      let res = Response.bad_request ~body:msg () in
      let _ = send_response conn res in
      Socket_pool.Handler.Close state

let handle_data_waiting_body = fun data conn state http_req expected_length accumulated_body ->
  let new_body = accumulated_body ^ data in
  let body_length = String.length new_body in
  if body_length >= expected_length then
    let complete_body = String.sub new_body 0 expected_length in
    let remaining_data =
      if body_length > expected_length then
        String.sub new_body expected_length (body_length - expected_length)
      else
        ""
    in
    (* Process the request with complete body *)
    let req = Request.of_http ~body:complete_body http_req in
    let result = handle_request
      {state with parse_state = WaitingForHeaders;sniffed_data = remaining_data;}
      conn
      req in
    (* If there's remaining data and we're keeping the connection alive, it might be the start of the next request *)
    result
  else
    (* Still need more data *)
    Socket_pool.Handler.Continue {
      state
      with parse_state = WaitingForBody {http_req;expected_length;accumulated_body = new_body;};
    }

let handle_data = fun data conn state ->
  match state.parse_state with
  | WaitingForHeaders ->
      let full_data = state.sniffed_data ^ data in
      handle_data_waiting_headers full_data conn state
  | WaitingForBody { http_req; expected_length; accumulated_body } -> handle_data_waiting_body
    data
    conn
    state
    http_req
    expected_length
    accumulated_body

let handle_error = fun err _conn state ->
  Log.error ("HTTP/1.1 error: " ^ (to_string_error err));
  Socket_pool.Handler.Close state

let handle_shutdown = fun _conn state -> Socket_pool.Handler.Close state

let handle_message = fun _msg _conn state -> Socket_pool.Handler.Continue state
