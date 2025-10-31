open Std

module Web_config = Config  (* Local Config module in web_server/ *)

type handler = Socket_pool.Connection.t -> Request.t -> Response.t

type state = {
  config : Web_config.t;
  handler : handler;
  is_keep_alive : bool;
  requests_processed : int;
  sniffed_data : string;
}

type error = [ `ParseError of string | `ExcessBodyRead | `IoError of string ]

let to_string_error = function
  | `ParseError msg -> format "Parse error: %s" msg
  | `ExcessBodyRead -> "Excess body read"
  | `IoError msg -> format "I/O error: %s" msg

let make_handler ~config ~handler ?(sniffed_data = "") () =
  {
    config;
    handler;
    sniffed_data;
    is_keep_alive = false;
    requests_processed = 0;
  }

let handle_close _conn _state = ()
let handle_connection _conn state = Socket_pool.Handler.Continue state

let should_keep_alive (req : Request.t) =
  match
    (Request.version req, Net.Http.Header.get (Request.headers req) "connection")
  with
  | _, Some "close" -> false
  | _, Some "keep-alive" -> true
  | Http11, _ -> true
  | _, _ -> false

let send_response conn (res : Response.t) =
  let headers = Net.Http.Header.add res.headers "vary" "accept-encoding" in

  let body_len = String.length res.body |> Int.to_string in
  let headers =
    match res.status with
    | NoContent | NotModified -> Net.Http.Header.remove headers "content-length"
    | _ when not (Net.Http.Header.has headers "content-length") ->
        Net.Http.Header.set headers "content-length" body_len
    | _ -> headers
  in

  let headers = headers in

  let status_line =
    format "%s %d %s\r\n"
      (Net.Http.Version.to_string res.version)
      (Net.Http.Status.to_int res.status)
      (Net.Http.Status.to_string res.status)
  in

  let header_lines =
    Net.Http.Header.to_list headers
    |> List.map (fun (k, v) -> format "%s: %s\r\n" k v)
    |> String.concat ""
  in

  let response_bytes = format "%s%s\r\n%s" status_line header_lines res.body in

  match Socket_pool.Connection.send conn response_bytes with
  | Ok () -> Ok ()
  | Error `Closed -> Error (`IoError "Connection closed")

let handle_request state conn (req : Request.t) =
  let res = state.handler conn req in

  match send_response conn res with
  | Ok () ->
      let is_keep_alive = should_keep_alive req in
      let requests_processed = state.requests_processed + 1 in
      let new_state =
        { state with is_keep_alive; requests_processed; sniffed_data = "" }
      in

      if is_keep_alive then Socket_pool.Handler.Continue new_state
      else Socket_pool.Handler.Close new_state
  | Error err -> Socket_pool.Handler.Error (state, err)

let handle_data data conn state =
  let full_data = state.sniffed_data ^ data in

  match
    Http.Http1.Request.parse
      ~max_request_line:state.config.max_request_line_length
      ~max_headers:state.config.max_header_count
      ~max_header_length:state.config.max_header_length full_data
  with
  | Done { value = http_req; remaining } ->
      let req = Request.of_http ~body:remaining http_req in
      handle_request state conn req
  | Need_more ->
      Socket_pool.Handler.Continue { state with sniffed_data = full_data }
  | Error msg ->
      let res = Response.bad_request ~body:msg () in
      let _ = send_response conn res in
      Socket_pool.Handler.Close state

let handle_error err _conn state =
  Log.error "HTTP/1.1 error: %s" (to_string_error err);
  Socket_pool.Handler.Close state

let handle_shutdown _conn state = Socket_pool.Handler.Close state
let handle_message _msg _conn state = Socket_pool.Handler.Continue state
