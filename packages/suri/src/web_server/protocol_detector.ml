open Std
module Cell = Sync.Cell

type error =
[
  `Detection_error of string
]

let to_string_error = function
  | `Detection_error msg -> "Protocol detection error: " ^ msg

type state = {
  config: Super.Config.t;
  handler: Http_handler.t;
  buffer: string Cell.t;
  detected: bool Cell.t;
}

let make_handler = fun ~config ~handler () ->
    {config; handler; buffer = Cell.create ""; detected = Cell.create false; }

let http1_handler =
  Socket_pool.Handler.{
    to_string_error = Http1_handler.to_string_error;
    handle_close = Http1_handler.handle_close;
    handle_connection = Http1_handler.handle_connection;
    handle_data = Http1_handler.handle_data;
    handle_error = Http1_handler.handle_error;
    handle_shutdown = Http1_handler.handle_shutdown;
    handle_message = Http1_handler.handle_message;

  }

let http2_handler =
  Socket_pool.Handler.{
    to_string_error = Http2_handler.to_string_error;
    handle_close = Http2_handler.handle_close;
    handle_connection = Http2_handler.handle_connection;
    handle_data = Http2_handler.handle_data;
    handle_error = Http2_handler.handle_error;
    handle_shutdown = Http2_handler.handle_shutdown;
    handle_message = Http2_handler.handle_message;

  }

(** Detect HTTP/2 by checking for connection preface *)
let is_http2 = fun data ->
    let preface = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" in
    if String.length data >= String.length preface then
      Some (String.sub data 0 (String.length preface) = preface)
    else
      None

(* Need more data *)

(** Detect HTTP/1.1 by checking for HTTP method *)
let is_http1 = fun data ->
    let methods = [ "GET"; "POST"; "PUT"; "DELETE"; "HEAD"; "OPTIONS"; "PATCH"; "CONNECT"; "TRACE" ] in
    List.exists (fun method_ -> String.starts_with ~prefix:method_ data) methods

let handle_connection = fun _conn state -> Socket_pool.Handler.Continue state

let handle_data = fun data conn state ->
    if Cell.get state.detected then
      Socket_pool.Handler.Error (state, `Detection_error "Handler called after protocol detection")
    else
      begin
        (* Accumulate data *)
        let current = Cell.get state.buffer in
        Cell.set state.buffer (current ^ data);
        let buffer_data = Cell.get state.buffer in
        (* Try to detect protocol *)
        match is_http2 buffer_data with
        | Some true ->
            (* HTTP/2 detected *)
            Cell.set state.detected true;
            let http2_state = Http2_handler.make_handler
              ~config:state.config
              ~handler:state.handler
              ~sniffed_data:buffer_data
              () in
            Socket_pool.Handler.Switch (Socket_pool.Handler.H {
              handler = http2_handler;
              state = http2_state;

            })
        | Some false ->
            (* Looks like HTTP/1.1 *)
            if is_http1 buffer_data then
              begin
                Cell.set state.detected true;
                let http1_state = Http1_handler.make_handler
                  ~config:state.config
                  ~handler:state.handler
                  ~sniffed_data:buffer_data
                  () in
                Socket_pool.Handler.Switch (Socket_pool.Handler.H {
                  handler = http1_handler;
                  state = http1_state;

                })
              end
            else
              Socket_pool.Handler.Error (state, `Detection_error "Unknown protocol")
        | None ->
            (* Need more data - wait for more bytes *)
            if String.length buffer_data > 100 then
              Socket_pool.Handler.Error (
                state,
                `Detection_error "Could not detect protocol after 100 bytes"
              )
            else
              Socket_pool.Handler.Continue state
      end

let handle_error = fun _error _conn state -> Socket_pool.Handler.Close state

let handle_close = fun _conn _state -> ()

let handle_shutdown = fun _conn state -> Socket_pool.Handler.Close state

let handle_message = fun _msg _conn state -> Socket_pool.Handler.Continue state
