module Config = Config

module Request = Request

module Response = Response

module Handler = Http_handler

module Http1 = Http1_handler

module Http2 = Http2_handler

module ProtocolDetector = Protocol_detector

(**
   Start an HTTP/1.1 server with supervision.

   This is the main entry point for starting a Suri web server.
   It creates a supervised socket pool that handles HTTP/1.1 connections.

   @param config Server configuration (limits, buffer sizes, etc.)
   @param handler Your HTTP request handler function
   @param host Host to bind to (e.g., "0.0.0.0" for all interfaces)
   @param port Port to listen on
   @param acceptors Number of concurrent acceptor processes (defaults to available parallelism)
   @return Ok supervisor_pid or Error if binding fails
*)
let start_link = fun ?(host = "0.0.0.0") ~port ?(acceptors = Std.Thread.available_parallelism) ~config ~handler () ->
  let handler_state = Http1.make_handler ~config ~handler () in
  let socket_handler: (Http1.state, Http1.error) Socket_pool.Handler.handler = {
    handle_connection = Http1.handle_connection;
    handle_data = Http1.handle_data;
    handle_close = Http1.handle_close;
    handle_error = Http1.handle_error;
    handle_shutdown = Http1.handle_shutdown;
    handle_message = Http1.handle_message;
    to_string_error = Http1.to_string_error
  }
  in
  Socket_pool.start_link ~host ~port ~acceptors ~buffer_size:config.Config.buffer_size socket_handler handler_state
