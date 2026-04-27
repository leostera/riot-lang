open Std

type t = {
  max_request_line_length: int;
  max_header_count: int;
  max_header_length: int;
  max_body_size: int;
  max_keep_alive_requests: int;
  max_websocket_frame_size: int;
  max_websocket_message_size: int;
  read_header_timeout_ms: int;
  read_body_timeout_ms: int;
  idle_timeout_ms: int;
  write_timeout_ms: int;
  buffer_size: int;
}

let make = fun
  ?(max_request_line_length = 8_192)
  ?(max_header_count = 100)
  ?(max_header_length = 8_192)
  ?(max_body_size = 10 * 1_024 * 1_024)
  ?(max_keep_alive_requests = 100)
  ?(max_websocket_frame_size = 1 * 1_024 * 1_024)
  ?(max_websocket_message_size = 16 * 1_024 * 1_024)
  ?(read_header_timeout_ms = 5_000)
  ?(read_body_timeout_ms = 30_000)
  ?(idle_timeout_ms = 60_000)
  ?(write_timeout_ms = 30_000)
  ?(buffer_size = 4_096)
  () ->
  {
    max_request_line_length;
    max_header_count;
    max_header_length;
    max_body_size;
    max_keep_alive_requests;
    max_websocket_frame_size;
    max_websocket_message_size;
    read_header_timeout_ms;
    read_body_timeout_ms;
    idle_timeout_ms;
    write_timeout_ms;
    buffer_size;
  }

let default = make ()
