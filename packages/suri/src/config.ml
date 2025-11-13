open Std

type t = {
  host : string;
  port : int;
  acceptors : int;
  max_request_line_length : int;
  max_header_count : int;
  max_header_length : int;
  buffer_size : int;
}

let default = {
  host = "0.0.0.0";
  port = 4000;
  acceptors = System.available_parallelism;
  max_request_line_length = 8192;
  max_header_count = 100;
  max_header_length = 8192;
  buffer_size = 4096;
}
