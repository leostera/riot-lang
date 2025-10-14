open Std

type t = {
  max_request_line_length : int;
  max_header_count : int;
  max_header_length : int;
  buffer_size : int;
}

let make ?(max_request_line_length = 8192) ?(max_header_count = 100)
    ?(max_header_length = 8192) ?(buffer_size = 4096) () =
  { max_request_line_length; max_header_count; max_header_length; buffer_size }

let default = make ()
