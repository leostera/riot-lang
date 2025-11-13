(** Server Configuration
    
    Compound configuration for the entire Suri server including
    network settings, HTTP limits, and protocol-specific options. *)

type t = {
  host : string;
  port : int;
  acceptors : int;
  max_request_line_length : int;
  max_header_count : int;
  max_header_length : int;
  buffer_size : int;
}

val default : t
(** Default configuration with sensible defaults *)
