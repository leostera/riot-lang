open Std

type t = {
  host: string;
  port: int;
  acceptors: int;
  max_request_line_length: int;
  max_header_count: int;
  max_header_length: int;
  buffer_size: int;
  liveview_secret: string;
}

let default = {
  host = "0.0.0.0";
  port = 4_000;
  acceptors = System.available_parallelism;
  max_request_line_length = 8_192;
  max_header_count = 100;
  max_header_length = 8_192;
  buffer_size = 4_096;
  liveview_secret = "INSECURE-CHANGE-ME-TO-AT-LEAST-32-CHARS";
}
(** Configuration spec for Std.Config - automatically registered *)
let spec = Config.Spec.for_app
  ~app:"suri"
  [
    Config.Spec.string "host" ~default:"0.0.0.0" ~help:"Server bind address";
    Config.Spec.int "port" ~default:4_000 ~help:"Server port number";
    Config.Spec.int "acceptors" ~default:System.available_parallelism ~help:"Number of acceptor processes";
    Config.Spec.int "max_request_line_length" ~default:8_192 ~help:"Maximum HTTP request line length in bytes";
    Config.Spec.int "max_header_count" ~default:100 ~help:"Maximum number of HTTP headers";
    Config.Spec.int "max_header_length" ~default:8_192 ~help:"Maximum HTTP header length in bytes";
    Config.Spec.int "buffer_size" ~default:4_096 ~help:"Network buffer size in bytes";
    Config.Spec.string "liveview_secret" ~help:"Secret key for signing LiveView session tokens (min 32 characters)";
  ]
(** Extract typed config from validated spec values *)
let get = fun conf ->
  let host = Config.get_string conf "host" in
  let port = Config.get_int conf "port" in
  let acceptors = Config.get_int conf "acceptors" in
  let max_request_line_length = Config.get_int conf "max_request_line_length" in
  let max_header_count = Config.get_int conf "max_header_count" in
  let max_header_length = Config.get_int conf "max_header_length" in
  let buffer_size = Config.get_int conf "buffer_size" in
  let liveview_secret = Config.get_string conf "liveview_secret" in
  (* Validate secret length *)
  if String.length liveview_secret < 32 then
    Error (Config.ValidationError {
      app = "suri";
      errors = [ "liveview_secret must be at least 32 characters long" ]
    })
  else
    Ok {
      host;
      port;
      acceptors;
      max_request_line_length;
      max_header_count;
      max_header_length;
      buffer_size;
      liveview_secret;
    }
