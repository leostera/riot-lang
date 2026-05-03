open Std

type env =
  | Development
  | Test
  | Production

type t = {
  env: env;
  host: string;
  port: int;
  acceptors: int;
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
  liveview_secret: string;
}

let placeholder_liveview_secret = "INSECURE-CHANGE-ME-TO-AT-LEAST-32-CHARS"

let default = {
  env = Development;
  host = "0.0.0.0";
  port = 4_000;
  acceptors = Std.Thread.available_parallelism;
  max_request_line_length = 8_192;
  max_header_count = 100;
  max_header_length = 8_192;
  max_body_size = 10 * 1_024 * 1_024;
  max_keep_alive_requests = 100;
  max_websocket_frame_size = 1 * 1_024 * 1_024;
  max_websocket_message_size = 16 * 1_024 * 1_024;
  read_header_timeout_ms = 5_000;
  read_body_timeout_ms = 30_000;
  idle_timeout_ms = 60_000;
  write_timeout_ms = 30_000;
  buffer_size = 4_096;
  liveview_secret = placeholder_liveview_secret;
}

type liveview_secret_error =
  | Missing
  | TooShort of int
  | Placeholder

type invalid_env = {
  value: string;
  normalized: string;
  allowed: env list;
}

type error =
  | InvalidEnv of invalid_env
  | InvalidPort of int
  | InvalidAcceptors of int
  | InvalidMaxRequestLineLength of int
  | InvalidMaxHeaderCount of int
  | InvalidMaxHeaderLength of int
  | InvalidMaxBodySize of int
  | InvalidMaxKeepAliveRequests of int
  | InvalidMaxWebSocketFrameSize of int
  | InvalidMaxWebSocketMessageSize of int
  | InvalidReadHeaderTimeoutMs of int
  | InvalidReadBodyTimeoutMs of int
  | InvalidIdleTimeoutMs of int
  | InvalidWriteTimeoutMs of int
  | InvalidBufferSize of int
  | InvalidLiveViewSecret of liveview_secret_error

let env_to_string = fun __tmp1 ->
  match __tmp1 with
  | Development -> "development"
  | Test -> "test"
  | Production -> "production"

let allowed_envs = [ Development; Test; Production ]

let env_from_string = fun raw ->
  let normalized = String.lowercase_ascii (String.trim raw) in
  match normalized with
  | "development"
  | "dev" -> Ok Development
  | "test" -> Ok Test
  | "production"
  | "prod" -> Ok Production
  | _ -> Error (InvalidEnv { value = raw; normalized; allowed = allowed_envs })

let liveview_secret_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | Missing -> "liveview_secret must not be empty"
  | TooShort len -> "liveview_secret must be at least 32 characters long, got " ^ Int.to_string len
  | Placeholder -> "liveview_secret must not use the default placeholder in production"

let error_to_string = fun __tmp1 ->
  match __tmp1 with
  | InvalidEnv { value; allowed; _ } ->
      "env must be one of "
      ^ (
        allowed
        |> List.map ~fn:env_to_string
        |> String.concat ", "
      )
      ^ ", got '"
      ^ value
      ^ "'"
  | InvalidPort port -> "port must be between 1 and 65535, got " ^ Int.to_string port
  | InvalidAcceptors acceptors -> "acceptors must be greater than 0, got " ^ Int.to_string acceptors
  | InvalidMaxRequestLineLength value ->
      "max_request_line_length must be greater than 0, got " ^ Int.to_string value
  | InvalidMaxHeaderCount value ->
      "max_header_count must be greater than 0, got " ^ Int.to_string value
  | InvalidMaxHeaderLength value ->
      "max_header_length must be greater than 0, got " ^ Int.to_string value
  | InvalidMaxBodySize value -> "max_body_size must be greater than 0, got " ^ Int.to_string value
  | InvalidMaxKeepAliveRequests value ->
      "max_keep_alive_requests must be greater than 0, got " ^ Int.to_string value
  | InvalidMaxWebSocketFrameSize value ->
      "max_websocket_frame_size must be greater than 0, got " ^ Int.to_string value
  | InvalidMaxWebSocketMessageSize value ->
      "max_websocket_message_size must be greater than 0, got " ^ Int.to_string value
  | InvalidReadHeaderTimeoutMs value ->
      "read_header_timeout_ms must be greater than 0, got " ^ Int.to_string value
  | InvalidReadBodyTimeoutMs value ->
      "read_body_timeout_ms must be greater than 0, got " ^ Int.to_string value
  | InvalidIdleTimeoutMs value ->
      "idle_timeout_ms must be greater than 0, got " ^ Int.to_string value
  | InvalidWriteTimeoutMs value ->
      "write_timeout_ms must be greater than 0, got " ^ Int.to_string value
  | InvalidBufferSize value -> "buffer_size must be greater than 0, got " ^ Int.to_string value
  | InvalidLiveViewSecret error -> liveview_secret_error_to_string error

let errors_to_string = fun errors ->
  errors
  |> List.map ~fn:error_to_string
  |> String.concat "\n"

let validate_liveview_secret = fun env secret ->
  let trimmed = String.trim secret in
  if String.equal trimmed "" then
    [ InvalidLiveViewSecret Missing ]
  else
    let errors = ref [] in
    if String.length secret < 32 then
      errors := InvalidLiveViewSecret (TooShort (String.length secret)) :: !errors;
  if env = Production && String.equal secret placeholder_liveview_secret then
    errors := InvalidLiveViewSecret Placeholder :: !errors;
  List.rev !errors

let validate = fun config ->
  let errors = ref [] in
  let add = fun error -> errors := error :: !errors in
  if config.port < 1 || config.port > 65_535 then
    add (InvalidPort config.port);
  if config.acceptors <= 0 then
    add (InvalidAcceptors config.acceptors);
  if config.max_request_line_length <= 0 then
    add (InvalidMaxRequestLineLength config.max_request_line_length);
  if config.max_header_count <= 0 then
    add (InvalidMaxHeaderCount config.max_header_count);
  if config.max_header_length <= 0 then
    add (InvalidMaxHeaderLength config.max_header_length);
  if config.max_body_size <= 0 then
    add (InvalidMaxBodySize config.max_body_size);
  if config.max_keep_alive_requests <= 0 then
    add (InvalidMaxKeepAliveRequests config.max_keep_alive_requests);
  if config.max_websocket_frame_size <= 0 then
    add (InvalidMaxWebSocketFrameSize config.max_websocket_frame_size);
  if config.max_websocket_message_size <= 0 then
    add (InvalidMaxWebSocketMessageSize config.max_websocket_message_size);
  if config.read_header_timeout_ms <= 0 then
    add (InvalidReadHeaderTimeoutMs config.read_header_timeout_ms);
  if config.read_body_timeout_ms <= 0 then
    add (InvalidReadBodyTimeoutMs config.read_body_timeout_ms);
  if config.idle_timeout_ms <= 0 then
    add (InvalidIdleTimeoutMs config.idle_timeout_ms);
  if config.write_timeout_ms <= 0 then
    add (InvalidWriteTimeoutMs config.write_timeout_ms);
  if config.buffer_size <= 0 then
    add (InvalidBufferSize config.buffer_size);
  List.for_each (validate_liveview_secret config.env config.liveview_secret) ~fn:add;
  match List.rev !errors with
  | [] -> Ok config
  | errors -> Error errors

(** Configuration spec for Std.Config - automatically registered *)
let spec =
  Config.Spec.for_app
    ~app:"suri"
    [
      Config.Spec.string
        "env"
        ~default:"development"
        ~help:"Runtime environment: development, test, or production";
      Config.Spec.string "host" ~default:"0.0.0.0" ~help:"Server bind address";
      Config.Spec.int "port" ~default:4_000 ~help:"Server port number";
      Config.Spec.int
        "acceptors"
        ~default:Std.Thread.available_parallelism
        ~help:"Number of acceptor processes";
      Config.Spec.int
        "max_request_line_length"
        ~default:8_192
        ~help:"Maximum HTTP request line length in bytes";
      Config.Spec.int "max_header_count" ~default:100 ~help:"Maximum number of HTTP headers";
      Config.Spec.int "max_header_length" ~default:8_192 ~help:"Maximum HTTP header length in bytes";
      Config.Spec.int
        "max_body_size"
        ~default:(10 * 1_024 * 1_024)
        ~help:"Maximum HTTP request body size in bytes";
      Config.Spec.int
        "max_keep_alive_requests"
        ~default:100
        ~help:"Maximum requests allowed per keep-alive connection";
      Config.Spec.int
        "max_websocket_frame_size"
        ~default:(1 * 1_024 * 1_024)
        ~help:"Maximum WebSocket frame payload size in bytes";
      Config.Spec.int
        "max_websocket_message_size"
        ~default:(16 * 1_024 * 1_024)
        ~help:"Maximum reassembled WebSocket message size in bytes";
      Config.Spec.int
        "read_header_timeout_ms"
        ~default:5_000
        ~help:"Maximum time to wait for HTTP request headers in milliseconds";
      Config.Spec.int
        "read_body_timeout_ms"
        ~default:30_000
        ~help:"Maximum time to wait for HTTP request bodies in milliseconds";
      Config.Spec.int
        "idle_timeout_ms"
        ~default:60_000
        ~help:"Maximum idle keep-alive time in milliseconds";
      Config.Spec.int
        "write_timeout_ms"
        ~default:30_000
        ~help:"Maximum time to wait for response writes in milliseconds";
      Config.Spec.int "buffer_size" ~default:4_096 ~help:"Network buffer size in bytes";
      Config.Spec.string
        "liveview_secret"
        ~default:placeholder_liveview_secret
        ~help:"Secret key for signing LiveView session tokens (min 32 characters)";
    ]

(** Extract typed config from validated spec values *)
let get = fun conf ->
  let env_raw = Config.get_string conf "env" in
  let host = Config.get_string conf "host" in
  let port = Config.get_int conf "port" in
  let acceptors = Config.get_int conf "acceptors" in
  let max_request_line_length = Config.get_int conf "max_request_line_length" in
  let max_header_count = Config.get_int conf "max_header_count" in
  let max_header_length = Config.get_int conf "max_header_length" in
  let max_body_size = Config.get_int conf "max_body_size" in
  let max_keep_alive_requests = Config.get_int conf "max_keep_alive_requests" in
  let max_websocket_frame_size = Config.get_int conf "max_websocket_frame_size" in
  let max_websocket_message_size = Config.get_int conf "max_websocket_message_size" in
  let read_header_timeout_ms = Config.get_int conf "read_header_timeout_ms" in
  let read_body_timeout_ms = Config.get_int conf "read_body_timeout_ms" in
  let idle_timeout_ms = Config.get_int conf "idle_timeout_ms" in
  let write_timeout_ms = Config.get_int conf "write_timeout_ms" in
  let buffer_size = Config.get_int conf "buffer_size" in
  let liveview_secret = Config.get_string conf "liveview_secret" in
  match env_from_string env_raw with
  | Error error ->
      Error (Config.ValidationError { app = "suri"; errors = [ error_to_string error ] })
  | Ok env -> (
      let config = {
        env;
        host;
        port;
        acceptors;
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
        liveview_secret;
      }
      in
      match validate config with
      | Ok config -> Ok config
      | Error errors ->
          Error (Config.ValidationError {
            app = "suri";
            errors = List.map errors ~fn:error_to_string;
          })
    )
