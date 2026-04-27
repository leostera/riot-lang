open Std

module StdConfig = Std.Config

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
  buffer_size = 4_096;
  liveview_secret = placeholder_liveview_secret;
}

type liveview_secret_error =
  | Missing
  | TooShort of int
  | Placeholder

type error =
  | InvalidEnv of string
  | InvalidPort of int
  | InvalidAcceptors of int
  | InvalidMaxRequestLineLength of int
  | InvalidMaxHeaderCount of int
  | InvalidMaxHeaderLength of int
  | InvalidBufferSize of int
  | InvalidLiveViewSecret of liveview_secret_error

let env_to_string = function
  | Development -> "development"
  | Test -> "test"
  | Production -> "production"

let env_from_string = fun raw ->
  match String.lowercase_ascii (String.trim raw) with
  | "development"
  | "dev" -> Ok Development
  | "test" -> Ok Test
  | "production"
  | "prod" -> Ok Production
  | env -> Error (InvalidEnv env)

let liveview_secret_error_to_string = function
  | Missing -> "liveview_secret must not be empty"
  | TooShort len -> "liveview_secret must be at least 32 characters long, got " ^ Int.to_string len
  | Placeholder -> "liveview_secret must not use the default placeholder in production"

let error_to_string = function
  | InvalidEnv env -> "env must be one of development, test, or production, got '" ^ env ^ "'"
  | InvalidPort port -> "port must be between 1 and 65535, got " ^ Int.to_string port
  | InvalidAcceptors acceptors -> "acceptors must be greater than 0, got " ^ Int.to_string acceptors
  | InvalidMaxRequestLineLength value ->
      "max_request_line_length must be greater than 0, got " ^ Int.to_string value
  | InvalidMaxHeaderCount value ->
      "max_header_count must be greater than 0, got " ^ Int.to_string value
  | InvalidMaxHeaderLength value ->
      "max_header_length must be greater than 0, got " ^ Int.to_string value
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
  if config.buffer_size <= 0 then
    add (InvalidBufferSize config.buffer_size);
  List.for_each (validate_liveview_secret config.env config.liveview_secret) ~fn:add;
  match List.rev !errors with
  | [] -> Ok config
  | errors -> Error errors

(** Configuration spec for Std.Config - automatically registered *)
let spec =
  StdConfig.Spec.for_app
    ~app:"suri"
    [
      StdConfig.Spec.string
        "env"
        ~default:"development"
        ~help:"Runtime environment: development, test, or production";
      StdConfig.Spec.string "host" ~default:"0.0.0.0" ~help:"Server bind address";
      StdConfig.Spec.int "port" ~default:4_000 ~help:"Server port number";
      StdConfig.Spec.int
        "acceptors"
        ~default:Std.Thread.available_parallelism
        ~help:"Number of acceptor processes";
      StdConfig.Spec.int
        "max_request_line_length"
        ~default:8_192
        ~help:"Maximum HTTP request line length in bytes";
      StdConfig.Spec.int "max_header_count" ~default:100 ~help:"Maximum number of HTTP headers";
      StdConfig.Spec.int
        "max_header_length"
        ~default:8_192
        ~help:"Maximum HTTP header length in bytes";
      StdConfig.Spec.int "buffer_size" ~default:4_096 ~help:"Network buffer size in bytes";
      StdConfig.Spec.string
        "liveview_secret"
        ~default:placeholder_liveview_secret
        ~help:"Secret key for signing LiveView session tokens (min 32 characters)";
    ]

(** Extract typed config from validated spec values *)
let get = fun conf ->
  let env_raw = StdConfig.get_string conf "env" in
  let host = StdConfig.get_string conf "host" in
  let port = StdConfig.get_int conf "port" in
  let acceptors = StdConfig.get_int conf "acceptors" in
  let max_request_line_length = StdConfig.get_int conf "max_request_line_length" in
  let max_header_count = StdConfig.get_int conf "max_header_count" in
  let max_header_length = StdConfig.get_int conf "max_header_length" in
  let buffer_size = StdConfig.get_int conf "buffer_size" in
  let liveview_secret = StdConfig.get_string conf "liveview_secret" in
  match env_from_string env_raw with
  | Error error ->
      Error (StdConfig.ValidationError { app = "suri"; errors = [ error_to_string error ] })
  | Ok env -> (
      let config = {
        env;
        host;
        port;
        acceptors;
        max_request_line_length;
        max_header_count;
        max_header_length;
        buffer_size;
        liveview_secret;
      }
      in
      match validate config with
      | Ok config -> Ok config
      | Error errors ->
          Error (StdConfig.ValidationError {
            app = "suri";
            errors = List.map errors ~fn:error_to_string;
          })
    )
