open Std

type t =
  | ConfigError of string
  | UnsupportedTransport of string
  | UriError of string
  | ConnectError of string
  | HttpError of string
  | DockerError of { status: int; body: string }
  | JsonError of string
  | MissingField of string

let to_string = fun error ->
  match error with
  | ConfigError message -> "docker config error: " ^ message
  | UnsupportedTransport value -> "unsupported Docker transport: " ^ value
  | UriError message -> "invalid Docker API URI: " ^ message
  | ConnectError message -> "failed to connect to Docker daemon: " ^ message
  | HttpError message -> "Docker HTTP request failed: " ^ message
  | DockerError { status; body } ->
      "Docker daemon returned HTTP " ^ Std.Int.to_string status ^ ": " ^ body
  | JsonError message -> "Docker JSON error: " ^ message
  | MissingField field -> "Docker response missing field: " ^ field
