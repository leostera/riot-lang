type t =
  | ConfigError of string
  | UnsupportedTransport of string
  | UriError of string
  | ConnectError of string
  | HttpError of string
  | DockerError of { status: int; body: string }
  | JsonError of string
  | MissingField of string

val to_string: t -> string
