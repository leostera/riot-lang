module Port = Port
module Config = Config
module Client = Client
module Image = Image
module Container = Container
module Testing = Testing

type error = Error.t =
  | ConfigError of string
  | UnsupportedTransport of string
  | UriError of string
  | ConnectError of string
  | HttpError of string
  | DockerError of { status: int; body: string }
  | JsonError of string
  | MissingField of string

let error_to_string = Error.to_string
