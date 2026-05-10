open Std

type t =
  | Docker of Docker_client.error
  | StartupTimeout of string
  | PortNotExposed of Docker_client.Port.t
  | NoPublishedPorts
  | AmbiguousPublishedPorts of Docker_client.Port.t list
  | AddressError of string
  | UriError of string

let docker = fun error -> Docker error

let addr_error_to_string = fun error ->
  match error with
  | Net.Addr.System_error error -> IO.error_message error
  | Net.Addr.Invalid_port_number value -> "invalid port: " ^ value
  | Net.Addr.Invalid_format value -> "invalid address: " ^ value

let address = fun error -> AddressError (addr_error_to_string error)

let uri_error_to_string = fun error ->
  match error with
  | Net.Uri.InvalidScheme -> "invalid scheme"
  | Net.Uri.InvalidAuthority -> "invalid authority"
  | Net.Uri.InvalidPath -> "invalid path"
  | Net.Uri.InvalidQuery -> "invalid query"
  | Net.Uri.InvalidFragment -> "invalid fragment"
  | Net.Uri.InvalidFormat -> "invalid format"
  | Net.Uri.TooLong -> "URI too long"

let uri = fun error -> UriError (uri_error_to_string error)

let to_string = fun error ->
  match error with
  | Docker error -> Docker_client.error_to_string error
  | StartupTimeout message -> "container startup timed out: " ^ message
  | PortNotExposed port -> "container port is not exposed: " ^ Docker_client.Port.to_string port
  | NoPublishedPorts -> "container has no published ports"
  | AmbiguousPublishedPorts ports ->
      "container has multiple published ports: "
      ^ (
        ports
        |> List.map ~fn:Docker_client.Port.to_string
        |> String.concat ", "
      )
  | AddressError message -> "container address error: " ^ message
  | UriError message -> "container URI error: " ^ message
