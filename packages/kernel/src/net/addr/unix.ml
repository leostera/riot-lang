open Prelude

let ( let* ) value fn = Result.and_then value ~fn

type error =
  | InvalidPort of { port: int }
  | HostNotFound of { host: string }
  | TemporaryFailure of { host: string }
  | NoAddressesFound of { host: string; port: int }
  | InvalidSocketAddr of { ip: string; port: int }
  | ResolutionFailed of { host: string }
  | System of System_error.t

let resolver_kind_stream = 0

let resolver_kind_datagram = 1

let resolver_error_base = 4_096

let resolver_error_host_not_found = resolver_error_base + 1

let resolver_error_temporary_failure = resolver_error_base + 2

let resolver_error_resolution_failed = resolver_error_base + 3

module FFI = struct
  external resolve: string -> int -> int -> ((string * int) array, int) Result.t = "kernel_new_net_addr_resolve"
end

let error_to_string = fun value ->
  match value with
  | InvalidPort { port } -> String.concat "" [ "invalid socket port: "; Int.to_string port ]
  | HostNotFound { host } -> String.concat "" [ "host not found: "; host ]
  | TemporaryFailure { host } -> String.concat "" [ "temporary name resolution failure: "; host ]
  | NoAddressesFound { host; port } -> String.concat
    ""
    [ "no addresses found for "; host; ":"; Int.to_string port ]
  | InvalidSocketAddr { ip; port } -> String.concat
    ""
    [ "invalid socket address returned by backend: "; ip; ":"; Int.to_string port ]
  | ResolutionFailed { host } -> String.concat "" [ "name resolution failed: "; host ]
  | System error -> System_error.to_string error

let validate_port = fun port ->
  if port < 0 || port > 65_535 then
    Result.Error (InvalidPort { port })
  else
    Result.Ok ()

let socket_addr_of_pair = fun (ip_text, port) ->
  let* ip =
    match Ip_addr.from_string ip_text with
    | Result.Ok value -> Result.Ok value
    | Result.Error _ -> Result.Error (InvalidSocketAddr { ip = ip_text; port })
  in
  match Socket_addr.from_parts ~ip ~port with
  | Result.Ok addr -> Result.Ok addr
  | Result.Error _ -> Result.Error (InvalidSocketAddr { ip = ip_text; port })

let resolver_error_of_code = fun ~host code ->
  match code with
  | value when value = resolver_error_host_not_found ->
      HostNotFound { host }
  | value when value = resolver_error_temporary_failure ->
      TemporaryFailure { host }
  | value when value = resolver_error_resolution_failed ->
      ResolutionFailed { host }
  | other -> (
      match System_error.from_code other with
      | System_error.Unknown _ -> ResolutionFailed { host }
      | system_error -> System system_error
    )

let resolve_literal = fun ~host ~port ->
  match Ip_addr.from_string host with
  | Result.Error _ -> Result.Ok None
  | Result.Ok ip -> (
      match Socket_addr.from_parts ~ip ~port with
      | Result.Ok addr -> Result.Ok (Some [|addr|])
      | Result.Error _ -> Result.Error (InvalidPort { port })
    )

let resolve_all = fun ~kind ~host ~port ->
  let* () = validate_port port in
  let* literal = resolve_literal ~host ~port in
  match literal with
  | Some addrs -> Result.Ok addrs
  | None ->
      let* raw_addrs = Result.map_err (FFI.resolve host port kind) ~fn:(resolver_error_of_code ~host) in
      if Array.length raw_addrs = 0 then
        Result.Error (NoAddressesFound { host; port })
      else
        let* first_addr = socket_addr_of_pair (Array.get_unchecked raw_addrs ~at:0) in
        let out = Array.make ~count:(Array.length raw_addrs) ~value:first_addr in
        let rec build index =
          if index >= Array.length raw_addrs then
            Result.Ok out
          else
            let* addr = socket_addr_of_pair (Array.get_unchecked raw_addrs ~at:index) in
            Array.set out ~at:index ~value:addr;
            build (index + 1)
        in
        build 0

let resolve_stream = fun ~host ~port -> resolve_all ~kind:resolver_kind_stream ~host ~port

let resolve_first_stream = fun ~host ~port ->
  let* addrs = resolve_stream ~host ~port in
  Result.Ok (Array.get_unchecked addrs ~at:0)

let resolve_datagram = fun ~host ~port -> resolve_all ~kind:resolver_kind_datagram ~host ~port

let resolve_first_datagram = fun ~host ~port ->
  let* addrs = resolve_datagram ~host ~port in
  Result.Ok (Array.get_unchecked addrs ~at:0)
