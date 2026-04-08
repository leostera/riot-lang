open Prelude

type t = string

type error = Error.t

module FFI = struct
  external is_valid: string -> bool = "kernel_new_net_ip_addr_is_valid"
end

let v4_loopback = "127.0.0.1"

let v6_loopback = "::1"

let of_string = fun value ->
  if FFI.is_valid value then
    Result.Ok value
  else
    Result.Error Error.Invalid_argument

let to_string = fun value -> value

let compare = String.compare

let equal = String.equal
