open Prelude

type t = string

type error =
  | InvalidText of { value: string }

module FFI = struct
  external is_valid: string -> bool = "kernel_new_net_ip_addr_is_valid"
end

let v4_loopback = "127.0.0.1"

let v6_loopback = "::1"

let error_to_string = fun value ->
  match value with
  | InvalidText { value } -> String.concat "" [ "invalid ip address: "; value ]

let of_string = fun value ->
  if FFI.is_valid value then
    Result.Ok value
  else
    Result.Error (InvalidText { value })

let to_string = fun value -> value

let compare = String.compare

let equal = String.equal
