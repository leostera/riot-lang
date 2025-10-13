(** Network address handling *)

open Global

include Kernel.Net.Addr

let of_host_and_port ~host ~port =
  match Kernel.Net.Addr.of_host_and_port ~host ~port with
  | Ok addr -> Ok addr
  | Error `Noop -> Error (`System_error "Failed to resolve address")
  | Error `No_info -> Error (`System_error "No address info available")
  | Error _ -> Error (`System_error "Address resolution error")

let parse s =
  (* Try to parse host:port format *)
  match String.rindex_opt s ':' with
  | None -> Error (`System_error "Invalid address format: missing port")
  | Some idx -> (
      let host = String.sub s 0 idx in
      let port_str = String.sub s (idx + 1) (String.length s - idx - 1) in
      match int_of_string_opt port_str with
      | None -> Error (`System_error "Invalid port number")
      | Some port -> (
          match of_host_and_port ~host ~port with
          | Ok addr -> Ok addr
          | Error `Noop -> Error (`System_error "Failed to parse address")
          | Error `No_info ->
              Error (`System_error "No address info available")
          | Error _ -> Error (`System_error "Address parsing error")))
