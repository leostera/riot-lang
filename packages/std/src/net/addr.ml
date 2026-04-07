(** Network address handling *)
open Global

include Kernel.Net.Addr

type error =
  | System_error of IO.error
  | Invalid_port_number of string
  | Invalid_format of string

let of_host_and_port = fun ~host ~port ->
  match Kernel.Net.Addr.of_host_and_port ~host ~port with
  | Ok addr -> Ok addr
  | Error err -> Error (System_error err)

let of_host_and_port_datagram = fun ~host ~port ->
  match Kernel.Net.Addr.of_host_and_port_datagram ~host ~port with
  | Ok addr -> Ok addr
  | Error err -> Error (System_error err)

let parse = fun s ->
  (* Try to parse host:port format *)
  match String.rindex_opt s ':' with
  | None -> Error (Invalid_format "missing port")
  | Some idx -> (
      let host = String.sub s 0 idx in
      let port_str = String.sub s (idx + 1) (String.length s - idx - 1) in
      match int_of_string_opt port_str with
      | None -> Error (Invalid_port_number port_str)
      | Some port -> (
          match of_host_and_port ~host ~port with
          | Ok addr -> Ok addr
          | Error err -> Error err
        )
    )

let parse_datagram = fun s ->
  match String.rindex_opt s ':' with
  | None -> Error (Invalid_format "missing port")
  | Some idx -> (
      let host = String.sub s 0 idx in
      let port_str = String.sub s (idx + 1) (String.length s - idx - 1) in
      match int_of_string_opt port_str with
      | None -> Error (Invalid_port_number port_str)
      | Some port -> (
          match of_host_and_port_datagram ~host ~port with
          | Ok addr -> Ok addr
          | Error err -> Error err
        )
    )
