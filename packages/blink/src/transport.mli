open Std

module type Intf = sig
  val name : string
  val connect : Net.Addr.stream_addr -> Net.Uri.t -> (Connection.t, Error.t) result
end

module Tcp : Intf
module Tls : Intf

val connect : Net.Uri.t -> (Connection.t, Error.t) result
