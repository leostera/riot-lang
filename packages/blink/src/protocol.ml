open Std

module type Intf = sig
  val name: string
end

module Http1: Intf = struct
  let name = "http/1.1"
end
