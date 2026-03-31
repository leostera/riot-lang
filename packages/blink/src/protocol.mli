open Std

module type Intf = sig
  val name: string
end

module Http1: Intf
