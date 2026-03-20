open Std

module type S = sig
  val name : string
  val rules : unit -> Rule.t list
  val diagnostic_codes : unit -> Diagnostic_code.package_entry list
end

type t = (module S)

let name ((module Provider) : t) = Provider.name
let rules ((module Provider) : t) = Provider.rules ()
let diagnostic_codes ((module Provider) : t) = Provider.diagnostic_codes ()
