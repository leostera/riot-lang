open Std

module type S = sig
  val name : string
  val rules : unit -> Rule.t list
  val diagnostic_codes : unit -> Diagnostic_code.package_entry list
end

type t = (module S)

val name : t -> string
val rules : t -> Rule.t list
val diagnostic_codes : t -> Diagnostic_code.package_entry list
