open Std

module type S = sig
  val name: string

  val rules: unit -> Rule.t list

  val explanations: unit -> Explanation.t list
end

type t = (module S)

let name = fun ((module Provider): t) -> Provider.name

let rules = fun ((module Provider): t) -> Provider.rules ()

let explanations = fun ((module Provider): t) -> Provider.explanations ()
