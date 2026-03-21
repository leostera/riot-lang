open Std

module type S = sig
  val name : string
  val rules : unit -> Rule.t list
  val explanations : unit -> Explanation.t list
end

type t = (module S)

let name ((module Provider) : t) = Provider.name
let rules ((module Provider) : t) = Provider.rules ()
let explanations ((module Provider) : t) = Provider.explanations ()
