open Std

module type S = sig
  val name : string

  val rules : unit -> Rule.t list

  val explanations : unit -> Explanation.t list
end

type t = (module S)
val name : t -> string

val rules : t -> Rule.t list

val explanations : t -> Explanation.t list
