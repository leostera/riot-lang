(* Abstract type followed by multiline val with optional parameters *)

module Border: sig
  type t
  val make: ?top:string -> ?left:string -> ?bottom:string -> unit -> t

  val normal: t
end
