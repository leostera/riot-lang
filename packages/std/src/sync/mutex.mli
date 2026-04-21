open Kernel

type t
val create: unit -> t

val lock: t -> unit

val unlock: t -> unit

val try_lock: t -> bool

val suspend: t -> owner:Runtime.Pid.t -> (unit, string) result
