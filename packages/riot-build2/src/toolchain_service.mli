open Std

type t

val create: root:Path.t -> unit -> t

val ensure: t -> Toolchain_ready.t -> (unit, Error.t) result

val find: t -> Riot_model.Target.t -> Riot_toolchain.t option
