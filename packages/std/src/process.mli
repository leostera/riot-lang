include module type of Miniriot.Process

open Global

val self : unit -> Pid.t
val spawn : (unit -> (unit, exit_reason) result) -> Pid.t
val spawn_link : (unit -> (unit, exit_reason) result) -> Pid.t
