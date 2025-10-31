include module type of Miniriot.Process

val self : unit -> Pid.t
val spawn : (unit -> (unit, exit_reason) result) -> Pid.t
val spawn_link : (unit -> (unit, exit_reason) result) -> Pid.t
