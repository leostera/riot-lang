(** Build server - Miniriot process that orchestrates builds *)

val start : unit -> Miniriot.Pid.t
(** Start the build server and return its PID *)

val start_with_listener : unit -> Miniriot.Process.exit_reason
(** Start the server with TCP listener for RPC. This function makes the current
    process _become_ the Tusk server *)
