(** Tusk RPC Server - High-level server interface *)

open Miniriot

val create : Pid.t -> Jsonrpc.Server.config
(** Create a JSON-RPC server with the given server process PID *)
