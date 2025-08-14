(** Shared RPC message types for server-listener communication *)

open Miniriot

type Message.t +=
  | ClientRequest of Pid.t * Rpc.request
  | ServerResponse of Rpc.response
  | RestartServer
  | ShutdownServer
