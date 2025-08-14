(** Shared RPC message types for server-listener communication *)

open Miniriot

type Message.t +=
  | ClientRequest of Pid.t * Rpc.request
  | JsonClientRequest of Pid.t * string (* JSON-encoded request *)
  | ServerResponse of Rpc.response
  | JsonServerResponse of string (* JSON-encoded response *)
  | RestartServer
  | ShutdownServer
