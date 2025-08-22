(** Protocol types for communication with the Tusk server *)

open Miniriot

(** Target for build operations *)
type target = All | Package of string

(** Request types that can be sent to the server *)
type request =
  | Build of { client_pid : Pid.t; target : target }
  | Ping of { client_pid : Pid.t }
  | ScanWorkspace of { client_pid : Pid.t; current_dir : Path.t }

(** Response types from the server *)
type response = Pong | BuildCompleted

(** Message types for server communication *)
type Message.t += ServerRequest of request | ServerResponse of response