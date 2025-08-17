(** Server manager - Handles starting and managing the tusk server in the
    background *)

val is_server_running : unit -> bool
(** Check if the server is currently running *)

val start_background : unit -> bool
(** Start the server in the background. Returns true if the server was started
    successfully or was already running. *)

val stop_background : unit -> bool
(** Stop the background server. Returns true if the server was stopped
    successfully. *)

val kill_background : unit -> bool
(** Kill the background server forcefully (kill -9). Returns true if the server
    was killed successfully. *)

val ensure_running : unit -> bool
(** Ensure the server is running, starting it if necessary. Returns true if the
    server is running after this call. *)

val status : unit -> unit
(** Print the current server status *)

type daemon = {
  dir: string;
  os_pid: int;
  port: int;
}

val daemon : Workspace.workspace -> daemon option
(** Reads the daemon for a workspace with its dir, pid, and port, if available. *)

val write_daemon : Workspace.workspace -> port:int -> unit
(** Write daemon files (port and pid) for the given workspace and port *)

val remove_daemon : Workspace.workspace -> unit
(** Remove daemon files for the given workspace *)
