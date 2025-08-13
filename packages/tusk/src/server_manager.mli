(** Server manager - Handles starting and managing the tusk server in the background *)

(** Check if the server is currently running *)
val is_server_running : unit -> bool

(** Start the server in the background.
    Returns true if the server was started successfully or was already running. *)
val start_background : unit -> bool

(** Stop the background server.
    Returns true if the server was stopped successfully. *)
val stop_background : unit -> bool

(** Ensure the server is running, starting it if necessary.
    Returns true if the server is running after this call. *)
val ensure_running : unit -> bool

(** Print the current server status *)
val status : unit -> unit