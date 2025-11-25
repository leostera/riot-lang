(** Codedb Service - Supervisor for code indexing processes
    
    The service runs multiple supervised processes:
    1. Internal server - Indexes workspace on startup
    2. File watcher - Monitors file changes (future)
    3. (Future) Poneglyph server - Manages database queries
*)

(** Start the Codedb service supervisor with all child processes *)
val start : Config.t -> Std.Supervisor.t
