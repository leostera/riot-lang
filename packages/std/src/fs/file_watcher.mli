(** High-level file watching with process-based event delivery *)

(** File watcher process handle *)
type t = Pid.t
type Message.t += | FileEvents of Event.t list

(** Start file watcher process, sending events to owner_pid
    
    @param latency Polling interval for filesystem events (default: 1ms)
    @param ignore_prefixes List of path prefixes to ignore (default: [])
    @param root Root directory to watch
*)
val start_link : ?latency:Time.Duration.t -> ?ignore_prefixes:Path.t list -> root:Path.t -> unit -> t
