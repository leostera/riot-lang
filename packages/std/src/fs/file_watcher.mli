(** High-level file watching with process-based event delivery *)

type t = Pid.t
(** File watcher process handle *)

type Message.t += | FileEvents of Event.t list

(** Start file watcher process, sending events to owner_pid *)
val start_link : ?latency:Time.Duration.t -> root:Path.t -> unit -> t
