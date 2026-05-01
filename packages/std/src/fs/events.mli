open Global

(** File system event watcher handle *)

(** Watch identifier *)
type t
type watch_id
(** Create a new file system watcher *)
type error = IO.error

val create: unit -> (t, error) result

(** Watch a path for changes *)
val watch: t -> path:Path.t -> latency:Time.Duration.t -> (watch_id, error) result

(** Stop watching a path *)
val unwatch: t -> watch_id -> (unit, error) result

(** Read pending events (non-blocking) *)
val poll: t -> (Event.t list, error) result

(** Stop the watcher and release resources *)
val stop: t -> (unit, error) result
