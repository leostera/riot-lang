(** Low-level file system event watching *)

open Global0
  open IO

type t
(** File system event watcher handle *)

type watch_id = int
(** Watch identifier *)

type event = {
  path : string;
  flags : int32;
}

type event_kind =
  | Created
  | Modified
  | Deleted
  | Renamed
  | Metadata

(** Create a new file system watcher *)
val create : unit -> (t, error) result

(** Watch a path for changes *)
val watch : t -> path:string -> latency:float -> (watch_id, error) result

(** Stop watching a path *)
val unwatch : t -> watch_id -> (unit, error) result

(** Read pending events (non-blocking) *)
val read_events : t -> (event list, error) result

(** Decode event flags into kind *)
val decode_event_kind : int32 -> event_kind

(** Stop the watcher and release resources *)
val stop : t -> (unit, error) result
