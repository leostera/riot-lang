(** High-level file watching with process-based event delivery *)

type t = Pid.t
(** File watcher process handle *)

type config = {
  paths : Path.t list;
  ignore_patterns : string list;
  file_extensions : (string list) option;
  latency : float;
}

type event = {
  path : Path.t;
  kind : event_kind;
}

and event_kind =
  | Created
  | Modified
  | Deleted
  | Renamed
  | Metadata

type Message.t += 
  | FileWatchEvent of event
  | StopWatcher

(** Create default configuration *)
val default_config : paths:Path.t list -> config

(** Start file watcher process, sending events to owner_pid *)
val start : config:config -> owner_pid:Pid.t -> t

(** Stop the file watcher process *)
val stop : t -> unit
