(** Timer management for Actors *)
type id = Timer_id.t
(** Opaque timer identifier *)
type mode =
  | One_shot
  (** Fire once and remove *)
  | Interval of int64
(** Fire repeatedly with given interval in nanos *)
type action =
  | Wake_process of Process.t
  (** Wake a sleeping process (for receive/syscall timeouts) *)
  | Send_message of Pid.t * Message.t
(** Send a message to a process (for send_after/send_interval) *)
type t = {
  id: id;
  mode: mode;
  mutable started_at: int64;  (** Start time in nanoseconds *)
  mutable expires_at: int64;  (** Expiration time in nanoseconds *)
  duration_nanos: int64;  (** Duration in nanoseconds *)
  action: action;
  mutable status: 
    [
      `pending
      | `cancelled
    ];
}

(** A timer *)
val make: now:int64 -> duration_nanos:int64 -> mode:mode -> action:action -> t

(** Create a new timer *)
val is_cancelled: t -> bool

(** Check if timer has been cancelled *)
val cancel: t -> unit

(** Mark timer as cancelled *)
val should_fire: t -> now:int64 -> bool

(** Check if timer should fire at the given time *)
val reschedule: t -> now:int64 -> unit

(** Reschedule interval timer for next firing *)
