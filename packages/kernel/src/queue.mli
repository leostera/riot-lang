(**
   Lock-free FIFO queue.

   `Kernel.Queue` is a low-level unbounded queue for runtime internals and
   foundational code that cannot depend on `Std.Collections`.

   `create`, `push`, and `pop` provide the synchronization core. Observational
   helpers such as `front`, `length`, `is_empty`, `to_list`, and traversal are
   exact in single-owner code, but only weakly consistent under concurrent
   mutation.
*)
type 'value t

(** Create an empty queue. *)
val create: unit -> 'value t

(** Create an empty queue. The capacity hint is accepted but ignored. *)
val with_capacity: size:int -> 'value t

(** Build a queue from a list in FIFO order. *)
val from_list: 'value list -> 'value t

(** Enqueue one value. Safe under concurrent producers. *)
val push: 'value t -> value:'value -> unit

(** Dequeue one value from the front. Safe under concurrent consumers. *)
val pop: 'value t -> 'value option

(** Observe the current front value without removing it. *)
val front: 'value t -> 'value option

(** Observe the current queue length. *)
val length: 'value t -> int

(** Observe whether the queue is currently empty. *)
val is_empty: 'value t -> bool

(** Pop until the queue appears empty. *)
val clear: 'value t -> unit

(** Visit the currently reachable values in FIFO order. *)
val for_each: 'value t -> fn:('value -> unit) -> unit

(** Fold over the currently reachable values in FIFO order. *)
val fold_left: 'value t -> acc:'acc -> fn:('acc -> 'value -> 'acc) -> 'acc

(** Collect the currently reachable values into a list in FIFO order. *)
val to_list: 'value t -> 'value list

(** Snapshot-style membership test over the currently reachable values. *)
val contains: 'value t -> value:'value -> bool

(** Drain values observed in the right queue into the left queue. *)
val append: 'value t -> 'value t -> unit

(** Drain values observed in [src] into [dst]. *)
val transfer: src:'value t -> dst:'value t -> unit
