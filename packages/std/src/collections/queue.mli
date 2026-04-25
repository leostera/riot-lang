(**
   Lock-free FIFO queue.

   `Std.Collections.Queue` is the one shared concurrent queue surface in
   `std`. It is backed by a non-blocking linked queue and is safe to share
   across multiple actors, threads, producers, and consumers.

   The queue is unbounded and does not block. Coordination stays explicit:
   callers decide whether to spin, sleep, yield, or pair the queue with other
   signalling primitives.

   Semantics are split into two groups:

   - `create`, `push`, and `pop` are the synchronization core. They provide
     lock-free FIFO enqueue/dequeue behavior.
   - observational helpers such as `front`, `length`, `is_empty`, `iter`,
     `to_list`, `append`, `transfer`, and `clear` are convenience operations.
     They are exact in single-owner code, but under concurrent mutation they
     are only weakly consistent snapshots or best-effort drains.

   In particular:

   - `with_capacity` keeps the API shape for callers that want a constructor
     with a size hint, but the linked queue does not preallocate.
   - `front`, `length`, and `is_empty` may become stale immediately when other
     producers or consumers race.
   - traversal helpers (`for_each`, `fold_left`, `to_list`, `contains`,
     `iter`) walk the currently reachable chain and may miss or include
     concurrent changes.
   - drain/move helpers (`clear`, `append`, `transfer`, `mut_iter`) are built
     on repeated `pop`, so concurrent producers may interleave with them.
*)
type 'value t

(** Create an empty queue. *)
val create: unit -> 'value t

(**
   Create an empty queue.

   The capacity hint is accepted for compatibility but ignored by the
   lock-free linked implementation.
*)
val with_capacity: size:int -> 'value t

(** Build a queue from a list in FIFO order. *)
val from_list: 'value list -> 'value t

(** Enqueue one value. Safe under concurrent producers. *)
val push: 'value t -> value:'value -> unit

(**
   Dequeue one value from the front of the queue. Safe under concurrent
   consumers.
*)
val pop: 'value t -> 'value option

(**
   Observe the current front value without removing it.

   Under concurrent mutation this is only a weak snapshot.
*)
val front: 'value t -> 'value option

(**
   Observe the current queue length.

   Exact in single-owner code. Under concurrent mutation this is a moving
   approximation derived from successful enqueue/dequeue operations.
*)
val length: 'value t -> int

(**
   Observe whether the queue is currently empty. Weakly consistent under
   concurrent mutation.
*)
val is_empty: 'value t -> bool

(**
   Pop until the queue appears empty.

   Exact in single-owner code. Under concurrent producers it drains the values
   it can observe and may return before later pushes arrive.
*)
val clear: 'value t -> unit

(**
   Visit the currently reachable values in FIFO order.

   Under concurrent mutation this is a weak snapshot traversal.
*)
val for_each: 'value t -> fn:('value -> unit) -> unit

(**
   Fold over the currently reachable values in FIFO order.

   Under concurrent mutation this is a weak snapshot traversal.
*)
val fold_left: 'value t -> init:'acc -> fn:('acc -> 'value -> 'acc) -> 'acc

(**
   Collect the currently reachable values into a list in FIFO order.

   Under concurrent mutation this is a weak snapshot.
*)
val to_list: 'value t -> 'value list

(** Snapshot-style membership test over the currently reachable values. *)
val contains: 'value t -> value:'value -> bool

(** Drain values observed in the right queue into the left queue. *)
val append: 'value t -> 'value t -> unit

(** Drain values observed in [src] into [dst]. *)
val transfer: src:'value t -> dst:'value t -> unit

(** Immutable iterator over a weak snapshot of the queue contents. *)
val iter: 'value t -> 'value Iter.Iterator.t

(**
   Mutable iterator that repeatedly pops from the queue.

   Cloning the iterator snapshots the currently reachable values into a fresh
   queue.
*)
val mut_iter: 'value t -> 'value Iter.MutIterator.t
