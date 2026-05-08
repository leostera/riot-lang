(**
   Lock-free concurrent hash map.

   `Std.Collections.ConcurrentHashMap` is safe to share across actors, runtime
   scheduler domains, and kernel threads. The synchronization core is a fixed
   array of atomic bucket slots. Bucket heads are allocated and installed on
   first write, then mutating operations retry with CAS when another writer
   wins the bucket race. `insert`, `remove`, `get`, and `compute` do not take
   locks.

   The table does not resize after construction. Use `with_capacity` when the
   expected key count is known so buckets stay short under contention.

   Semantics are split into two groups:

   - `insert`, `remove`, and `get` are the synchronization core. They are
     linearizable with respect to the bucket that owns the key.
   - observational helpers such as `length`, `is_empty`, traversal, iterators,
     and `clear` are exact in single-owner code, but under concurrent mutation
     they are weakly consistent snapshots or best-effort sweeps.

   Callback-based updates may call the callback more than once when a CAS race
   is lost and the operation retries. Keep those callbacks pure or idempotent.
*)
type ('key, 'value) t
type ('key, 'value) entry =
  | Occupied of 'value
  | Vacant
type ('value, 'result) operation =
  | Insert of 'value * 'result
  | Remove of 'result
  | Abort of 'result

(** Create an empty map using the runtime parallelism based default bucket count. *)
val create: unit -> ('key, 'value) t

(**
   Create an empty map with enough bucket slots for the expected key count.

   The capacity is a construction-time hint. The map does not resize after it is
   shared. Bucket heads are allocated only for slots that receive writes.
*)
val with_capacity: size:int -> ('key, 'value) t

(** Build a map from a list. Later duplicate keys replace earlier values. *)
val from_list: ('key * 'value) list -> ('key, 'value) t

(** Return the number of bucket slots backing the map. *)
val bucket_count: ('key, 'value) t -> int

(** Insert or replace a key, returning the previous value when present. *)
val insert: ('key, 'value) t -> key:'key -> value:'value -> 'value option

(** Read the current value for a key. *)
val get: ('key, 'value) t -> key:'key -> 'value option

(** Remove a key, returning the removed value when present. *)
val remove: ('key, 'value) t -> key:'key -> 'value option

val has_key: ('key, 'value) t -> key:'key -> bool

(**
   Count entries visible through striped size counters.

   Exact without concurrent mutation; weakly consistent while writers race.
*)
val length: ('key, 'value) t -> int

(** Weakly consistent emptiness check under concurrent mutation. *)
val is_empty: ('key, 'value) t -> bool

(**
   Clear every bucket with atomic exchanges.

   Exact in single-owner code. Concurrent inserts may appear before or after the
   sweep depending on their bucket race.
*)
val clear: ('key, 'value) t -> unit

val keys: ('key, 'value) t -> 'key list

val values: ('key, 'value) t -> 'value list

val for_each: ('key, 'value) t -> fn:('key -> 'value -> unit) -> unit

val fold_left: ('key, 'value) t -> init:'acc -> fn:('acc -> 'key -> 'value -> 'acc) -> 'acc

val to_list: ('key, 'value) t -> ('key * 'value) list

val entry: ('key, 'value) t -> key:'key -> ('key, 'value) entry

(**
   Atomically compute an insert, removal, or abort from the current value.

   The callback may be evaluated more than once when a CAS race is lost.
*)
val compute:
  ('key, 'value) t ->
  key:'key ->
  fn:('value option -> ('value, 'result) operation) ->
  'result

val iter: ('key, 'value) t -> ('key * 'value) Iter.Iterator.t

val mut_iter: ('key, 'value) t -> ('key * 'value) Iter.MutIterator.t
