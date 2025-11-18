(** LSM Database Lock - Prevents race conditions between readers and writers
    
    Uses POSIX file locking (lockf-style) with shared/exclusive modes:
    - SHARED locks: Multiple readers can hold simultaneously
    - EXCLUSIVE locks: Only one writer, blocks ALL others
    
    Lock file: {data_dir}/LOCK
    
    Locking strategy:
    - Read operations (query, get, stats): SHARED lock (concurrent reads OK)
    - Write operations (state, compact): EXCLUSIVE lock (blocks everything)
    
    This prevents the compaction race:
    - Reader holds SHARED → compactor cannot delete SSTables
    - Writer holds EXCLUSIVE → readers cannot start (wait for compaction to finish)
    
    The lock is automatically released when:
    - Process calls release explicitly
    - Process exits (even if crashed)
    - File descriptor is closed
*)

open Std

type t
(** Opaque lock handle *)

type lock_mode =
  | Shared     (** Multiple readers can hold simultaneously (LOCK_SH) *)
  | Exclusive  (** Only one process, blocks all others (LOCK_EX) *)
(** Lock access modes *)

(** {1 Lock Acquisition} *)

val acquire : data_dir:Path.t -> mode:lock_mode -> timeout:Time.Duration.t -> (t, string) result
(** Acquire lock on database directory.
    
    Opens {data_dir}/LOCK and acquires lock with specified mode:
    - Shared: Multiple processes can hold LOCK_SH concurrently (for reads)
    - Exclusive: Only one process, blocks all others (for writes/compaction)
    
    Blocks if lock is unavailable, up to timeout.
    
    @param data_dir Database directory path
    @param mode Shared or Exclusive
    @param timeout Max duration to wait (Duration.zero = fail immediately, 
                   negative = wait forever)
    @return Lock handle or error
    
    Example:
    {[
      (* Reader *)
      let timeout = Time.Duration.from_secs 30 in
      match Lockfile.acquire ~data_dir:(Path.v ".codedb") ~mode:Shared ~timeout with
      | Ok lock -> 
          (* Run query... *)
          Lockfile.release lock
      | Error e ->
          eprintln "Database locked: %s" e
      
      (* Writer *)
      match Lockfile.acquire ~data_dir:(Path.v ".codedb") ~mode:Exclusive ~timeout with
      | Ok lock ->
          (* Do writes/compaction... *)
          Lockfile.release lock
      | Error e ->
          eprintln "Database locked: %s" e
    ]}
*)

val try_acquire : data_dir:Path.t -> mode:lock_mode -> (t option, string) result
(** Try to acquire lock without blocking.
    
    @param data_dir Database directory path  
    @param mode Shared or Exclusive
    @return Some lock if acquired, None if unavailable
*)

val release : t -> (unit, string) result
(** Release lock and close lock file.
    
    Safe to call multiple times (idempotent).
*)

(** {1 Diagnostics} *)

val is_locked : data_dir:Path.t -> bool
(** Check if database is currently locked (non-blocking peek).
    
    Useful for diagnostics, but not for synchronization
    (lock state can change immediately after check).
*)
