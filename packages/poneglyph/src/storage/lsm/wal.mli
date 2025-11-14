(** Write-Ahead Log for durability.

    The WAL records all mutations before they're applied to the memtable.
    If the system crashes, we can replay the WAL to recover uncommitted data.
*)

open Std

(** The WAL handle *)
type t

(** A WAL entry represents a single operation *)
type entry =
  | Put of bytes * bytes  (** Key bytes (41 bytes) and value bytes *)
  | Delete of bytes  (** Key bytes (41 bytes) *)

(** {1 Creation and Opening} *)

(** [create ~path] creates a new WAL file at the given path.
    Returns an error if the file already exists. *)
val create : path:string -> (t, string) result

(** [open_existing ~path] opens an existing WAL file.
    Returns an error if the file doesn't exist. *)
val open_existing : path:string -> (t, string) result

(** {1 Write Operations} *)

(** [append wal key value] appends a Put entry to the WAL.
    The write is fsynced to disk before returning.
    @param key 41-byte encoded key
    @param value encoded value bytes *)
val append : t -> key:bytes -> value:bytes -> (unit, string) result

(** [append_delete wal key] appends a Delete entry to the WAL.
    The write is fsynced to disk before returning.
    @param key 41-byte encoded key *)
val append_delete : t -> key:bytes -> (unit, string) result

(** {1 Recovery} *)

(** [replay wal] reads all entries from the WAL in order.
    Used during recovery to rebuild the memtable.
    Verifies checksums and returns an error if corruption is detected. *)
val replay : t -> (entry list, string) result

(** {1 Maintenance} *)

(** [truncate wal] clears all entries from the WAL.
    Called after successfully flushing the memtable to an SSTable. *)
val truncate : t -> (unit, string) result

(** [close wal] closes the WAL file. *)
val close : t -> (unit, string) result

(** {1 Utilities} *)

(** [path wal] returns the filesystem path of the WAL. *)
val path : t -> string
