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

(** Index tag for multi-index atomicity.
    Tags identify which LSM index an entry belongs to. *)
type index_tag =
  | EAVT    (** Entity-Attribute-Value-Transaction index *)
  | AVET    (** Attribute-Value-Entity-Transaction index *)
  | FACT    (** Fact metadata index *)
  | SOURCE  (** Source metadata index *)
  | URIS    (** URI string storage index *)

(** A tagged entry includes an index identifier for multi-index atomicity *)
type tagged_entry =
  | TaggedPut of index_tag * bytes * bytes  (** Index tag, key, value *)
  | TaggedDelete of index_tag * bytes  (** Index tag, key *)

(** {1 Creation and Opening} *)

(** [create_or_open ~path] opens a WAL file for reading and appending.
    Creates the file if it doesn't exist, or opens existing file without truncating.
    
    Uses O_RDWR | O_APPEND | O_CREAT flags:
    - Can read (for replay on startup)
    - All writes automatically go to end of file (append-only)
    - Creates file if it doesn't exist
    - Does NOT truncate existing file (preserves data)
    
    This is the recommended function to use for all WAL operations. *)
val create_or_open : path:string -> (t, string) result

(** [create ~path] (Deprecated: Use create_or_open instead)
    Creates a new WAL file at the given path. *)
val create : path:string -> (t, string) result

(** [open_existing ~path] (Deprecated: Use create_or_open instead)
    Opens an existing WAL file. *)
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

(** [append_batch wal entries] atomically appends multiple entries to the WAL.
    All entries are written in a single fsync operation, ensuring atomicity.
    Either all entries are persisted or none are (atomic batch write).
    Returns Ok () if successful, Error if any part of the write fails. *)
val append_batch : t -> entry list -> (unit, string) result

(** [append_batch_tagged wal entries] atomically appends multiple tagged entries.
    Tagged entries include an index identifier (EAVT, AVET, FACT, SOURCE, URIS) so that
    a single WAL can store entries for multiple LSM indices. This enables atomic
    updates across all indices - either all are persisted or none are.
    Returns Ok () if successful, Error if any part of the write fails. *)
val append_batch_tagged : t -> tagged_entry list -> (unit, string) result

(** {1 Recovery} *)

(** [replay wal] reads all entries from the WAL in order.
    Used during recovery to rebuild the memtable.
    Verifies checksums and returns an error if corruption is detected. *)
val replay : t -> (entry list, string) result

(** [replay_tagged wal] reads all tagged entries from the WAL in order.
    Tagged entries include an index tag that identifies which LSM index
    the entry belongs to (EAVT, AVET, FACT, SOURCE, or URIS).
    Used during recovery to route entries to the correct index.
    Verifies checksums and returns an error if corruption is detected. *)
val replay_tagged : t -> (tagged_entry list, string) result

(** {1 Maintenance} *)

(** [truncate wal] clears all entries from the WAL.
    Called after successfully flushing the memtable to an SSTable. *)
val truncate : t -> (unit, string) result

(** [close wal] closes the WAL file. *)
val close : t -> (unit, string) result

(** {1 Utilities} *)

(** [path wal] returns the filesystem path of the WAL. *)
val path : t -> string
