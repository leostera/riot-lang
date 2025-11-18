(** Manifest - SSTable metadata tracking across process boundaries
    
    The manifest persists SSTable metadata to disk so that short-lived
    CLI processes can coordinate compaction state.
*)

open Std

(** Metadata for a single SSTable *)
type sstable_metadata = {
  path : string;           (** Relative path: "sstable_123.sst" *)
  tier : int;              (** Size tier: 0=tiny, 1=small, 2=medium, etc *)
  size_bytes : int;        (** File size in bytes *)
  min_key : bytes;         (** First key in SSTable *)
  max_key : bytes;         (** Last key in SSTable *)
  entry_count : int;       (** Number of entries *)
  created_at : int64;      (** Unix timestamp for age tracking *)
}

(** Manifest for a single index (e.g., EAVT, AVET) *)
type index_manifest = {
  sstables : sstable_metadata list;  (** Sorted by tier, then age *)
  next_sstable_id : int;             (** Next SSTable ID to allocate (RocksDB-style) *)
}

(** Complete manifest for all indices *)
type t = {
  version : int;
  indices : (string, index_manifest) Collections.HashMap.t;
}

(** {1 Creation and Loading} *)

(** Create empty manifest *)
val empty : unit -> t

(** Load manifest from file.
    Returns empty manifest if file doesn't exist or is invalid. *)
val load : path:string -> (t, string) result

(** Save manifest to file atomically (temp file + rename) *)
val save : path:string -> t -> (unit, string) result

(** {1 SSTable Management} *)

(** Add new SSTable to manifest *)
val add_sstable : t -> index:string -> sstable_metadata -> t

(** Remove SSTables by path *)
val remove_sstables : t -> index:string -> paths:string list -> t

(** Get all SSTables for an index *)
val get_sstables : t -> index:string -> sstable_metadata list

(** {1 Next SSTable ID Management} *)

(** Get the next SSTable ID to allocate for an index *)
val get_next_sstable_id : t -> index:string -> int

(** Update the next SSTable ID for an index.
    Should be called after allocating IDs to persist the counter. *)
val update_next_sstable_id : t -> index:string -> int -> t

(** {1 Tier Management} *)

(** Group SSTables by tier *)
val group_by_tier : sstable_metadata list -> (int * sstable_metadata list) list

(** Calculate which tier an SSTable belongs to based on size.
    
    Tiers:
    - 0: < 1 MB
    - 1: 1-8 MB  
    - 2: 8-64 MB
    - 3: 64-512 MB
    - 4+: larger
*)
val tier_for_size : int -> int
