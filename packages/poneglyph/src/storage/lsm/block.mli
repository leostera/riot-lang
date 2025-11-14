(** Block - Fixed-size sorted data blocks for LSM storage
    
    A block is a 16KB container that stores sorted key-value pairs.
    Blocks are the fundamental I/O unit in LSM storage - they are
    read/written atomically from/to disk.
    
    Design:
    - Maximum size: 16KB (16384 bytes)
    - Stores sorted key-value pairs (keys are 41 bytes from Key module)
    - Values are fact data serialized as bytes
    - Metadata: first_key, last_key, count, checksum
    - Binary format for disk persistence
    
    Layout on disk:
    [header: 128 bytes]
      - magic: 4 bytes ("BLOK")
      - version: 1 byte
      - count: 2 bytes (max 65535 entries)
      - first_key: 41 bytes
      - last_key: 41 bytes  
      - data_size: 4 bytes (actual data size in bytes)
      - checksum: 8 bytes (xxHash64 of data)
      - reserved: 27 bytes (for future use)
    [data: up to 16256 bytes]
      - Array of (key_offset: 4 bytes, value_offset: 4 bytes, value_size: 4 bytes)
      - Followed by packed keys and values
*)

open Std

(** The type of a block *)
type t

(** Maximum block size in bytes (16KB) *)
val max_block_size : int

(** Header size in bytes (128 bytes) *)
val header_size : int

(** Maximum data size (max_block_size - header_size) *)
val max_data_size : int

(** Create an empty block *)
val create : unit -> t

(** Add a key-value pair to the block
    
    Returns Error if:
    - Block would exceed max size
    - Key is not greater than the last key (must be sorted)
    
    @param key The 41-byte index key
    @param value The fact data as bytes
*)
val add : t -> key:bytes -> value:bytes -> (t, string) result

(** Get the number of entries in the block *)
val count : t -> int

(** Get the size of the block in bytes (including header) *)
val size : t -> int

(** Check if the block is empty *)
val is_empty : t -> bool

(** Get the first key in the block (None if empty) *)
val first_key : t -> bytes option

(** Get the last key in the block (None if empty) *)
val last_key : t -> bytes option

(** Find a value by key
    
    Uses binary search since keys are sorted.
    Returns None if key not found.
*)
val get : t -> key:bytes -> bytes option

(** Iterate over all key-value pairs in order *)
val iter : t -> f:(key:bytes -> value:bytes -> unit) -> unit

(** Fold over all key-value pairs in order *)
val fold : t -> init:'acc -> f:(acc:'acc -> key:bytes -> value:bytes -> 'acc) -> 'acc

(** Serialize block to bytes for writing to disk
    
    Format includes checksum for integrity verification.
*)
val to_bytes : t -> bytes

(** Deserialize block from bytes
    
    Returns Error if:
    - Invalid magic number
    - Unsupported version
    - Checksum mismatch
    - Corrupted data
*)
val from_bytes : bytes -> (t, string) result
