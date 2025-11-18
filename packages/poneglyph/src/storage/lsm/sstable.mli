(** SSTable - Sorted String Table for persistent storage
    
    An SSTable is an immutable on-disk file format that stores sorted key-value pairs
    in blocks. It consists of:
    
    1. Data blocks (multiple 16KB blocks)
    2. Index block (maps first key of each data block → offset)
    3. Footer (metadata: first/last key, counts, offsets)
    
    File Layout:
    ┌─────────────────────────────────────┐
    │ Data Block 0 (16KB)                 │
    ├─────────────────────────────────────┤
    │ Data Block 1 (16KB)                 │
    ├─────────────────────────────────────┤
    │ ...                                 │
    ├─────────────────────────────────────┤
    │ Index Block (variable size)         │
    │   - Entry 0: first_key_0, offset_0  │
    │   - Entry 1: first_key_1, offset_1  │
    │   - ...                             │
    ├─────────────────────────────────────┤
    │ Footer (256 bytes)                  │
    │   - magic: "SST1"                   │
    │   - version: 1                      │
    │   - block_count                     │
    │   - entry_count                     │
    │   - first_key (41 bytes)            │
    │   - last_key (41 bytes)             │
    │   - index_offset                    │
    │   - index_size                      │
    │   - checksum                        │
    └─────────────────────────────────────┘
    
    Usage:
    
    Writing:
    ```ocaml
    match SSTable.create_builder ~path:"data.sst" with
    | Error e -> Error e
    | Ok builder ->
        builder |> add ~key:k1 ~value:v1
                |> Result.flat_map (add ~key:k2 ~value:v2)
                |> ...
                |> Result.flat_map finalize
    ```
    
    Reading:
    ```ocaml
    let sst = SSTable.open_read ~path:"data.sst" in
    match SSTable.get sst ~key with
    | Some value -> ...
    | None -> ...
    ```
*)

open Std

(** SSTable reader handle *)
type reader

(** SSTable builder for writing *)
type builder

(** Footer size in bytes (256 bytes) *)
val footer_size : int

(** Create a new SSTable builder
    
    @param path Path to the SSTable file to create
    @return Error if file creation fails
*)
val create_builder : path:string -> (builder, string) result

(** Add a key-value pair to the SSTable being built
    
    Keys must be added in strictly increasing order.
    Returns Error if key is out of order.
    
    Automatically creates new blocks when current block is full.
    
    @param key The 41-byte index key
    @param value The fact data as bytes
*)
val add : builder -> key:bytes -> value:bytes -> (builder, string) result

(** Finalize the SSTable and write to disk
    
    This:
    1. Flushes the current block
    2. Writes the index block
    3. Writes the footer
    4. Closes the file
    
    Returns the total number of entries written.
*)
val finalize : builder -> (int, string) result

(** Open an SSTable for reading
    
    This reads and validates the footer, then loads the index into memory.
    
    @param path Path to the SSTable file
*)
val open_read : path:string -> (reader, string) result

(** Get a value by key from the SSTable
    
    Uses the index to find the correct block, then searches within that block.
    Returns None if key not found.
    
    @param reader The SSTable reader
    @param key The 41-byte key to search for
*)
val get : reader -> key:bytes -> bytes option

(** Iterate over all entries in the SSTable in sorted order
    
    Reads blocks sequentially from disk.
*)
val iter : reader -> f:(key:bytes -> value:bytes -> unit) -> unit

(** Get the first key in the SSTable (from footer metadata) *)
val first_key : reader -> bytes

(** Get the last key in the SSTable (from footer metadata) *)
val last_key : reader -> bytes

(** Get the total number of entries in the SSTable *)
val entry_count : reader -> int

(** Get the number of data blocks in the SSTable *)
val block_count : reader -> int

(** Close the SSTable reader *)
val close : reader -> unit

(** Check if a key might be in the SSTable based on first/last key range
    
    This is a cheap pre-check before doing expensive disk reads.
    Returns true if key is in range [first_key, last_key].
*)
val in_range : reader -> key:bytes -> bool

(** Scan all keys with given prefix
    
    Returns all key-value pairs where the key starts with the given prefix.
    Results are in sorted order.
    
    @param reader The SSTable reader
    @param prefix The prefix bytes to match
    @return List of (key, value) pairs matching the prefix
*)
val scan_prefix : reader -> prefix:bytes -> (bytes * bytes) list
