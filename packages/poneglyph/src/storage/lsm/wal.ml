open Std
open Std.IO
open Std.Collections
open Std.Sync

type t = { path : string; file : Fs.File.t Cell.t }

type entry = Put of bytes * bytes | Delete of bytes

(* Index tags for multi-index atomicity *)
type index_tag = EAVT | AVET | FACT | SOURCE | URIS

type tagged_entry =
  | TaggedPut of index_tag * bytes * bytes
  | TaggedDelete of index_tag * bytes

(* Entry type tags *)
let put_tag = 0x01
let delete_tag = 0x02

(* Index tag values *)
let eavt_tag = 0x01
let avet_tag = 0x02
let fact_tag = 0x03
let source_tag = 0x04
let uris_tag = 0x05

let index_tag_to_byte = function
  | EAVT -> eavt_tag
  | AVET -> avet_tag
  | FACT -> fact_tag
  | SOURCE -> source_tag
  | URIS -> uris_tag

let index_tag_of_byte = function
  | 0x01 -> EAVT
  | 0x02 -> AVET
  | 0x03 -> FACT
  | 0x04 -> SOURCE
  | 0x05 -> URIS
  | n -> panic ("Invalid index tag: " ^ string_of_int n)

(* Simple hash-based checksum (using FNV-1a algorithm) *)
let hash_bytes bytes =
  let fnv_prime = 16777619l in
  let fnv_offset = -2128831035l in  (* 2166136261 as signed int32 *)
  let hash = cell fnv_offset in
  Bytes.iter
    (fun byte ->
      let h = Cell.get hash in
      let h = Int32.logxor h (Int32.of_int (Char.code byte)) in
      let h = Int32.mul h fnv_prime in
      Cell.set hash h)
    bytes;
  Cell.get hash

(** Open WAL for reading and appending. Creates if doesn't exist.
    
    Uses O_RDWR | O_APPEND | O_CREAT flags:
    - Can read (for replay on startup)
    - All writes automatically go to end of file (append-only)
    - Creates file if it doesn't exist
    - Does NOT truncate existing file (preserves data)
*)
let create_or_open ~path =
  let path_v = Path.v path in
  match Fs.File.open_append path_v with
  | Error _ -> Error ("Failed to open WAL: " ^ path)
  | Ok file -> Ok { path; file = cell file }

(** Deprecated: Use create_or_open instead. *)
let create ~path = create_or_open ~path

(** Deprecated: Use create_or_open instead. *)
let open_existing ~path = create_or_open ~path

let path wal = wal.path

(* Write a u32 in big-endian format at offset *)
let write_u32_at buf offset value =
  Bytes.set buf offset (Char.chr ((value lsr 24) land 0xFF));
  Bytes.set buf (offset + 1) (Char.chr ((value lsr 16) land 0xFF));
  Bytes.set buf (offset + 2) (Char.chr ((value lsr 8) land 0xFF));
  Bytes.set buf (offset + 3) (Char.chr (value land 0xFF))

(* Read a u32 in big-endian format *)
let read_u32 bytes offset =
  let b0 = Char.code (Bytes.get bytes offset) in
  let b1 = Char.code (Bytes.get bytes (offset + 1)) in
  let b2 = Char.code (Bytes.get bytes (offset + 2)) in
  let b3 = Char.code (Bytes.get bytes (offset + 3)) in
  (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3

(* Write an i32 in big-endian format at offset *)
let write_i32_at buf offset value =
  let value = Int32.to_int value in
  write_u32_at buf offset value

(* Read an i32 in big-endian format *)
let read_i32 bytes offset = Int32.of_int (read_u32 bytes offset)

let append wal ~key ~value =
  let file = Cell.get wal.file in

  (* Key and value are already bytes *)
  let key_len = Bytes.length key in
  let value_len = Bytes.length value in

  (* Build entry: tag (1) + key_len (4) + key + value_len (4) + value *)
  let entry_size = 1 + 4 + key_len + 4 + value_len in
  let entry = Bytes.create entry_size in
  Bytes.set entry 0 (Char.chr put_tag);
  write_u32_at entry 1 key_len;
  Bytes.blit key 0 entry 5 key_len;
  write_u32_at entry (5 + key_len) value_len;
  Bytes.blit value 0 entry (9 + key_len) value_len;

  (* Calculate checksum *)
  let checksum = hash_bytes entry in
  let checksum_bytes = Bytes.create 4 in
  write_i32_at checksum_bytes 0 checksum;

  (* Write entry + checksum *)
  (match Fs.File.write file entry ~offset:0 ~len:entry_size with
  | Error _ -> Error ("Failed to write entry: ")
  | Ok _ -> (
      match Fs.File.write file checksum_bytes ~offset:0 ~len:4 with
      | Error _ -> Error ("Failed to write checksum: ")
      | Ok _ -> (
          match Fs.File.sync_all file with
          | Error _ -> Error ("Failed to sync: ")
          | Ok () -> Ok ())))

let append_delete wal ~key =
  let file = Cell.get wal.file in

  (* Key is already bytes *)
  let key_len = Bytes.length key in

  (* Build entry: tag (1) + key_len (4) + key *)
  let entry_size = 1 + 4 + key_len in
  let entry = Bytes.create entry_size in
  Bytes.set entry 0 (Char.chr delete_tag);
  write_u32_at entry 1 key_len;
  Bytes.blit key 0 entry 5 key_len;

  (* Calculate checksum *)
  let checksum = hash_bytes entry in
  let checksum_bytes = Bytes.create 4 in
  write_i32_at checksum_bytes 0 checksum;

  (* Write entry + checksum *)
  match Fs.File.write file entry ~offset:0 ~len:entry_size with
  | Error _ -> Error ("Failed to write entry: ")
  | Ok _ -> (
      match Fs.File.write file checksum_bytes ~offset:0 ~len:4 with
      | Error _ -> Error ("Failed to write checksum: ")
      | Ok _ -> (
          match Fs.File.sync_all file with
          | Error _ -> Error ("Failed to sync: ")
          | Ok () -> Ok ()))

let replay wal =
  let file = Cell.get wal.file in

  (* Seek to start *)
  (match Fs.File.seek file 0L with
  | Error _ -> Error ("Failed to seek: ")
  | Ok _ ->
      let entries = vec [] in
      let rec read_entries () =
        (* Try to read entry tag *)
        let tag_buf = Bytes.create 1 in
        match Fs.File.read_exact file tag_buf ~offset:0 ~len:1 with
        | Error _ -> Ok () (* EOF - normal termination *)
        | Ok () ->
            let tag = Char.code (Bytes.get tag_buf 0) in

            (* Read key length *)
            let key_len_buf = Bytes.create 4 in
            (match Fs.File.read_exact file key_len_buf ~offset:0 ~len:4 with
            | Error _ ->
                Error ("Failed to read key length: ")
            | Ok () ->
                let key_len = read_u32 key_len_buf 0 in

                (* Read key *)
                let key_bytes = Bytes.create key_len in
                (match
                   Fs.File.read_exact file key_bytes ~offset:0 ~len:key_len
                 with
                | Error _ ->
                    Error ("Failed to read key: ")
                | Ok () -> (
                    match tag with
                        | t when t = put_tag ->
                            (* Read value length *)
                            let value_len_buf = Bytes.create 4 in
                            (match
                               Fs.File.read_exact file value_len_buf ~offset:0
                                 ~len:4
                             with
                             | Error _ ->
                                Error ("Failed to read value length: ")
                            | Ok () ->
                                let value_len = read_u32 value_len_buf 0 in

                                (* Read value *)
                                let value_bytes = Bytes.create value_len in
                                (match
                                   Fs.File.read_exact file value_bytes ~offset:0
                                     ~len:value_len
                                 with
                                | Error _ ->
                                    Error ("Failed to read value: ")
                                | Ok () ->
                                    (* Build entry for checksum *)
                                    let entry_size =
                                      1 + 4 + key_len + 4 + value_len
                                    in
                                    let entry = Bytes.create entry_size in
                                    Bytes.set entry 0 (Char.chr put_tag);
                                    write_u32_at entry 1 key_len;
                                    Bytes.blit key_bytes 0 entry 5 key_len;
                                    write_u32_at entry (5 + key_len) value_len;
                                    Bytes.blit value_bytes 0 entry (9 + key_len)
                                      value_len;

                                    (* Read and verify checksum *)
                                    let checksum_buf = Bytes.create 4 in
                                    (match
                                       Fs.File.read_exact file checksum_buf
                                         ~offset:0 ~len:4
                                     with
                                        | Error _ ->
                                            Error ("Failed to read checksum: ")
                                        | Ok () ->
                                            let expected_checksum = hash_bytes entry in
                                            let actual_checksum = read_i32 checksum_buf 0 in
                                            if not (Int32.equal expected_checksum actual_checksum) then
                                              Error "Checksum mismatch"
                                            else (
                                              Vector.push entries (Put (key_bytes, value_bytes));
                                              read_entries ()))))
                        | t when t = delete_tag ->
                            (* Build entry for checksum *)
                            let entry_size = 1 + 4 + key_len in
                            let entry = Bytes.create entry_size in
                            Bytes.set entry 0 (Char.chr delete_tag);
                            write_u32_at entry 1 key_len;
                            Bytes.blit key_bytes 0 entry 5 key_len;

                            (* Read and verify checksum *)
                            let checksum_buf = Bytes.create 4 in
                            (match Fs.File.read_exact file checksum_buf ~offset:0 ~len:4 with
                             | Error _ -> Error ("Failed to read checksum: ")
                             | Ok () ->
                                 let expected_checksum = hash_bytes entry in
                                 let actual_checksum = read_i32 checksum_buf 0 in
                                 if not (Int32.equal expected_checksum actual_checksum) then
                                   Error "Checksum mismatch"
                                 else (
                                   Vector.push entries (Delete key_bytes);
                                   read_entries ()))
                    | _ -> Error ("Unknown tag: " ^ string_of_int tag))))
      in
      match read_entries () with
      | Error e -> Error e
      | Ok () -> 
          let iter = Vector.to_mut_iter entries in
          Ok (Iter.MutIterator.to_list iter))

(** [replay_tagged wal] reads all tagged entries from the WAL.
    Tagged entries include an index tag that identifies which LSM index
    the entry belongs to (EAVT, AVET, FACT, or SOURCE). *)
let replay_tagged wal =
  let file = Cell.get wal.file in

  (* Seek to start *)
  match Fs.File.seek file 0L with
  | Error _ -> Error "Failed to seek to start"
  | Ok _ ->
      let entries = vec [] in
      let rec read_tagged_entries () =
        (* Try to read index tag *)
        let index_tag_buf = Bytes.create 1 in
        match Fs.File.read_exact file index_tag_buf ~offset:0 ~len:1 with
        | Error _ -> Ok () (* EOF *)
        | Ok () ->
            let index_tag_byte = Char.code (Bytes.get index_tag_buf 0) in
            let index_tag = index_tag_of_byte index_tag_byte in
            
            (* Read entry type tag *)
            let entry_tag_buf = Bytes.create 1 in
            (match Fs.File.read_exact file entry_tag_buf ~offset:0 ~len:1 with
            | Error _ -> Error "Failed to read entry tag"
            | Ok () ->
                let entry_tag = Char.code (Bytes.get entry_tag_buf 0) in
                
                (* Read key length *)
                let key_len_buf = Bytes.create 4 in
                (match Fs.File.read_exact file key_len_buf ~offset:0 ~len:4 with
                | Error _ -> Error "Failed to read key length"
                | Ok () ->
                    let key_len = read_u32 key_len_buf 0 in
                    
                    (* Read key *)
                    let key_bytes = Bytes.create key_len in
                    (match Fs.File.read_exact file key_bytes ~offset:0 ~len:key_len with
                    | Error _ -> Error "Failed to read key"
                    | Ok () -> (
                        match entry_tag with
                        | t when t = put_tag ->
                            (* Read value length *)
                            let value_len_buf = Bytes.create 4 in
                            (match Fs.File.read_exact file value_len_buf ~offset:0 ~len:4 with
                            | Error _ -> Error "Failed to read value length"
                            | Ok () ->
                                let value_len = read_u32 value_len_buf 0 in
                                
                                (* Read value *)
                                let value_bytes = Bytes.create value_len in
                                (match Fs.File.read_exact file value_bytes ~offset:0 ~len:value_len with
                                | Error _ -> Error "Failed to read value"
                                | Ok () ->
                                    (* Build entry for checksum *)
                                    let entry_size = 1 + 1 + 4 + key_len + 4 + value_len in
                                    let entry = Bytes.create entry_size in
                                    Bytes.set entry 0 (Char.chr index_tag_byte);
                                    Bytes.set entry 1 (Char.chr put_tag);
                                    write_u32_at entry 2 key_len;
                                    Bytes.blit key_bytes 0 entry 6 key_len;
                                    write_u32_at entry (6 + key_len) value_len;
                                    Bytes.blit value_bytes 0 entry (10 + key_len) value_len;
                                    
                                    (* Read and verify checksum *)
                                    let checksum_buf = Bytes.create 4 in
                                    (match Fs.File.read_exact file checksum_buf ~offset:0 ~len:4 with
                                    | Error _ -> Error "Failed to read checksum"
                                    | Ok () ->
                                        let expected = hash_bytes entry in
                                        let actual = read_i32 checksum_buf 0 in
                                        if not (Int32.equal expected actual) then
                                          Error "Checksum mismatch"
                                        else (
                                          Vector.push entries (TaggedPut (index_tag, key_bytes, value_bytes));
                                          read_tagged_entries ()))))
                        | t when t = delete_tag ->
                            (* Build entry for checksum *)
                            let entry_size = 1 + 1 + 4 + key_len in
                            let entry = Bytes.create entry_size in
                            Bytes.set entry 0 (Char.chr index_tag_byte);
                            Bytes.set entry 1 (Char.chr delete_tag);
                            write_u32_at entry 2 key_len;
                            Bytes.blit key_bytes 0 entry 6 key_len;
                            
                            (* Read and verify checksum *)
                            let checksum_buf = Bytes.create 4 in
                            (match Fs.File.read_exact file checksum_buf ~offset:0 ~len:4 with
                            | Error _ -> Error "Failed to read checksum"
                            | Ok () ->
                                let expected = hash_bytes entry in
                                let actual = read_i32 checksum_buf 0 in
                                if not (Int32.equal expected actual) then
                                  Error "Checksum mismatch"
                                else (
                                  Vector.push entries (TaggedDelete (index_tag, key_bytes));
                                  read_tagged_entries ()))
                        | _ -> Error ("Unknown entry tag: " ^ string_of_int entry_tag)))))
      in
      match read_tagged_entries () with
      | Error e -> Error e
      | Ok () ->
          let iter = Vector.to_mut_iter entries in
          Ok (Iter.MutIterator.to_list iter)

let truncate wal =
  let old_file = Cell.get wal.file in
  let path_v = Path.v wal.path in
  match Fs.File.close old_file with
  | Error _ -> Error ("Failed to close file: ")
  | Ok () -> (
      match Fs.File.create path_v with
      | Error _ -> Error ("Failed to truncate WAL: ")
      | Ok new_file ->
          Cell.set wal.file new_file;
          Ok ())

(* Calculate total size needed for a batch of entries *)
let calculate_batch_size entries =
  List.fold_left
    (fun acc entry ->
      match entry with
      | Put (key, value) ->
          (* tag (1) + key_len (4) + key + value_len (4) + value + checksum (4) *)
          acc + 1 + 4 + Bytes.length key + 4 + Bytes.length value + 4
      | Delete key ->
          (* tag (1) + key_len (4) + key + checksum (4) *)
          acc + 1 + 4 + Bytes.length key + 4)
    0 entries

(* Encode a single entry into a buffer at the given offset *)
let encode_entry_at buffer offset entry =
  match entry with
  | Put (key, value) ->
      let key_len = Bytes.length key in
      let value_len = Bytes.length value in
      let entry_size = 1 + 4 + key_len + 4 + value_len in
      
      (* Build entry *)
      Bytes.set buffer offset (Char.chr put_tag);
      write_u32_at buffer (offset + 1) key_len;
      Bytes.blit key 0 buffer (offset + 5) key_len;
      write_u32_at buffer (offset + 5 + key_len) value_len;
      Bytes.blit value 0 buffer (offset + 9 + key_len) value_len;
      
      (* Calculate checksum *)
      let entry_bytes = Bytes.sub buffer offset entry_size in
      let checksum = hash_bytes entry_bytes in
      write_i32_at buffer (offset + entry_size) checksum;
      
      (* Return new offset *)
      offset + entry_size + 4
      
  | Delete key ->
      let key_len = Bytes.length key in
      let entry_size = 1 + 4 + key_len in
      
      (* Build entry *)
      Bytes.set buffer offset (Char.chr delete_tag);
      write_u32_at buffer (offset + 1) key_len;
      Bytes.blit key 0 buffer (offset + 5) key_len;
      
      (* Calculate checksum *)
      let entry_bytes = Bytes.sub buffer offset entry_size in
      let checksum = hash_bytes entry_bytes in
      write_i32_at buffer (offset + entry_size) checksum;
      
      (* Return new offset *)
      offset + entry_size + 4

(** [append_batch wal entries] atomically appends multiple entries to the WAL.
    All entries are written in a single fsync operation, ensuring atomicity.
    Either all entries are persisted or none are. *)
let append_batch wal entries =
  if List.length entries = 0 then Ok ()
  else
    let file = Cell.get wal.file in
    
    (* Calculate total buffer size *)
    let total_size = calculate_batch_size entries in
    let buffer = Bytes.create total_size in
    
    (* Encode all entries into the buffer *)
    let _ = List.fold_left (fun offset entry -> 
      encode_entry_at buffer offset entry
    ) 0 entries in
    
    (* Single atomic write + fsync *)
    match Fs.File.write file buffer ~offset:0 ~len:total_size with
    | Error _ -> Error "Failed to write batch"
    | Ok _ -> (
        match Fs.File.sync_all file with
        | Error _ -> Error "Failed to sync batch"
        | Ok () -> Ok ())

(* Calculate total size for tagged entries *)
let calculate_tagged_batch_size entries =
  List.fold_left
    (fun acc entry ->
      match entry with
      | TaggedPut (_tag, key, value) ->
          (* index_tag (1) + entry_tag (1) + key_len (4) + key + value_len (4) + value + checksum (4) *)
          acc + 1 + 1 + 4 + Bytes.length key + 4 + Bytes.length value + 4
      | TaggedDelete (_tag, key) ->
          (* index_tag (1) + entry_tag (1) + key_len (4) + key + checksum (4) *)
          acc + 1 + 1 + 4 + Bytes.length key + 4)
    0 entries

(* Encode a tagged entry into buffer at offset *)
let encode_tagged_entry_at buffer offset entry =
  match entry with
  | TaggedPut (index_tag, key, value) ->
      let key_len = Bytes.length key in
      let value_len = Bytes.length value in
      let entry_size = 1 + 1 + 4 + key_len + 4 + value_len in
      
      (* Build entry: index_tag + entry_tag + key_len + key + value_len + value *)
      Bytes.set buffer offset (Char.chr (index_tag_to_byte index_tag));
      Bytes.set buffer (offset + 1) (Char.chr put_tag);
      write_u32_at buffer (offset + 2) key_len;
      Bytes.blit key 0 buffer (offset + 6) key_len;
      write_u32_at buffer (offset + 6 + key_len) value_len;
      Bytes.blit value 0 buffer (offset + 10 + key_len) value_len;
      
      (* Calculate checksum *)
      let entry_bytes = Bytes.sub buffer offset entry_size in
      let checksum = hash_bytes entry_bytes in
      write_i32_at buffer (offset + entry_size) checksum;
      
      offset + entry_size + 4
      
  | TaggedDelete (index_tag, key) ->
      let key_len = Bytes.length key in
      let entry_size = 1 + 1 + 4 + key_len in
      
      (* Build entry: index_tag + entry_tag + key_len + key *)
      Bytes.set buffer offset (Char.chr (index_tag_to_byte index_tag));
      Bytes.set buffer (offset + 1) (Char.chr delete_tag);
      write_u32_at buffer (offset + 2) key_len;
      Bytes.blit key 0 buffer (offset + 6) key_len;
      
      (* Calculate checksum *)
      let entry_bytes = Bytes.sub buffer offset entry_size in
      let checksum = hash_bytes entry_bytes in
      write_i32_at buffer (offset + entry_size) checksum;
      
      offset + entry_size + 4

(** [append_batch_tagged wal entries] atomically appends multiple tagged entries.
    Tagged entries include an index identifier (EAVT, AVET, FACT, SOURCE) so that
    a single WAL can store entries for multiple LSM indices. This enables atomic
    updates across all indices - either all are persisted or none are. *)
let append_batch_tagged wal entries =
  if List.length entries = 0 then Ok ()
  else
    let file = Cell.get wal.file in
    
    (* Calculate total buffer size *)
    let total_size = calculate_tagged_batch_size entries in
    let buffer = Bytes.create total_size in
    
    (* Encode all entries into the buffer *)
    let _ = List.fold_left (fun offset entry ->
      encode_tagged_entry_at buffer offset entry
    ) 0 entries in
    
    (* Single atomic write + fsync *)
    match Fs.File.write file buffer ~offset:0 ~len:total_size with
    | Error _ -> Error "Failed to write tagged batch"
    | Ok _ -> (
        match Fs.File.sync_all file with
        | Error _ -> Error "Failed to sync tagged batch"
        | Ok () -> Ok ())

let close wal =
  let file = Cell.get wal.file in
  match Fs.File.close file with
  | Error _ -> Error ("Failed to close WAL: ")
  | Ok () -> Ok ()
