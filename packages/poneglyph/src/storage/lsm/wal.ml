open Std
open Std.IO
open Std.Collections
open Std.Sync

type t = { path : string; file : Fs.File.t Cell.t }

type entry = Put of bytes * bytes | Delete of bytes

(* Entry type tags *)
let put_tag = 0x01
let delete_tag = 0x02

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

let create ~path =
  let path_v = Path.v path in
  match Fs.File.create path_v with
  | Error _ -> Error ("Failed to create WAL: " ^ path)
  | Ok file -> Ok { path; file = cell file }

let open_existing ~path =
  let path_v = Path.v path in
  match Fs.File.open_read_write path_v with
  | Error _ -> Error ("Failed to open WAL: " ^ path)
  | Ok file -> Ok { path; file = cell file }

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

let close wal =
  let file = Cell.get wal.file in
  match Fs.File.close file with
  | Error _ -> Error ("Failed to close WAL: ")
  | Ok () -> Ok ()
