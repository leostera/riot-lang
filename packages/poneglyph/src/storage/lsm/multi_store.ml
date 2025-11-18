(** Multi-Index LSM Store - Atomic writes across EAVT, AVET, FACT, SOURCE indices *)

open Std
open Std.UUID
open Std.Sync
open Std.Collections
open Model

module Bytes = Kernel.IO.Bytes

type t = {
  data_dir : string;
  global_wal : Wal.t;
  eavt_engine : Engine.t;
  avet_engine : Engine.t;
  fact_engine : Engine.t;
  source_engine : Engine.t;
  uris_engine : Engine.t;
}

(** Helper to close engines when setup fails *)
let cleanup_engines ~wal ~engines =
  List.iter (fun engine -> ignore (Engine.close engine)) engines;
  ignore (Wal.close wal)

(** Helper to open all 5 engines sequentially with proper cleanup on failure *)
let open_all_engines ~global_wal ~engine_config ~eavt_dir ~avet_dir ~fact_dir ~source_dir ~uris_dir =
  match Engine.open_engine (engine_config eavt_dir) with
  | Error e ->
      cleanup_engines ~wal:global_wal ~engines:[];
      Error ("Failed to open EAVT engine: " ^ e)
  | Ok eavt_engine ->
      match Engine.open_engine (engine_config avet_dir) with
      | Error e ->
          cleanup_engines ~wal:global_wal ~engines:[eavt_engine];
          Error ("Failed to open AVET engine: " ^ e)
      | Ok avet_engine ->
          match Engine.open_engine (engine_config fact_dir) with
          | Error e ->
              cleanup_engines ~wal:global_wal ~engines:[eavt_engine; avet_engine];
              Error ("Failed to open FACT engine: " ^ e)
          | Ok fact_engine ->
              match Engine.open_engine (engine_config source_dir) with
              | Error e ->
                  cleanup_engines ~wal:global_wal ~engines:[eavt_engine; avet_engine; fact_engine];
                  Error ("Failed to open SOURCE engine: " ^ e)
              | Ok source_engine ->
                  match Engine.open_engine (engine_config uris_dir) with
                  | Error e ->
                      cleanup_engines ~wal:global_wal ~engines:[eavt_engine; avet_engine; fact_engine; source_engine];
                      Error ("Failed to open URIS engine: " ^ e)
                  | Ok uris_engine ->
                      Ok (eavt_engine, avet_engine, fact_engine, source_engine, uris_engine)

(** Create a new multi-index store with 5 LSM engines and a shared global WAL *)
let create ~data_dir =
  match Fs.create_dir_all (Path.v data_dir) with
  | Error _ -> Error ("Failed to create data directory: " ^ data_dir)
  | Ok () ->
      let wal_path = data_dir ^ "/global.wal" in
      match Wal.create_or_open ~path:wal_path with
      | Error e -> Error ("Failed to create global WAL: " ^ e)
      | Ok global_wal ->
          let eavt_dir = data_dir ^ "/eavt" in
          let avet_dir = data_dir ^ "/avet" in
          let fact_dir = data_dir ^ "/fact" in
          let source_dir = data_dir ^ "/source" in
          let uris_dir = data_dir ^ "/uris" in
          
          let engine_config dir = {
            Engine.data_dir = dir;
            max_memtable_size = 4 * 1024 * 1024;  (* 4MB *)
            compaction_threshold = 4;
          } in
          
          match open_all_engines ~global_wal ~engine_config ~eavt_dir ~avet_dir ~fact_dir ~source_dir ~uris_dir with
          | Error e -> Error e
          | Ok (eavt_engine, avet_engine, fact_engine, source_engine, uris_engine) ->
              Ok {
                data_dir;
                global_wal;
                eavt_engine;
                avet_engine;
                fact_engine;
                source_engine;
                uris_engine;
              }

(** Close all engines and the global WAL *)
let flush_all store =
  (* Flush all engines *)
  let r1 = Engine.flush store.eavt_engine in
  let r2 = Engine.flush store.avet_engine in
  let r3 = Engine.flush store.fact_engine in
  let r4 = Engine.flush store.source_engine in
  let r5 = Engine.flush store.uris_engine in
  
  (* Return first error *)
  match (r1, r2, r3, r4, r5) with
  | Error e, _, _, _, _ -> Error e
  | _, Error e, _, _, _ -> Error e
  | _, _, Error e, _, _ -> Error e
  | _, _, _, Error e, _ -> Error e
  | _, _, _, _, Error e -> Error e
  | Ok (), Ok (), Ok (), Ok (), Ok () -> Ok ()

let close store =
  (* Close engines (which also flushes) *)
  let r1 = Engine.close store.uris_engine in
  let r2 = Engine.close store.source_engine in
  let r3 = Engine.close store.fact_engine in
  let r4 = Engine.close store.avet_engine in
  let r5 = Engine.close store.eavt_engine in
  let r6 = Wal.close store.global_wal in
  
  (* Return first error *)
  match (r1, r2, r3, r4, r5, r6) with
  | Error e, _, _, _, _, _ -> Error e
  | _, Error e, _, _, _, _ -> Error e
  | _, _, Error e, _, _, _ -> Error e
  | _, _, _, Error e, _, _ -> Error e
  | _, _, _, _, Error e, _ -> Error e
  | _, _, _, _, _, Error e -> Error e
  | Ok (), Ok (), Ok (), Ok (), Ok (), Ok () -> Ok ()

(** Encode a fact value for storage in FACT index 
    
    NEW Format (with full SHA-256):
    [fact_uri_sha256:32]   - Full SHA-256 hash
    [entity_uri_sha256:32] - Full SHA-256 hash
    [attr_uri_sha256:32]   - Full SHA-256 hash
    [source_uri_sha256:32] - Full SHA-256 hash
    [value_kind:1]         - Value type tag
    [value_data:variable]  - For String: [len:4][data:N], For URI: [sha256:32], Others: [value:8]
    [stated_at_micros:8]   - Timestamp
    [retracted:1]          - Boolean flag
*)
let encode_fact_value fact =
  (* Extract SHA-256 hashes from URI records *)
  let fact_sha256 = fact.Fact.fact_uri.Uri.sha256 in
  let entity_sha256 = fact.Fact.entity.Uri.sha256 in
  let attr_sha256 = fact.Fact.attribute.Uri.sha256 in
  let source_sha256 = fact.Fact.source_uri.Uri.sha256 in
  
  (* Encode value *)
  let value_kind, value_repr = Encoding.encode_value fact.Fact.value in
  let value_repr_int64 = Encoding.value_repr_to_int64 value_repr in
  
  (* Get stated_at as int64 microseconds *)
  let stated_at_micros = Datetime.to_unix_micros fact.Fact.stated_at in
  
  (* Calculate total size *)
  let value_size = match fact.Fact.value with
    | Fact.String s -> 1 + 4 + String.length s  (* kind + len + data *)
    | Fact.Uri _ -> 1 + 32  (* kind + sha256 *)
    | _ -> 1 + 8  (* kind + int64 *)
  in
  let total_size = 
    32 +          (* fact_sha256 *)
    32 +          (* entity_sha256 *)
    32 +          (* attr_sha256 *)
    32 +          (* source_sha256 *)
    value_size +  (* variable-length value *)
    8 +           (* stated_at *)
    1             (* retracted *)
  in
  
  let buf = Bytes.create total_size in
  let pos = cell 0 in
  
  (* Helper to write *)
  let write_u32 v =
    Bytes.set_int32_be buf (Cell.get pos) (Int32.of_int v);
    Cell.set pos (Cell.get pos + 4)
  in
  let write_u8 v =
    Bytes.set_uint8 buf (Cell.get pos) v;
    Cell.set pos (Cell.get pos + 1)
  in
  let write_i64 v =
    Bytes.set_int64_be buf (Cell.get pos) v;
    Cell.set pos (Cell.get pos + 8)
  in
  let write_bytes b =
    let len = Bytes.length b in
    Bytes.blit b 0 buf (Cell.get pos) len;
    Cell.set pos (Cell.get pos + len)
  in
  let write_string s =
    let len = String.length s in
    Bytes.blit_string s 0 buf (Cell.get pos) len;
    Cell.set pos (Cell.get pos + len)
  in
  
  (* Write URI SHA-256 hashes (32 bytes each) *)
  write_bytes fact_sha256;
  write_bytes entity_sha256;
  write_bytes attr_sha256;
  write_bytes source_sha256;
  
  (* Write value - variable length based on type *)
  write_u8 (Encoding.value_kind_to_byte value_kind);
  (match fact.Fact.value with
   | Fact.String s ->
       (* String: write length + data *)
       write_u32 (String.length s);
       write_string s
   | Fact.Uri uri ->
       (* URI: write SHA-256 hash *)
       write_bytes uri.Uri.sha256
   | _ ->
       (* Other types: write as int64 *)
       write_i64 value_repr_int64
  );
  
  (* Write stated_at *)
  write_i64 stated_at_micros;
  
  (* Write retracted flag *)
  write_u8 (if fact.Fact.retracted then 1 else 0);
  
  buf

(** Helper: Lookup URI from URIS index by SHA-256 hash
    
    NOTE: This should only be called during fact decoding where the URI
    was previously stored. For query URIs, use ensure_uri instead. *)
let lookup_uri store sha256_bytes =
  (* Key is 32-byte SHA-256 hash + 9 bytes padding = 41 bytes *)
  let key = Bytes.make 41 '\x00' in
  Bytes.blit sha256_bytes 0 key 0 32;
  
  match Engine.get store.uris_engine ~key with
  | None -> 
      (* Database corruption - URI should exist *)
      let hex = Data.Base16.encode_bytes sha256_bytes in
      Log.error ("[URI-LOOKUP-FAIL] URI not found: sha256=" ^ hex);
      panic ("URI not found in URIS index: " ^ hex)
  | Some value ->
      let uri_string = Bytes.to_string value in
      { Uri.uri = uri_string; sha256 = sha256_bytes }

(** Helper: Ensure a URI exists in the URIS index (get or insert)
    
    This is used when we have a complete Uri.t (with both string and hash)
    and want to ensure it's in the URIS index for future lookups. *)
let ensure_uri store uri =
  let key = Bytes.make 41 '\x00' in
  Bytes.blit uri.Uri.sha256 0 key 0 32;
  
  match Engine.get store.uris_engine ~key with
  | Some _ -> ()  (* Already exists *)
  | None ->
      (* Insert it *)
      let value = Bytes.of_string uri.Uri.uri in
      let _ = Engine.put store.uris_engine ~key ~value in
      Log.info ("[URI-ENSURE] Inserted missing URI: " ^ uri.Uri.uri)

(** Decode fact value from FACT index *)
let decode_fact_value store tx_id bytes =
  let pos = cell 0 in
  
  let read_u32 () =
    let v = Bytes.get_int32_be bytes (Cell.get pos) in
    Cell.set pos (Cell.get pos + 4);
    Int32.to_int v
  in
  let read_u8 () =
    let v = Bytes.get_uint8 bytes (Cell.get pos) in
    Cell.set pos (Cell.get pos + 1);
    v
  in
  let read_i64 () =
    let v = Bytes.get_int64_be bytes (Cell.get pos) in
    Cell.set pos (Cell.get pos + 8);
    v
  in
  let read_bytes len =
    let b = Bytes.sub bytes (Cell.get pos) len in
    Cell.set pos (Cell.get pos + len);
    b
  in
  let read_string len =
    let s = Bytes.sub_string bytes (Cell.get pos) len in
    Cell.set pos (Cell.get pos + len);
    s
  in
  
  (* Read SHA-256 hashes (32 bytes each) and lookup URIs from URIS index *)
  let fact_sha256 = read_bytes 32 in
  let entity_sha256 = read_bytes 32 in
  let attr_sha256 = read_bytes 32 in
  let source_sha256 = read_bytes 32 in
  
  (* Lookup with context for debugging *)
  let lookup_with_context ctx sha256 =
    let key = Bytes.make 41 '\x00' in
    Bytes.blit sha256 0 key 0 32;
    let hex = Data.Base16.encode_bytes sha256 in
    match Engine.get store.uris_engine ~key with
    | None ->
        Log.error ("[URI-LOOKUP-FAIL] " ^ ctx ^ " URI not found: " ^ hex);
        Log.error ("[URI-LOOKUP-FAIL] Key length: " ^ string_of_int (Bytes.length key) ^ ", SHA256 length: " ^ string_of_int (Bytes.length sha256));
        panic ("URI not found in URIS index (" ^ ctx ^ "): " ^ hex)
    | Some value ->
        { Uri.uri = Bytes.to_string value; sha256 }
  in
  
  let fact_uri = lookup_with_context "fact_uri" fact_sha256 in
  let entity = lookup_with_context "entity" entity_sha256 in
  let attribute = lookup_with_context "attribute" attr_sha256 in
  let source_uri = lookup_with_context "source" source_sha256 in
  
  (* Read value - variable length based on kind *)
  let value_kind_byte = read_u8 () in
  let value_kind = Encoding.value_kind_of_byte value_kind_byte in
  let value = match value_kind with
    | Encoding.VK_String ->
        (* String: read length + data *)
        let str_len = read_u32 () in
        let str_data = read_string str_len in
        Fact.String str_data
    | Encoding.VK_Uri ->
        (* URI: read SHA-256 hash and lookup from URIS index *)
        let uri_sha256 = read_bytes 32 in
        let uri = lookup_uri store uri_sha256 in
        Fact.Uri uri
    | _ ->
        (* Other types: read as int64 and decode *)
        let value_repr_int64 = read_i64 () in
        let value_repr = Encoding.int64_to_value_repr value_kind value_repr_int64 in
        Encoding.decode_value value_kind value_repr
  in
  
  (* Read stated_at *)
  let stated_at_micros = read_i64 () in
  let stated_at = Datetime.from_unix_micros stated_at_micros in
  
  (* Read retracted *)
  let retracted_byte = read_u8 () in
  let retracted = retracted_byte != 0 in
  
  {
    Fact.fact_uri;
    source_uri;
    entity;
    attribute;
    value;
    stated_at;
    tx_id;
    retracted;
  }

(** State operation - atomically write facts to all 4 indices *)
let state store facts =
  (* Generate transaction ID (UUIDv7 for time-ordering and monotonicity) *)
  let tx_id = UUID.v7_monotonic () in
  
  (* Extract unique URIs from facts - use HashMap keyed by sha256 hex *)
  let uri_map = HashMap.create () in
  let uri_value_count = cell 0 in
  List.iter (fun fact ->
    (* Use hex encoding of SHA-256 as map key for perfect deduplication *)
    let hex_e = Data.Base16.encode_bytes fact.Fact.entity.Uri.sha256 in
    let hex_a = Data.Base16.encode_bytes fact.Fact.attribute.Uri.sha256 in
    let hex_f = Data.Base16.encode_bytes fact.Fact.fact_uri.Uri.sha256 in
    let hex_s = Data.Base16.encode_bytes fact.Fact.source_uri.Uri.sha256 in
    let _ = HashMap.insert uri_map hex_e fact.Fact.entity in
    let _ = HashMap.insert uri_map hex_a fact.Fact.attribute in
    let _ = HashMap.insert uri_map hex_f fact.Fact.fact_uri in
    let _ = HashMap.insert uri_map hex_s fact.Fact.source_uri in
    (* Also extract URI from value if it's a URI type *)
    (match fact.Fact.value with
     | Fact.Uri uri -> 
         Cell.set uri_value_count (Cell.get uri_value_count + 1);
         let hex_v = Data.Base16.encode_bytes uri.Uri.sha256 in
         let _ = HashMap.insert uri_map hex_v uri in ()
     | _ -> ());
    ()
  ) facts;
  
  (* Build URI entries: SHA-256 hash (32 bytes) -> URI string *)
  let uri_list = HashMap.to_list uri_map in
  
  let uri_entries = List.map (fun (_hex, uri) ->
    let key = Bytes.make 41 '\x00' in  (* 41-byte key: 32 bytes hash + 9 padding *)
    Bytes.blit uri.Uri.sha256 0 key 0 32;
    let value = Bytes.of_string uri.Uri.uri in
    (key, value)
  ) uri_list in
  
  (* Build WAL batch with URIS tag *)
  let uri_wal_entries = List.map (fun (key, value) ->
    Wal.TaggedPut (Wal.URIS, key, value)
  ) uri_entries in
  
  (* Build batch entries for all indices atomically *)
  let fact_batch = List.concat_map (fun fact ->
    (* Convert URIs to int64 IDs *)
    let entity_id = Key.uri_to_id fact.Fact.entity in
    let attr_id = Key.uri_to_id fact.Fact.attribute in
    let fact_id = Key.uri_to_id fact.Fact.fact_uri in
    let source_id = Key.uri_to_id fact.Fact.source_uri in
    
    (* Get value representation *)
    let value_kind, value_repr = Encoding.encode_value fact.Fact.value in
    let value_repr_int64 = Encoding.value_repr_to_int64 value_repr in
    
    (* Get tx_id as int64 - extract first 8 bytes *)
    let tx_id_bytes = UUID.to_bytes tx_id in
    let tx_id_int64 = Bytes.get_int64_be tx_id_bytes 0 in
    
    (* Build keys *)
    let eavt_key = Key.encode_eavt {
      entity_id;
      attr_id;
      value_kind;
      value_repr = value_repr_int64;
      tx_id = tx_id_int64;
      fact_id;
    } in
    
    let avet_key = Key.encode_avet {
      attr_id;
      value_kind;
      value_repr = value_repr_int64;
      entity_id;
      tx_id = tx_id_int64;
      fact_id;
    } in
    
    let fact_key = Key.encode_fact {
      fact_id;
      tx_id = tx_id_int64;
    } in
    
    let source_key = Key.encode_source {
      source_id;
      entity_id;
      attr_id;
      tx_id = tx_id_int64;
      fact_id;
    } in
    
    (* Index marker value (non-empty to avoid tombstone filtering) *)
    let index_marker = Bytes.create 1 in
    Bytes.set_uint8 index_marker 0 1;
    let fact_value = encode_fact_value fact in
    
    (* Create tagged batch entries *)
    [
      Wal.TaggedPut (Wal.EAVT, eavt_key, index_marker);
      Wal.TaggedPut (Wal.AVET, avet_key, index_marker);
      Wal.TaggedPut (Wal.FACT, fact_key, fact_value);
      Wal.TaggedPut (Wal.SOURCE, source_key, index_marker);
    ]
  ) facts in
  
  (* Combine URI entries with fact entries *)
  let batch = uri_wal_entries @ fact_batch in
  
  (* Write to global WAL atomically *)
  (match Wal.append_batch_tagged store.global_wal batch with
  | Error e -> Error ("Global WAL write failed: " ^ e)
  | Ok () ->
      (* Route entries to individual engines - BATCHED for performance! *)
      (* Collect entries by index tag, then do batch writes *)
      let eavt_entries = cell [] in
      let avet_entries = cell [] in
      let fact_entries = cell [] in
      let source_entries = cell [] in
      let uris_entries = cell [] in
      
      List.iter (fun entry ->
        match entry with
        | Wal.TaggedPut (Wal.EAVT, key, value) -> Cell.set eavt_entries ((key, value) :: Cell.get eavt_entries)
        | Wal.TaggedPut (Wal.AVET, key, value) -> Cell.set avet_entries ((key, value) :: Cell.get avet_entries)
        | Wal.TaggedPut (Wal.FACT, key, value) -> Cell.set fact_entries ((key, value) :: Cell.get fact_entries)
        | Wal.TaggedPut (Wal.SOURCE, key, value) -> Cell.set source_entries ((key, value) :: Cell.get source_entries)
        | Wal.TaggedPut (Wal.URIS, key, value) -> Cell.set uris_entries ((key, value) :: Cell.get uris_entries)
        | Wal.TaggedDelete _ -> ()  (* Not used in state operation *)
      ) batch;
      
      (* Batch write to each index - MUCH faster than individual puts! *)
      (* Write URIS first so they're available for reads *)
      let uris_list = Cell.get uris_entries in
      (match Engine.put_batch store.uris_engine ~entries:(List.rev uris_list) with
      | Error e -> Error ("URIS batch write failed: " ^ e)
      | Ok () -> (
        let eavt_list_rev = List.rev (Cell.get eavt_entries) in
        match Engine.put_batch store.eavt_engine ~entries:eavt_list_rev with
          | Error e -> Error ("EAVT batch write failed: " ^ e)
          | Ok () -> (
              match Engine.put_batch store.avet_engine ~entries:(List.rev (Cell.get avet_entries)) with
              | Error e -> Error ("AVET batch write failed: " ^ e)
              | Ok () -> (
                  match Engine.put_batch store.fact_engine ~entries:(List.rev (Cell.get fact_entries)) with
                  | Error e -> Error ("FACT batch write failed: " ^ e)
                  | Ok () -> (
                      match Engine.put_batch store.source_engine ~entries:(List.rev (Cell.get source_entries)) with
                      | Error e -> Error ("SOURCE batch write failed: " ^ e)
                      | Ok () -> Ok (List.length facts)))))))

(** Retract facts - writes facts with retracted=true *)
let retract store facts =
  (* Generate transaction ID *)
  let tx_id = UUID.v7_monotonic () in
  
  (* Extract unique URIs from facts - use HashMap keyed by sha256 hex *)
  let uri_map = HashMap.create () in
  List.iter (fun fact ->
    let hex_e = Data.Base16.encode_bytes fact.Fact.entity.Uri.sha256 in
    let hex_a = Data.Base16.encode_bytes fact.Fact.attribute.Uri.sha256 in
    let hex_f = Data.Base16.encode_bytes fact.Fact.fact_uri.Uri.sha256 in
    let hex_s = Data.Base16.encode_bytes fact.Fact.source_uri.Uri.sha256 in
    let _ = HashMap.insert uri_map hex_e fact.Fact.entity in
    let _ = HashMap.insert uri_map hex_a fact.Fact.attribute in
    let _ = HashMap.insert uri_map hex_f fact.Fact.fact_uri in
    let _ = HashMap.insert uri_map hex_s fact.Fact.source_uri in
    (* Also extract URI from value if it's a URI type *)
    (match fact.Fact.value with
     | Fact.Uri uri -> 
         let hex_v = Data.Base16.encode_bytes uri.Uri.sha256 in
         let _ = HashMap.insert uri_map hex_v uri in ()
     | _ -> ());
    ()
  ) facts;
  
  (* Build URI entries: SHA-256 hash (32 bytes) -> URI string *)
  let uri_entries = HashMap.to_list uri_map |> List.map (fun (_hex, uri) ->
    let key = Bytes.make 41 '\x00' in  (* 41-byte key: 32 bytes hash + 9 padding *)
    Bytes.blit uri.Uri.sha256 0 key 0 32;
    let value = Bytes.of_string uri.Uri.uri in
    (key, value)
  ) in
  
  (* Build WAL batch with URIS tag *)
  let uri_wal_entries = List.map (fun (key, value) ->
    Wal.TaggedPut (Wal.URIS, key, value)
  ) uri_entries in
  
  (* Build tagged batch for all indices, marking facts as retracted *)
  let fact_batch = List.concat_map (fun fact ->
    (* Convert URIs to int64 IDs *)
    let entity_id = Key.uri_to_id fact.Fact.entity in
    let attr_id = Key.uri_to_id fact.Fact.attribute in
    let fact_id = Key.uri_to_id fact.Fact.fact_uri in
    let source_id = Key.uri_to_id fact.Fact.source_uri in
    
    (* Get value representation *)
    let value_kind, value_repr = Encoding.encode_value fact.Fact.value in
    let value_repr_int64 = Encoding.value_repr_to_int64 value_repr in
    
    (* Get tx_id as int64 - extract first 8 bytes *)
    let tx_id_bytes = UUID.to_bytes tx_id in
    let tx_id_int64 = Bytes.get_int64_be tx_id_bytes 0 in
    
    (* Build keys *)
    let eavt_key = Key.encode_eavt {
      entity_id;
      attr_id;
      value_kind;
      value_repr = value_repr_int64;
      tx_id = tx_id_int64;
      fact_id;
    } in
    
    let avet_key = Key.encode_avet {
      attr_id;
      value_kind;
      value_repr = value_repr_int64;
      entity_id;
      tx_id = tx_id_int64;
      fact_id;
    } in
    
    let fact_key = Key.encode_fact {
      fact_id;
      tx_id = tx_id_int64;
    } in
    
    let source_key = Key.encode_source {
      source_id;
      entity_id;
      attr_id;
      tx_id = tx_id_int64;
      fact_id;
    } in
    
    (* Index marker value (non-empty to avoid tombstone filtering) *)
    let index_marker = Bytes.create 1 in
    Bytes.set_uint8 index_marker 0 1;
    
    (* Encode fact value with retracted=true *)
    let retracted_fact = { fact with Fact.retracted = true } in
    let fact_value = encode_fact_value retracted_fact in
    
    (* Create tagged batch entries *)
    [
      Wal.TaggedPut (Wal.EAVT, eavt_key, index_marker);
      Wal.TaggedPut (Wal.AVET, avet_key, index_marker);
      Wal.TaggedPut (Wal.FACT, fact_key, fact_value);
      Wal.TaggedPut (Wal.SOURCE, source_key, index_marker);
    ]
  ) facts in
  
  (* Combine URI and fact batches *)
  let combined_batch = List.concat [uri_wal_entries; fact_batch] in
  
  (* Write to global WAL atomically *)
  (match Wal.append_batch_tagged store.global_wal combined_batch with
  | Error e -> Error ("Global WAL write failed: " ^ e)
  | Ok () ->
      (* Route entries to individual engines - BATCHED for performance! *)
      let eavt_entries = cell [] in
      let avet_entries = cell [] in
      let fact_entries = cell [] in
      let source_entries = cell [] in
      let uris_entries = cell [] in
      
      List.iter (fun entry ->
        match entry with
        | Wal.TaggedPut (Wal.EAVT, key, value) -> Cell.set eavt_entries ((key, value) :: Cell.get eavt_entries)
        | Wal.TaggedPut (Wal.AVET, key, value) -> Cell.set avet_entries ((key, value) :: Cell.get avet_entries)
        | Wal.TaggedPut (Wal.FACT, key, value) -> Cell.set fact_entries ((key, value) :: Cell.get fact_entries)
        | Wal.TaggedPut (Wal.SOURCE, key, value) -> Cell.set source_entries ((key, value) :: Cell.get source_entries)
        | Wal.TaggedPut (Wal.URIS, key, value) -> Cell.set uris_entries ((key, value) :: Cell.get uris_entries)
        | Wal.TaggedDelete _ -> ()  (* Not used in retract operation *)
      ) combined_batch;
      
      (* Batch write to each index - write URIS first *)
      (match Engine.put_batch store.uris_engine ~entries:(List.rev (Cell.get uris_entries)) with
      | Error e -> Error ("URIS batch write failed: " ^ e)
      | Ok () -> (
          match Engine.put_batch store.eavt_engine ~entries:(List.rev (Cell.get eavt_entries)) with
      | Error e -> Error ("EAVT batch write failed: " ^ e)
      | Ok () -> (
          match Engine.put_batch store.avet_engine ~entries:(List.rev (Cell.get avet_entries)) with
          | Error e -> Error ("AVET batch write failed: " ^ e)
          | Ok () -> (
              match Engine.put_batch store.fact_engine ~entries:(List.rev (Cell.get fact_entries)) with
              | Error e -> Error ("FACT batch write failed: " ^ e)
              | Ok () -> (
                  match Engine.put_batch store.source_engine ~entries:(List.rev (Cell.get source_entries)) with
                  | Error e -> Error ("SOURCE batch write failed: " ^ e)
                  | Ok () -> Ok (List.length facts)))))))

(** Query operations *)

(** Get all facts for an entity *)
let get_entity_facts store ~entity =
  let open Std.IO in
  
  (* Convert entity URI to ID *)
  let entity_id = Key.uri_to_id entity in
  
  (* Build EAVT prefix: just entity_id (8 bytes) *)
  let prefix = Bytes.create 8 in
  Bytes.set_int64_be prefix 0 entity_id;
  
  (* Scan EAVT index - STREAM, don't materialize! *)
  let eavt_results = Engine.scan_prefix store.eavt_engine ~prefix in
  
  (* Build deduplication map by streaming through results *)
  let fact_map = HashMap.create () in
  
  Iter.MutIterator.for_each eavt_results ~fn:(fun (eavt_key, _value) ->
    (* Decode EAVT key to get fact_id and tx_id *)
    let decoded = Key.decode_eavt eavt_key in
    
    (* Build FACT key *)
    let fact_key = Key.encode_fact {
      fact_id = decoded.fact_id;
      tx_id = decoded.tx_id;
    } in
    
    (* Lookup in FACT index *)
    match Engine.get store.fact_engine ~key:fact_key with
    | None -> ()  (* Skip if fact not found *)
    | Some fact_value ->
        (* Reconstruct UUID from tx_id int64 *)
        let tx_id_bytes = Bytes.create 16 in
        Bytes.set_int64_be tx_id_bytes 0 decoded.tx_id;
        (* Fill rest with zeros - good enough for MVP *)
        let tx_id = match UUID.of_bytes tx_id_bytes with
          | Ok uuid -> uuid
          | Error _ -> UUID.v4 ()  (* Fallback *)
        in
        
        (* Decode fact from FACT value (includes fact_uri) *)
        let fact = decode_fact_value store tx_id fact_value in
        
        (* Create deduplication key from attribute + value *)
        let attr_str = Uri.to_string fact.Fact.attribute in
        let value_str = match fact.Fact.value with
          | Fact.Uri u -> "uri:" ^ Uri.to_string u
          | Fact.Int i -> "int:" ^ string_of_int i
          | Fact.Bool b -> "bool:" ^ string_of_bool b
          | Fact.String s -> "str:" ^ s
          | Fact.Float f -> "float:" ^ string_of_float f
          | Fact.DateTime dt -> "dt:" ^ Datetime.to_iso8601 dt
        in
        let map_key = attr_str ^ "|" ^ value_str in
        
        (* Check if we already have this attr-value pair *)
        match HashMap.get fact_map map_key with
        | None -> 
            let _ = HashMap.insert fact_map map_key fact in
            ()
        | Some existing_fact ->
            (* Keep the fact with the higher tx_id (more recent) *)
            let existing_tx_id_bytes = UUID.to_bytes existing_fact.Fact.tx_id in
            let existing_tx_id_int64 = Bytes.get_int64_be existing_tx_id_bytes 0 in
            
            let new_tx_id_bytes = UUID.to_bytes fact.Fact.tx_id in
            let new_tx_id_int64 = Bytes.get_int64_be new_tx_id_bytes 0 in
            
            if new_tx_id_int64 > existing_tx_id_int64 then
              let _ = HashMap.insert fact_map map_key fact in
              ()
  );
  
  (* Extract facts from map and filter out retracted ones *)
  let latest_facts = vec [] in
  HashMap.iter (fun _key fact ->
    if not fact.Fact.retracted then
      Vector.push latest_facts fact
  ) fact_map;
  
  Vector.to_mut_iter latest_facts

(** Find entities that have a specific attribute-value pair *)
let find_entities_by_attr_value store ~attribute ~value =
  (* Convert attribute URI to ID *)
  let attr_id = Key.uri_to_id attribute in
  
  (* Encode the value *)
  let value_kind, value_repr = Encoding.encode_value value in
  let value_repr_int64 = Encoding.value_repr_to_int64 value_repr in
  
  (* Build AVET prefix: attr_id (8 bytes) + value_kind (1 byte) + value_repr (8 bytes) = 17 bytes *)
  let prefix = Bytes.make 17 '\x00' in
  Bytes.set_int64_be prefix 0 attr_id;
  Bytes.set_uint8 prefix 8 (Encoding.value_kind_to_byte value_kind);
  Bytes.set_int64_be prefix 9 value_repr_int64;
  
  (* Scan AVET index - STREAM, don't materialize! *)
  let avet_results = Engine.scan_prefix store.avet_engine ~prefix in
  
  (* Build deduplication map by streaming through results *)
  let fact_map = HashMap.create () in
  
  Iter.MutIterator.for_each avet_results ~fn:(fun (avet_key, _value) ->
    (* Decode AVET key to get entity_id, fact_id, tx_id *)
    let decoded = Key.decode_avet avet_key in
    
    (* Build FACT key to get full fact *)
    let fact_key = Key.encode_fact {
      fact_id = decoded.fact_id;
      tx_id = decoded.tx_id;
    } in
    
    (* Lookup in FACT index *)
    match Engine.get store.fact_engine ~key:fact_key with
    | None -> ()  (* Skip if fact not found *)
    | Some fact_value ->
        (* Reconstruct UUID from tx_id int64 *)
        let tx_id_bytes = Bytes.make 16 '\x00' in
        Bytes.set_int64_be tx_id_bytes 0 decoded.tx_id;
        let tx_id = match UUID.of_bytes tx_id_bytes with
          | Ok uuid -> uuid
          | Error _ -> UUID.v4 ()
        in
        
        (* Decode fact *)
        let fact = decode_fact_value store tx_id fact_value in
        
        let entity_str = Uri.to_string fact.Fact.entity in
        
        match HashMap.get fact_map entity_str with
        | None -> 
            let _ = HashMap.insert fact_map entity_str fact in
            ()
        | Some existing_fact ->
            (* Keep the fact with the higher tx_id (more recent) *)
            let existing_tx_id_bytes = UUID.to_bytes existing_fact.Fact.tx_id in
            let existing_tx_id_int64 = Bytes.get_int64_be existing_tx_id_bytes 0 in
            
            let new_tx_id_bytes = UUID.to_bytes fact.Fact.tx_id in
            let new_tx_id_int64 = Bytes.get_int64_be new_tx_id_bytes 0 in
            
            if new_tx_id_int64 > existing_tx_id_int64 then
              let _ = HashMap.insert fact_map entity_str fact in
              ()
  );
  
  (* Extract entities from non-retracted facts *)
  let entities = vec [] in
  HashMap.iter (fun _entity_str fact ->
    if not fact.Fact.retracted then
      Vector.push entities fact.Fact.entity
  ) fact_map;
  
  Vector.to_mut_iter entities

(** Get all facts for a specific attribute using AVET index - optimized query! *)
let get_facts_by_attribute store ~attribute =
  (* Convert attribute URI to ID *)
  let attr_id = Key.uri_to_id attribute in
  
  (* Build AVET prefix: just attr_id (8 bytes) to get ALL facts for this attribute *)
  let prefix = Bytes.create 8 in
  Bytes.set_int64_be prefix 0 attr_id;
  
  Log.info ("LSM get_facts_by_attribute: scanning AVET for attr=" ^ Uri.to_string attribute);
  
  (* Scan AVET index with attribute prefix *)
  let avet_results = Engine.scan_prefix store.avet_engine ~prefix in
  
  (* Group by (entity, value) and keep only the latest version (highest tx_id) *)
  (* Process streaming - build HashMap as we iterate *)
  let fact_map = HashMap.create () in
  
  Iter.MutIterator.for_each avet_results ~fn:(fun (avet_key, _value) ->
    (* Decode AVET key to get fact_id and tx_id *)
    let decoded = Key.decode_avet avet_key in
    
    (* Build FACT key to get full fact *)
    let fact_key = Key.encode_fact {
      fact_id = decoded.fact_id;
      tx_id = decoded.tx_id;
    } in
    
    (* Lookup in FACT index *)
    match Engine.get store.fact_engine ~key:fact_key with
    | None -> ()  (* Skip if fact not found *)
    | Some fact_value ->
        (* Reconstruct UUID from tx_id int64 *)
        let tx_id_bytes = Bytes.make 16 '\x00' in
        Bytes.set_int64_be tx_id_bytes 0 decoded.tx_id;
        let tx_id = match UUID.of_bytes tx_id_bytes with
          | Ok uuid -> uuid
          | Error _ -> UUID.v4 ()
        in
        
        (* Decode fact *)
        let fact = decode_fact_value store tx_id fact_value in
        
        (* Add to dedup map *)
        let entity_str = Uri.to_string fact.Fact.entity in
        let value_str = match fact.Fact.value with
          | Fact.Uri u -> "uri:" ^ Uri.to_string u
          | Fact.Int i -> "int:" ^ string_of_int i
          | Fact.Bool b -> "bool:" ^ string_of_bool b
          | Fact.String s -> "str:" ^ s
          | Fact.Float f -> "float:" ^ string_of_float f
          | Fact.DateTime dt -> "dt:" ^ Datetime.to_iso8601 dt
        in
        let map_key = entity_str ^ "|" ^ value_str in
        
        (* Check if we already have this entity-value pair *)
        match HashMap.get fact_map map_key with
        | None -> 
            let _ = HashMap.insert fact_map map_key fact in
            ()
        | Some existing_fact ->
            (* Keep the fact with the higher tx_id (more recent) *)
            let existing_tx_id_bytes = UUID.to_bytes existing_fact.Fact.tx_id in
            let existing_tx_id_int64 = Bytes.get_int64_be existing_tx_id_bytes 0 in
            
            let new_tx_id_bytes = UUID.to_bytes fact.Fact.tx_id in
            let new_tx_id_int64 = Bytes.get_int64_be new_tx_id_bytes 0 in
            
            if new_tx_id_int64 > existing_tx_id_int64 then
              let _ = HashMap.insert fact_map map_key fact in
              ()
  );
  
  (* Extract non-retracted facts *)
  let latest_facts = vec [] in
  HashMap.iter (fun _key fact ->
    if not fact.Fact.retracted then
      Vector.push latest_facts fact
  ) fact_map;
  
  Vector.to_mut_iter latest_facts

(** Get all current (non-retracted) facts - expensive operation! *)
let get_all_current_facts store =
  let open Std.IO in
  
  Log.debug "LSM get_all_current_facts: scanning EAVT entries (streaming)";
  
  (* Scan entire EAVT index with empty prefix - STREAM, don't materialize! *)
  let prefix = Bytes.create 0 in
  let eavt_results = Engine.scan_prefix store.eavt_engine ~prefix in
  
  (* Build deduplication map by streaming through results *)
  let fact_map = HashMap.create () in
  let scan_count = cell 0 in
  
  Iter.MutIterator.for_each eavt_results ~fn:(fun (eavt_key, _value) ->
    Cell.set scan_count (Cell.get scan_count + 1);
    
    (* Decode EAVT key to get fact_id and tx_id *)
    let decoded = Key.decode_eavt eavt_key in
    
    (* DEBUG: Check for Symbol52 *)
    let entity_id_bytes = Bytes.create 8 in
    Bytes.set_int64_be entity_id_bytes 0 decoded.entity_id;
    (* Build FACT key *)
    let fact_key = Key.encode_fact {
      fact_id = decoded.fact_id;
      tx_id = decoded.tx_id;
    } in
    
    (* Lookup in FACT index *)
    match Engine.get store.fact_engine ~key:fact_key with
    | None -> ()  (* Skip if fact not found *)
    | Some fact_value ->
        (* Reconstruct UUID from tx_id int64 *)
        let tx_id_bytes = Bytes.create 16 in
        Bytes.set_int64_be tx_id_bytes 0 decoded.tx_id;
        let tx_id = match UUID.of_bytes tx_id_bytes with
          | Ok uuid -> uuid
          | Error _ -> UUID.v4 ()
        in
        
        (* Decode fact from FACT value *)
        let fact = decode_fact_value store tx_id fact_value in
        
        (* Create deduplication key from entity + attribute + value *)
        let entity_str = Uri.to_string fact.Fact.entity in
        let attr_str = Uri.to_string fact.Fact.attribute in
        let value_str = match fact.Fact.value with
          | Fact.Uri u -> "uri:" ^ Uri.to_string u
          | Fact.Int i -> "int:" ^ string_of_int i
          | Fact.Bool b -> "bool:" ^ string_of_bool b
          | Fact.String s -> "str:" ^ s
          | Fact.Float f -> "float:" ^ string_of_float f
          | Fact.DateTime dt -> "dt:" ^ Datetime.to_iso8601 dt
        in
        let map_key = entity_str ^ "|" ^ attr_str ^ "|" ^ value_str in
        
        (* Check if we already have this fact *)
        match HashMap.get fact_map map_key with
        | None -> 
            let _ = HashMap.insert fact_map map_key fact in
            ()
        | Some existing_fact ->
            (* Keep the fact with the higher tx_id (more recent) *)
            let existing_tx_id_bytes = UUID.to_bytes existing_fact.Fact.tx_id in
            let existing_tx_id_int64 = Bytes.get_int64_be existing_tx_id_bytes 0 in
            
            let new_tx_id_bytes = UUID.to_bytes fact.Fact.tx_id in
            let new_tx_id_int64 = Bytes.get_int64_be new_tx_id_bytes 0 in
            
            if new_tx_id_int64 > existing_tx_id_int64 then
              let _ = HashMap.insert fact_map map_key fact in
              ()
  );
  
  (* Extract non-retracted facts *)
  let latest_facts = vec [] in
  HashMap.iter (fun _key fact ->
    if not fact.Fact.retracted then
      Vector.push latest_facts fact
  ) fact_map;
  
  Vector.to_mut_iter latest_facts

(** Compact one tier across all indices *)
let compact_tier store ~tier ~threshold ?(max_merge=4) () =
  let r1 = Engine.compact_one_tier store.eavt_engine ~tier ~threshold ~max_merge () in
  let r2 = Engine.compact_one_tier store.avet_engine ~tier ~threshold ~max_merge () in
  let r3 = Engine.compact_one_tier store.fact_engine ~tier ~threshold ~max_merge () in
  let r4 = Engine.compact_one_tier store.source_engine ~tier ~threshold ~max_merge () in
  
  match (r1, r2, r3, r4) with
  | Error e, _, _, _ | _, Error e, _, _ | _, _, Error e, _ | _, _, _, Error e -> 
      Error e
  | Ok b1, Ok b2, Ok b3, Ok b4 -> 
      Ok (b1 || b2 || b3 || b4)  (* True if any compaction happened *)

(** Check if any index needs compaction *)
let needs_compaction store ~threshold =
  Engine.needs_compaction store.eavt_engine ||
  Engine.needs_compaction store.avet_engine ||
  Engine.needs_compaction store.fact_engine ||
  Engine.needs_compaction store.source_engine

(** Get all facts for a specific source using SOURCE index - optimized query! *)
let get_facts_by_source store ~source =
  let open Std.IO in
  
  (* Convert source URI to ID *)
  let source_id = Key.uri_to_id source in
  
  (* Build SOURCE prefix: just source_id (8 bytes) to get ALL facts from this source *)
  let prefix = Bytes.create 8 in
  Bytes.set_int64_be prefix 0 source_id;
  

  
  (* Scan SOURCE index with source prefix *)
  let source_results = Engine.scan_prefix store.source_engine ~prefix in
  
  (* Build deduplication map by streaming through results *)
  let fact_map = HashMap.create () in
  
  Iter.MutIterator.for_each source_results ~fn:(fun (source_key, _value) ->
    (* Decode SOURCE key to get fact_id and tx_id *)
    let decoded = Key.decode_source source_key in
    
    (* Build FACT key to get full fact *)
    let fact_key = Key.encode_fact {
      fact_id = decoded.fact_id;
      tx_id = decoded.tx_id;
    } in
    
    (* Lookup in FACT index *)
    match Engine.get store.fact_engine ~key:fact_key with
    | None -> ()  (* Skip if fact not found *)
    | Some fact_value ->
        (* Reconstruct UUID from tx_id int64 *)
        let tx_id_bytes = Bytes.make 16 '\x00' in
        Bytes.set_int64_be tx_id_bytes 0 decoded.tx_id;
        let tx_id = match UUID.of_bytes tx_id_bytes with
          | Ok uuid -> uuid
          | Error _ -> UUID.v4 ()
        in
        
        (* Decode fact *)
        let fact = decode_fact_value store tx_id fact_value in
        
        (* Create deduplication key from entity + attribute + value *)
        let entity_str = Uri.to_string fact.Fact.entity in
        let attr_str = Uri.to_string fact.Fact.attribute in
        let value_str = match fact.Fact.value with
          | Fact.Uri u -> "uri:" ^ Uri.to_string u
          | Fact.Int i -> "int:" ^ string_of_int i
          | Fact.Bool b -> "bool:" ^ string_of_bool b
          | Fact.String s -> "str:" ^ s
          | Fact.Float f -> "float:" ^ string_of_float f
          | Fact.DateTime dt -> "dt:" ^ Datetime.to_iso8601 dt
        in
        let map_key = entity_str ^ "|" ^ attr_str ^ "|" ^ value_str in
        
        (* Check if we already have this fact *)
        match HashMap.get fact_map map_key with
        | None -> 
            let _ = HashMap.insert fact_map map_key fact in
            ()
        | Some existing_fact ->
            (* Keep the fact with the higher tx_id (more recent) *)
            let existing_tx_id_bytes = UUID.to_bytes existing_fact.Fact.tx_id in
            let existing_tx_id_int64 = Bytes.get_int64_be existing_tx_id_bytes 0 in
            
            let new_tx_id_bytes = UUID.to_bytes fact.Fact.tx_id in
            let new_tx_id_int64 = Bytes.get_int64_be new_tx_id_bytes 0 in
            
            if new_tx_id_int64 > existing_tx_id_int64 then
              let _ = HashMap.insert fact_map map_key fact in
              ()
  );
  
  (* Extract non-retracted facts *)
  let latest_facts = vec [] in
  HashMap.iter (fun _key fact ->
    if not fact.Fact.retracted then
      Vector.push latest_facts fact
  ) fact_map;
  
  Vector.to_mut_iter latest_facts

(** Get detailed statistics about the database *)
let get_stats store =
  (* Helper to get stats for one index *)
  let index_stats engine =
    (* Get manifest from engine *)
    let manifest = Engine.get_manifest engine in
    let all_sstables = Manifest.get_sstables manifest ~index:"engine" in
    
    (* Group by tier - support up to tier 10 *)
    let sstables_by_tier = Array.make 11 [] in
    List.iter (fun sst ->
      let tier = sst.Manifest.tier in
      if tier >= 0 && tier <= 10 then
        sstables_by_tier.(tier) <- sst :: sstables_by_tier.(tier)
    ) all_sstables;
    
    (* Compute stats per tier *)
    let tier_stats = Array.mapi (fun tier sstables ->
      let count = List.length sstables in
      let total_size = List.fold_left (fun acc sst ->
        acc + sst.Manifest.size_bytes
      ) 0 sstables in
      let total_entries = List.fold_left (fun acc sst ->
        acc + sst.Manifest.entry_count
      ) 0 sstables in
      
      Data.Json.obj [
        ("tier", Data.Json.int tier);
        ("file_count", Data.Json.int count);
        ("total_size_bytes", Data.Json.int total_size);
        ("total_entries", Data.Json.int total_entries);
      ]
    ) sstables_by_tier in
    
    (* Total across all tiers *)
    let total_files = Array.fold_left (fun acc tier_sstables ->
      acc + List.length tier_sstables
    ) 0 sstables_by_tier in
    
    let total_size = Array.fold_left (fun acc tier_sstables ->
      acc + List.fold_left (fun sum sst -> sum + sst.Manifest.size_bytes) 0 tier_sstables
    ) 0 sstables_by_tier in
    
    let total_entries = Array.fold_left (fun acc tier_sstables ->
      acc + List.fold_left (fun sum sst -> sum + sst.Manifest.entry_count) 0 tier_sstables
    ) 0 sstables_by_tier in
    
    Data.Json.obj [
      ("total_files", Data.Json.int total_files);
      ("total_size_bytes", Data.Json.int total_size);
      ("total_entries", Data.Json.int total_entries);
      ("tiers", Data.Json.array (Array.to_list tier_stats));
    ]
  in
  
  Data.Json.obj [
    ("location", Data.Json.string store.data_dir);
    ("indices", Data.Json.obj [
      ("eavt", index_stats store.eavt_engine);
      ("avet", index_stats store.avet_engine);
      ("fact", index_stats store.fact_engine);
      ("source", index_stats store.source_engine);
    ]);
  ]

(** Cleanup orphaned SST files not tracked in manifest *)
let cleanup_orphaned_files store =
  let open Std.IO in
  

  
  let index_names = [("eavt", store.eavt_engine); ("avet", store.avet_engine); 
                     ("fact", store.fact_engine); ("source", store.source_engine)] in
  
  let total_deleted = ref 0 in
  
  List.iter (fun (name, engine) ->
    let index_dir = store.data_dir ^ "/" ^ name in
    
    (* Get tracked files from manifest *)
    let manifest = Engine.get_manifest engine in
    let tracked_files = Manifest.get_sstables manifest ~index:"engine" in
    let tracked_filenames = HashMap.create () in
    List.iter (fun sst ->
      (* Extract just the filename from path *)
      let path_parts = String.split_on_char '/' sst.Manifest.path in
      let filename = List.fold_left (fun _ part -> part) "" path_parts in
      let _ = HashMap.insert tracked_filenames filename () in
      ()
    ) tracked_files;
    
    (* List all .sst files in directory *)
    match Fs.read_dir (Path.v index_dir) with
    | Error e ->
        Log.warn ("Failed to read directory " ^ index_dir ^ ": " ^ IO.error_message e)
    | Ok entries ->
        let sst_files = Iter.MutIterator.to_list entries
          |> List.map Path.to_string
          |> List.filter (fun path ->
              String.ends_with ~suffix:".sst" path)
          |> List.map (fun path ->
              let parts = String.split_on_char '/' path in
              List.fold_left (fun _ part -> part) "" parts) in
        
        Log.debug ("  " ^ name ^ ": found " ^ string_of_int (List.length sst_files) ^ " SST files, " ^
                  string_of_int (List.length tracked_files) ^ " tracked in manifest");
        
        (* Delete orphaned files *)
        List.iter (fun filename ->
          match HashMap.get tracked_filenames filename with
          | Some _ -> ()  (* File is tracked, keep it *)
          | None ->
              (* File not in manifest, delete it *)
              let filepath = index_dir ^ "/" ^ filename in
              match Fs.remove_file (Path.v filepath) with
              | Ok () ->
                  Log.debug ("    Deleted orphaned file: " ^ filename);
                  total_deleted := !total_deleted + 1
              | Error e ->
                  Log.warn ("    Failed to delete " ^ filename ^ ": " ^ IO.error_message e)
        ) sst_files
  ) index_names;
  
  Log.debug ("Deleted " ^ string_of_int !total_deleted ^ " orphaned SST files")
