(** Manifest - SSTable metadata tracking *)

open Std
open Std.Collections

type sstable_metadata = {
  path : string;
  tier : int;
  size_bytes : int;
  min_key : bytes;
  max_key : bytes;
  entry_count : int;
  created_at : int64;
}

type index_manifest = {
  sstables : sstable_metadata list;
  next_sstable_id : int;  (* RocksDB-style: next ID to allocate *)
}

type t = {
  version : int;
  indices : (string, index_manifest) HashMap.t;
}

(** Tier boundaries in bytes *)
let tier_boundaries = [
  1_000_000;      (* 1 MB *)
  8_000_000;      (* 8 MB *)
  64_000_000;     (* 64 MB *)
  512_000_000;    (* 512 MB *)
]

let tier_for_size size =
  let rec find_tier tier boundaries =
    match boundaries with
    | [] -> tier
    | threshold :: rest ->
        if size < threshold then tier
        else find_tier (tier + 1) rest
  in
  find_tier 0 tier_boundaries

let empty () = {
  version = 1;
  indices = HashMap.create ();
}

(** {1 JSON Serialization} *)

let sstable_metadata_to_json meta =
  Data.Json.obj [
    ("path", Data.Json.string meta.path);
    ("tier", Data.Json.int meta.tier);
    ("size_bytes", Data.Json.int meta.size_bytes);
    ("min_key", Data.Json.string (Data.Base16.encode_bytes meta.min_key));
    ("max_key", Data.Json.string (Data.Base16.encode_bytes meta.max_key));
    ("entry_count", Data.Json.int meta.entry_count);
    ("created_at", Data.Json.string (Int64.to_string meta.created_at));
  ]

let sstable_metadata_of_json json =
  try
    let get_str field = 
      match Data.Json.get_field field json with
      | Some v -> (match Data.Json.get_string v with
                   | Some s -> s
                   | None -> panic ("Field not a string: " ^ field))
      | None -> panic ("Missing field: " ^ field)
    in
    let get_int field =
      match Data.Json.get_field field json with
      | Some v -> (match Data.Json.get_int v with
                   | Some i -> i
                   | None -> panic ("Field not an int: " ^ field))
      | None -> panic ("Missing field: " ^ field)
    in
    
    let path = get_str "path" in
    let tier = get_int "tier" in
    let size_bytes = get_int "size_bytes" in
    let min_key_hex = get_str "min_key" in
    let max_key_hex = get_str "max_key" in
    let entry_count = get_int "entry_count" in
    let created_at = get_str "created_at" |> Int64.of_string in
    
    let min_key = match Data.Base16.decode_bytes min_key_hex with
      | Ok b -> b
      | Error _ -> panic "Invalid hex in min_key"
    in
    let max_key = match Data.Base16.decode_bytes max_key_hex with
      | Ok b -> b
      | Error _ -> panic "Invalid hex in max_key"
    in
    
    Ok {
      path;
      tier;
      size_bytes;
      min_key;
      max_key;
      entry_count;
      created_at;
    }
  with _ -> Error "Failed to parse SSTable metadata"

let index_manifest_to_json manifest =
  Data.Json.obj [
    ("sstables", Data.Json.array (List.map sstable_metadata_to_json manifest.sstables));
    ("next_sstable_id", Data.Json.int manifest.next_sstable_id);
  ]

let index_manifest_of_json json =
  try
    let sstables_json = match Data.Json.get_field "sstables" json with
      | Some v -> (match Data.Json.get_array v with
                   | Some arr -> arr
                   | None -> panic "sstables field is not an array")
      | None -> panic "Missing sstables field"
    in
    let sstables_results = List.map sstable_metadata_of_json sstables_json in
    
    (* Read next_sstable_id - default to 0 for backward compatibility *)
    let next_sstable_id = match Data.Json.get_field "next_sstable_id" json with
      | Some v -> (match Data.Json.get_int v with
                   | Some i -> i
                   | None -> 0)
      | None -> 0  (* Old manifest without this field *)
    in
    
    (* Check for errors *)
    let rec collect_ok acc results =
      match results with
      | [] -> Ok (List.rev acc)
      | Ok meta :: rest -> collect_ok (meta :: acc) rest
      | Error e :: _ -> Error e
    in
    
    match collect_ok [] sstables_results with
    | Error e -> Error e
    | Ok sstables -> Ok { sstables; next_sstable_id }
  with _ -> Error "Failed to parse index manifest"

let to_json manifest =
  (* Convert HashMap to list of key-value pairs *)
  let indices_list = vec [] in
  HashMap.iter (fun key value ->
    Vector.push indices_list (key, index_manifest_to_json value)
  ) manifest.indices;
  
  (* Convert vector to list using iterator *)
  let open Iter in
  let indices_pairs = Vector.to_mut_iter indices_list 
    |> MutIterator.to_list in
  let indices_json = Data.Json.obj indices_pairs in
  
  Data.Json.obj [
    ("version", Data.Json.int manifest.version);
    ("indices", indices_json);
  ]

let of_json json =
  try
    let version = match Data.Json.get_field "version" json with
      | Some v -> (match Data.Json.get_int v with
                   | Some i -> i
                   | None -> panic "version field is not an int")
      | None -> panic "Missing version field"
    in
    let indices_json = match Data.Json.get_field "indices" json with
      | Some v -> v
      | None -> panic "Missing indices field"
    in
    
    let indices = HashMap.create () in
    
    (* Parse each index *)
    (match indices_json with
     | Data.Json.Object fields ->
         List.iter (fun (name, index_json) ->
           match index_manifest_of_json index_json with
           | Ok index_manifest ->
               let _ = HashMap.insert indices name index_manifest in
               ()
           | Error _ -> ()  (* Skip invalid indices *)
         ) fields
     | _ -> ());
    
    Ok { version; indices }
  with _ -> Error "Failed to parse manifest"

(** {1 File I/O} *)

let load ~path =
  match Fs.read_to_string (Path.v path) with
  | Error _ -> Ok (empty ())  (* File doesn't exist, return empty *)
  | Ok json_str ->
      match Data.Json.of_string json_str with
      | Error _ -> Ok (empty ())  (* Invalid JSON, return empty *)
      | Ok json -> of_json json

let save ~path manifest =
  let json = to_json manifest in
  let json_str = Data.Json.to_string json in
  
  (* Write to temp file first *)
  let temp_path = path ^ ".tmp" in
  (match Fs.write json_str (Path.v temp_path) with
   | Error e -> Error ("Failed to write manifest: " ^ IO.error_message e)
   | Ok () ->
       (* Atomic rename *)
       match Fs.rename ~src:(Path.v temp_path) ~dst:(Path.v path) with
       | Error e -> Error ("Failed to rename manifest: " ^ IO.error_message e)
       | Ok () -> Ok ())

(** {1 SSTable Management} *)

let get_sstables manifest ~index =
  match HashMap.get manifest.indices index with
  | None -> []
  | Some index_manifest -> index_manifest.sstables

let add_sstable manifest ~index sstable_meta =
  let current_manifest = match HashMap.get manifest.indices index with
    | None -> { sstables = []; next_sstable_id = 0 }
    | Some m -> m
  in
  
  let new_sstables = sstable_meta :: current_manifest.sstables in
  let new_index_manifest = { 
    sstables = new_sstables;
    next_sstable_id = current_manifest.next_sstable_id;  (* Preserve counter *)
  } in
  
  (* Update the existing indices (mutably) *)
  let _ = HashMap.insert manifest.indices index new_index_manifest in
  
  manifest

let remove_sstables manifest ~index ~paths =
  let current_manifest = match HashMap.get manifest.indices index with
    | None -> { sstables = []; next_sstable_id = 0 }
    | Some m -> m
  in
  
  let new_sstables = List.filter (fun meta ->
    not (List.mem meta.path paths)
  ) current_manifest.sstables in
  
  let new_index_manifest = { 
    sstables = new_sstables;
    next_sstable_id = current_manifest.next_sstable_id;  (* Preserve counter *)
  } in
  
  (* Update the existing indices (mutably) *)
  let _ = HashMap.insert manifest.indices index new_index_manifest in
  
  manifest

(** {1 Next SSTable ID Management} *)

let get_next_sstable_id manifest ~index =
  match HashMap.get manifest.indices index with
  | None -> 0
  | Some m -> m.next_sstable_id

let update_next_sstable_id manifest ~index new_id =
  let current_manifest = match HashMap.get manifest.indices index with
    | None -> { sstables = []; next_sstable_id = 0 }
    | Some m -> m
  in
  
  let new_index_manifest = {
    current_manifest with
    next_sstable_id = new_id;
  } in
  
  (* Update the existing indices (mutably) *)
  let _ = HashMap.insert manifest.indices index new_index_manifest in
  
  manifest

(** {1 Tier Management} *)

let group_by_tier sstables =
  let tier_map = HashMap.create () in
  
  List.iter (fun meta ->
    let current = match HashMap.get tier_map meta.tier with
      | None -> []
      | Some list -> list
    in
    let _ = HashMap.insert tier_map meta.tier (meta :: current) in
    ()
  ) sstables;
  
  (* Convert to list *)
  let result = vec [] in
  HashMap.iter (fun tier metas ->
    Vector.push result (tier, metas)
  ) tier_map;
  
  let open Iter in
  Vector.to_mut_iter result
  |> MutIterator.to_list
  |> List.sort (fun (t1, _) (t2, _) -> Int.compare t1 t2)
