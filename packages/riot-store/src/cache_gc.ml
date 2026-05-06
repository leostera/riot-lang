open Std
open Std.Collections
open Riot_model

type generation_lane = {
  profile: string;
  target: Riot_model.Target.t;
  hashes: string list;
}

type new_cache_entry = {
  profile: string;
  target: Riot_model.Target.t;
  hash: string;
  size_bytes: int64;
}

type summary = {
  ran_gc: bool;
  kept_generations: int;
  deleted_generations: int;
  deleted_entries: int;
  size_before_bytes: int64;
  size_after_bytes: int64;
}

type error = string

type trigger =
  | Manual
  | Post_build

type event =
  | GcStarted of {
      trigger: trigger;
    }
  | GcCacheScanStarted of {
      trigger: trigger;
      build_root: Path.t;
    }
  | GcCacheEntryScanStarted of {
      trigger: trigger;
      hash: string;
      path: Path.t;
    }
  | GcCacheEntryScanned of {
      trigger: trigger;
      hash: string;
      path: Path.t;
      size_bytes: int64;
    }
  | GcCacheScanCompleted of {
      trigger: trigger;
      entry_count: int;
      total_size_bytes: int64;
    }
  | GcPlanComputed of {
      trigger: trigger;
      deleted_entries: int;
      deleted_generations: int;
      reclaimable_bytes: int64;
    }
  | GcCacheEntryDeleteStarted of {
      trigger: trigger;
      hash: string;
      path: Path.t;
      size_bytes: int64;
    }
  | GcGenerationDeleteStarted of {
      trigger: trigger;
      path: Path.t;
    }
  | GcSkipped of {
      trigger: trigger;
      summary: summary;
    }
  | GcCompleted of {
      trigger: trigger;
      summary: summary;
    }
  | GcFailed of {
      trigger: trigger;
      error: string;
    }
  | ForceCleanStarted of {
      build_root: Path.t;
    }
  | ForceCleanCompleted of {
      build_root: Path.t;
    }
  | ForceCleanFailed of {
      build_root: Path.t;
      error: string;
    }

type cache_entry = {
  hash: string;
  dir: Path.t;
  size_bytes: int64;
}

type receipt = {
  hash: string;
  lanes: generation_lane list;
}

type receipt_file = {
  path: Path.t;
  receipt: receipt;
}

type cache_state = {
  tracked_size_bytes: int64;
  generation_hashes: string list option;
  receipt_count: int option;
}

type tracked_size_snapshot = {
  tracked_size_bytes: int64;
  generation_hashes: string list;
  cache_entries: cache_entry list option;
  rebuilt: bool;
}

let ( let* ) result fn = Result.and_then result ~fn

let no_event: event -> unit = fun _ -> ()

let cache_root = fun ~(workspace:Workspace.t) -> Path.(workspace.target_dir_root / Path.v "cache")

let generations_root = fun ~(workspace:Workspace.t) ->
  Path.(cache_root ~workspace / Path.v "generations")

let state_path = fun ~(workspace:Workspace.t) -> Path.(cache_root ~workspace / Path.v "state.json")

let receipt_filename = fun hash -> hash ^ ".json"

let receipt_path = fun ~(workspace:Workspace.t) hash ->
  Path.(generations_root ~workspace / Path.v (receipt_filename hash))

let temp_receipt_path = fun ~(workspace:Workspace.t) hash ->
  Path.(generations_root ~workspace / Path.v (receipt_filename hash ^ ".tmp"))

let scaled_size_string = fun bytes divisor suffix ->
  let whole = Int64.div bytes divisor in
  let remainder = Int64.rem bytes divisor in
  let fraction = Int64.div (Int64.mul remainder 10L) divisor in
  Int64.to_string whole ^ "." ^ Int64.to_string fraction ^ " " ^ suffix

let size_to_string = fun size_bytes ->
  let kib = 1_024L in
  let mib = Int64.mul kib 1_024L in
  let gib = Int64.mul mib 1_024L in
  let tib = Int64.mul gib 1_024L in
  if Int64.compare size_bytes tib != Order.LT then
    scaled_size_string size_bytes tib "TiB"
  else if Int64.compare size_bytes gib != Order.LT then
    scaled_size_string size_bytes gib "GiB"
  else if Int64.compare size_bytes mib != Order.LT then
    scaled_size_string size_bytes mib "MiB"
  else if Int64.compare size_bytes kib != Order.LT then
    scaled_size_string size_bytes kib "KiB"
  else
    Int64.to_string size_bytes ^ " B"

let summary_message = fun summary ->
  if not summary.ran_gc then
    "tracked cache is already within policy ("
    ^ size_to_string summary.size_after_bytes
    ^ "); build root kept"
  else
    "removed "
    ^ Int.to_string summary.deleted_entries
    ^ " cache entries and "
    ^ Int.to_string summary.deleted_generations
    ^ " generations from tracked cache ("
    ^ size_to_string summary.size_before_bytes
    ^ " -> "
    ^ size_to_string summary.size_after_bytes
    ^ ")"

let trigger_to_string = fun __tmp1 ->
  match __tmp1 with
  | Manual -> "manual"
  | Post_build -> "post_build"

let short_hash = fun hash ->
  if String.length hash > 12 then
    String.sub hash ~offset:0 ~len:12
  else
    hash

let summary_to_json = fun summary ->
  Data.Json.Object [
    ("ran_gc", Data.Json.Bool summary.ran_gc);
    ("kept_generations", Data.Json.Int summary.kept_generations);
    ("deleted_generations", Data.Json.Int summary.deleted_generations);
    ("deleted_entries", Data.Json.Int summary.deleted_entries);
    ("size_before_bytes", Data.Json.String (Int64.to_string summary.size_before_bytes));
    ("size_after_bytes", Data.Json.String (Int64.to_string summary.size_after_bytes));
  ]

let event_message = fun __tmp1 ->
  match __tmp1 with
  | GcStarted { trigger } -> "starting tracked cache GC (" ^ trigger_to_string trigger ^ ")"
  | GcCacheScanStarted { build_root; _ } ->
      "scanning tracked cache entries under " ^ Path.to_string build_root
  | GcCacheEntryScanStarted { hash; path; _ } ->
      "scanning cache entry " ^ short_hash hash ^ " at " ^ Path.to_string path
  | GcCacheEntryScanned { hash; size_bytes; _ } ->
      "scanned cache entry " ^ short_hash hash ^ " (" ^ size_to_string size_bytes ^ ")"
  | GcCacheScanCompleted { entry_count; total_size_bytes; _ } ->
      "scanned "
      ^ Int.to_string entry_count
      ^ " cache entries ("
      ^ size_to_string total_size_bytes
      ^ ")"
  | GcPlanComputed { deleted_entries; deleted_generations; reclaimable_bytes; _ } ->
      "will remove "
      ^ Int.to_string deleted_entries
      ^ " cache entries and "
      ^ Int.to_string deleted_generations
      ^ " generations, reclaiming "
      ^ size_to_string reclaimable_bytes
  | GcCacheEntryDeleteStarted { hash; size_bytes; _ } ->
      "removing cache entry " ^ short_hash hash ^ " (" ^ size_to_string size_bytes ^ ")"
  | GcGenerationDeleteStarted { path; _ } -> "removing generation receipt " ^ Path.to_string path
  | GcSkipped { summary; _ } -> summary_message summary
  | GcCompleted { summary; _ } -> summary_message summary
  | GcFailed { error; _ } -> error
  | ForceCleanStarted { build_root } -> "removing build root " ^ Path.to_string build_root
  | ForceCleanCompleted { build_root } -> "removed build root " ^ Path.to_string build_root
  | ForceCleanFailed { build_root; error } ->
      "failed to remove build root " ^ Path.to_string build_root ^ ": " ^ error

let event_to_json = fun __tmp1 ->
  match __tmp1 with
  | GcStarted { trigger } ->
      Data.Json.Object [
        ("type", Data.Json.String "CacheGcStarted");
        ("trigger", Data.Json.String (trigger_to_string trigger));
      ]
  | GcCacheScanStarted { trigger; build_root } ->
      Data.Json.Object [
        ("type", Data.Json.String "CacheGcScanStarted");
        ("trigger", Data.Json.String (trigger_to_string trigger));
        ("build_root", Data.Json.String (Path.to_string build_root));
      ]
  | GcCacheEntryScanStarted { trigger; hash; path } ->
      Data.Json.Object [
        ("type", Data.Json.String "CacheGcEntryScanStarted");
        ("trigger", Data.Json.String (trigger_to_string trigger));
        ("hash", Data.Json.String hash);
        ("path", Data.Json.String (Path.to_string path));
      ]
  | GcCacheEntryScanned {
      trigger;
      hash;
      path;
      size_bytes;
    } ->
      Data.Json.Object [
        ("type", Data.Json.String "CacheGcEntryScanned");
        ("trigger", Data.Json.String (trigger_to_string trigger));
        ("hash", Data.Json.String hash);
        ("path", Data.Json.String (Path.to_string path));
        ("size_bytes", Data.Json.String (Int64.to_string size_bytes));
      ]
  | GcCacheScanCompleted { trigger; entry_count; total_size_bytes } ->
      Data.Json.Object [
        ("type", Data.Json.String "CacheGcScanCompleted");
        ("trigger", Data.Json.String (trigger_to_string trigger));
        ("entry_count", Data.Json.Int entry_count);
        ("total_size_bytes", Data.Json.String (Int64.to_string total_size_bytes));
      ]
  | GcPlanComputed {
      trigger;
      deleted_entries;
      deleted_generations;
      reclaimable_bytes;
    } ->
      Data.Json.Object [
        ("type", Data.Json.String "CacheGcPlanComputed");
        ("trigger", Data.Json.String (trigger_to_string trigger));
        ("deleted_entries", Data.Json.Int deleted_entries);
        ("deleted_generations", Data.Json.Int deleted_generations);
        ("reclaimable_bytes", Data.Json.String (Int64.to_string reclaimable_bytes));
      ]
  | GcCacheEntryDeleteStarted {
      trigger;
      hash;
      path;
      size_bytes;
    } ->
      Data.Json.Object [
        ("type", Data.Json.String "CacheGcEntryDeleteStarted");
        ("trigger", Data.Json.String (trigger_to_string trigger));
        ("hash", Data.Json.String hash);
        ("path", Data.Json.String (Path.to_string path));
        ("size_bytes", Data.Json.String (Int64.to_string size_bytes));
      ]
  | GcGenerationDeleteStarted { trigger; path } ->
      Data.Json.Object [
        ("type", Data.Json.String "CacheGcGenerationDeleteStarted");
        ("trigger", Data.Json.String (trigger_to_string trigger));
        ("path", Data.Json.String (Path.to_string path));
      ]
  | GcSkipped { trigger; summary } ->
      Data.Json.Object [
        ("type", Data.Json.String "CacheGcSkipped");
        ("trigger", Data.Json.String (trigger_to_string trigger));
        ("summary", summary_to_json summary);
      ]
  | GcCompleted { trigger; summary } ->
      Data.Json.Object [
        ("type", Data.Json.String "CacheGcCompleted");
        ("trigger", Data.Json.String (trigger_to_string trigger));
        ("summary", summary_to_json summary);
      ]
  | GcFailed { trigger; error } ->
      Data.Json.Object [
        ("type", Data.Json.String "CacheGcFailed");
        ("trigger", Data.Json.String (trigger_to_string trigger));
        ("error", Data.Json.String error);
      ]
  | ForceCleanStarted { build_root } ->
      Data.Json.Object [
        ("type", Data.Json.String "ForceCleanStarted");
        ("build_root", Data.Json.String (Path.to_string build_root));
      ]
  | ForceCleanCompleted { build_root } ->
      Data.Json.Object [
        ("type", Data.Json.String "ForceCleanCompleted");
        ("build_root", Data.Json.String (Path.to_string build_root));
      ]
  | ForceCleanFailed { build_root; error } ->
      Data.Json.Object [
        ("type", Data.Json.String "ForceCleanFailed");
        ("build_root", Data.Json.String (Path.to_string build_root));
        ("error", Data.Json.String error);
      ]

let sort_uniq_strings = fun values ->
  let rec dedupe acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> List.reverse acc
    | [ value ] -> List.reverse (value :: acc)
    | left :: ((right :: _) as rest) ->
        if String.equal left right then
          dedupe acc rest
        else
          dedupe (left :: acc) rest
  in
  values
  |> List.sort ~compare:String.compare
  |> dedupe []

let normalize_lane = fun (lane: generation_lane) -> {
  lane with
  hashes = sort_uniq_strings lane.hashes;
}

let normalize_lanes: generation_lane list -> generation_lane list = fun lanes ->
  lanes
  |> List.map ~fn:normalize_lane
  |> List.sort
    ~compare:(fun (left: generation_lane) (right: generation_lane) ->
      match String.compare left.profile right.profile with
      | Order.EQ ->
          String.compare
            (Riot_model.Target.to_string left.target)
            (Riot_model.Target.to_string right.target)
      | order -> order)

let lane_to_json = fun (lane: generation_lane) ->
  Data.Json.Object [
    ("profile", Data.Json.String lane.profile);
    ("target", Data.Json.String (Riot_model.Target.to_string lane.target));
    ("hashes", Data.Json.Array (List.map lane.hashes ~fn:Data.Json.string));
  ]

let generation_hash_of_lanes = fun lanes ->
  let module H = Crypto.Sha256 in
  let state = H.create () in
  H.write state "riot-cache-generation:v2";
  H.write_int state (List.length lanes);
  List.for_each
    lanes
    ~fn:(fun (lane: generation_lane) ->
      H.write state lane.profile;
      H.write state (Riot_model.Target.to_string lane.target);
      H.write_int state (List.length lane.hashes);
      List.for_each lane.hashes ~fn:(H.write state));
  H.finish state
  |> Crypto.Digest.hex

let receipt_to_json = fun receipt ->
  Data.Json.Object [
    ("schema_version", Data.Json.Int 2);
    ("hash", Data.Json.String receipt.hash);
    ("lanes", Data.Json.Array (List.map receipt.lanes ~fn:lane_to_json));
  ]

let cache_state_to_json = fun (state: cache_state) ->
  Data.Json.Object (
    [
      ("schema_version", Data.Json.Int 2);
      ("tracked_size_bytes", Data.Json.String (Int64.to_string state.tracked_size_bytes));
    ] @ match state.generation_hashes with
    | Some generation_hashes ->
        [
          ("generation_hashes", Data.Json.Array (List.map generation_hashes ~fn:Data.Json.string));
        ]
    | None ->
        [] @ match state.receipt_count with
        | Some receipt_count -> [ ("receipt_count", Data.Json.Int receipt_count); ]
        | None -> []
  )

let generation_lane_of_json = fun json ->
  let* profile =
    match Data.Json.get_field "profile" json with
    | Some value -> (
        match Data.Json.get_string value with
        | Some profile -> Ok profile
        | None -> Error "generation lane is missing string field 'profile'"
      )
    | None -> Error "generation lane is missing string field 'profile'"
  in
  let* target =
    match Data.Json.get_field "target" json with
    | Some value -> (
        match Data.Json.get_string value with
        | Some target ->
            Riot_model.Target.from_string target
            |> Result.map_err ~fn:Riot_model.Target.error_message
        | None -> Error "generation lane is missing string field 'target'"
      )
    | None -> Error "generation lane is missing string field 'target'"
  in
  let* hashes =
    match Data.Json.get_field "hashes" json with
    | Some (Data.Json.Array hashes) ->
        let rec loop acc = fun __tmp1 ->
          match __tmp1 with
          | [] -> Ok (List.reverse acc)
          | value :: rest -> (
              match Data.Json.get_string value with
              | Some hash -> loop (hash :: acc) rest
              | None -> Error "generation lane field 'hashes' must contain only strings"
            )
        in
        loop [] hashes
    | _ -> Error "generation lane is missing array field 'hashes'"
  in
  Ok (normalize_lane { profile; target; hashes })

let receipt_of_json = fun json ->
  let* lanes =
    match Data.Json.get_field "lanes" json with
    | Some (Data.Json.Array lanes) ->
        let rec loop acc = fun __tmp1 ->
          match __tmp1 with
          | [] -> Ok (List.reverse acc)
          | lane :: rest -> (
              match generation_lane_of_json lane with
              | Ok lane -> loop (lane :: acc) rest
              | Error _ as err -> err
            )
        in
        loop [] lanes
    | _ -> Error "generation receipt is missing array field 'lanes'"
  in
  let hash =
    match Data.Json.get_field "hash" json with
    | Some value -> (
        match Data.Json.get_string value with
        | Some hash -> hash
        | None -> generation_hash_of_lanes lanes
      )
    | None -> generation_hash_of_lanes lanes
  in
  Ok { hash; lanes }

let cache_state_of_json = fun json ->
  match Data.Json.get_field "tracked_size_bytes" json with
  | Some value -> (
      match Data.Json.get_string value with
      | Some tracked_size_bytes -> (
          match Int64.parse tracked_size_bytes with
          | Some tracked_size_bytes ->
              let generation_hashes =
                match Data.Json.get_field "generation_hashes" json with
                | Some (Data.Json.Array hashes) ->
                    let rec loop acc = fun __tmp1 ->
                      match __tmp1 with
                      | [] -> Some (List.reverse acc)
                      | value :: rest -> (
                          match Data.Json.get_string value with
                          | Some hash -> loop (hash :: acc) rest
                          | None -> None
                        )
                    in
                    loop [] hashes
                | _ -> None
              in
              let receipt_count =
                match Data.Json.get_field "receipt_count" json with
                | Some value -> Data.Json.get_int value
                | None -> None
              in
              Ok ({ tracked_size_bytes; generation_hashes; receipt_count }: cache_state)
          | None -> Error "cache state field 'tracked_size_bytes' must be an int64 string"
        )
      | None -> Error "cache state is missing string field 'tracked_size_bytes'"
    )
  | None -> Error "cache state is missing string field 'tracked_size_bytes'"

let path_exists = fun path ->
  Fs.exists path
  |> Result.unwrap_or ~default:false

let path_is_directory = fun path ->
  Fs.metadata path
  |> Result.map ~fn:Fs.Metadata.is_dir
  |> Result.unwrap_or ~default:false

let list_children = fun dir ->
  if not (path_exists dir) then
    []
  else
    match Fs.read_dir dir with
    | Error _ -> []
    | Ok reader ->
        Std.Iter.MutIterator.to_list reader
        |> List.map ~fn:(Path.join dir)

let list_subdirectories = fun dir ->
  list_children dir
  |> List.filter ~fn:path_is_directory

let is_json_file = fun path ->
  String.ends_with ~suffix:".json" (Path.basename path) && not (path_is_directory path)

let is_hex_char = fun __tmp1 ->
  match __tmp1 with
  | '0' .. '9'
  | 'a' .. 'f'
  | 'A' .. 'F' -> true
  | _ -> false

let is_hash_dir_name = fun name ->
  let len = String.length name in
  let rec loop idx =
    if idx = len then
      true
    else if is_hex_char (String.get_unchecked name ~at:idx) then
      loop (idx + 1)
    else
      false
  in
  len = 64 && loop 0

let receipt_paths_desc = fun ~(workspace:Workspace.t) ->
  list_children (generations_root ~workspace)
  |> List.filter ~fn:is_json_file
  |> List.sort
    ~compare:(fun left right -> String.compare (Path.basename right) (Path.basename left))

let count_receipts = fun ~(workspace:Workspace.t) -> List.length (receipt_paths_desc ~workspace)

let ensure_cache_root = fun ~(workspace:Workspace.t) ->
  Fs.create_dir_all (cache_root ~workspace)
  |> Result.map_err
    ~fn:(fun err -> "failed to create workspace cache directory: " ^ IO.error_message err)

let ensure_generations_root = fun ~(workspace:Workspace.t) ->
  Fs.create_dir_all (generations_root ~workspace)
  |> Result.map_err
    ~fn:(fun err -> "failed to create generation receipt directory: " ^ IO.error_message err)

let write_state = fun ~(workspace:Workspace.t) (state: cache_state) ->
  let* () = ensure_cache_root ~workspace in
  Fs.write (Data.Json.to_string_pretty (cache_state_to_json state)) (state_path ~workspace)
  |> Result.map_err ~fn:(fun err -> "failed to write cache state: " ^ IO.error_message err)

let read_state = fun ~(workspace:Workspace.t) ->
  let path = state_path ~workspace in
  if not (path_exists path) then
    Ok None
  else
    let* content =
      Fs.read_to_string path
      |> Result.map_err ~fn:(fun err -> "failed to read cache state: " ^ IO.error_message err)
    in
    let* json =
      Data.Json.from_string content
      |> Result.map_err
        ~fn:(fun err -> "failed to parse cache state JSON: " ^ Data.Json.error_to_string err)
    in
    let* state = cache_state_of_json json in
    Ok (Some state)

let write_receipt = fun ~(workspace:Workspace.t) receipt ->
  let* () = ensure_generations_root ~workspace in
  let final_path = receipt_path ~workspace receipt.hash in
  if path_exists final_path then
    Ok ()
  else
    let temp_path = temp_receipt_path ~workspace receipt.hash in
    let* () =
      Fs.write (Data.Json.to_string_pretty (receipt_to_json receipt)) temp_path
      |> Result.map_err
        ~fn:(fun err -> "failed to write generation receipt: " ^ IO.error_message err)
    in
    match Fs.rename ~src:temp_path ~dst:final_path with
    | Ok () -> Ok ()
    | Error err ->
        let _ = Fs.remove_file temp_path in
        Error ("failed to commit generation receipt: " ^ IO.error_message err)

let read_receipt_file = fun path ->
  let* content =
    Fs.read_to_string path
    |> Result.map_err ~fn:(fun err -> "failed to read generation receipt: " ^ IO.error_message err)
  in
  let* json =
    Data.Json.from_string content
    |> Result.map_err
      ~fn:(fun err -> "failed to parse generation receipt JSON: " ^ Data.Json.error_to_string err)
  in
  let* receipt = receipt_of_json json in
  Ok { path; receipt }

let load_latest_receipt = fun ~(workspace:Workspace.t) ->
  match receipt_paths_desc ~workspace with
  | [] -> Ok None
  | path :: _ ->
      read_receipt_file path
      |> Result.map ~fn:Option.some

let load_receipts = fun ~(workspace:Workspace.t) ->
  let paths = receipt_paths_desc ~workspace in
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | path :: rest -> (
        match read_receipt_file path with
        | Ok receipt -> loop (receipt :: acc) rest
        | Error _ as err -> err
      )
  in
  loop [] paths

let write_canonical_receipts = fun ~(workspace:Workspace.t) receipts ->
  List.fold_left
    receipts
    ~init:(Ok ())
    ~fn:(fun acc_result (receipt_file: receipt_file) ->
      let* () = acc_result in
      write_receipt ~workspace receipt_file.receipt)

let generation_hashes_of_receipts = fun receipts ->
  let seen = HashSet.create () in
  List.fold_left
    receipts
    ~init:[]
    ~fn:(fun acc (receipt_file: receipt_file) ->
      if HashSet.contains seen ~value:receipt_file.receipt.hash then
        acc
      else
        (
          let _ = HashSet.insert seen ~value:receipt_file.receipt.hash in
          receipt_file.receipt.hash :: acc
        ))
  |> List.reverse

let preserve_generation_recency = fun ~preferred ~discovered ->
  let discovered_set =
    List.fold_left
      discovered
      ~init:(HashSet.create ())
      ~fn:(fun set hash ->
        let _ = HashSet.insert set ~value:hash in
        set)
  in
  let add_if_available = fun (seen, acc) hash ->
    if HashSet.contains discovered_set ~value:hash && not (HashSet.contains seen ~value:hash) then (
      let _ = HashSet.insert seen ~value:hash in
      (seen, hash :: acc)
    ) else
      (seen, acc)
  in
  let (seen, acc) = List.fold_left preferred ~init:(HashSet.create (), []) ~fn:add_if_available in
  let (_, acc) = List.fold_left discovered ~init:(seen, acc) ~fn:add_if_available in
  List.reverse acc

let rebuild_generation_hashes = fun ~(workspace:Workspace.t) ->
  let* receipts = load_receipts ~workspace in
  let* () = write_canonical_receipts ~workspace receipts in
  Ok (generation_hashes_of_receipts receipts)

let path_size_bytes = fun root ->
  if not (path_exists root) then
    Ok 0L
  else
    let total = ref 0L in
    Fs.Walker.walk
      ~sort:false
      ~roots:[ root ]
      ~f:(fun item ->
        match Fs.Walker.FileItem.kind item with
        | Fs.Walker.File ->
            let path = Fs.Walker.FileItem.path item in
            let len =
              Fs.metadata path
              |> Result.map ~fn:Fs.Metadata.len
              |> Result.unwrap_or ~default:0
            in
            total := Int64.add !total (Int64.from_int len);
            Fs.Walker.Continue
        | Fs.Walker.Directory
        | Fs.Walker.Symlink
        | Fs.Walker.Other -> Fs.Walker.Continue)
      ()
    |> Result.map_err ~fn:IO.error_message
    |> Result.map ~fn:(fun () -> !total)

let total_size = fun entries ->
  List.fold_left
    entries
    ~init:0L
    ~fn:(fun acc (entry: cache_entry) -> Int64.add acc entry.size_bytes)

let collect_cache_entries = fun ~trigger ~on_event ~(workspace:Workspace.t) ->
  let collect_hash_dirs = fun cache_dir ->
    Path.(cache_dir / Path.v "trees")
    |> list_subdirectories
    |> List.flat_map
      ~fn:(fun shard_dir ->
        list_subdirectories shard_dir
        |> List.filter ~fn:(fun dir -> is_hash_dir_name (Path.basename dir)))
  in
  let profile_dirs =
    list_subdirectories workspace.target_dir_root
    |> List.filter ~fn:(fun dir -> not (String.equal (Path.basename dir) "cache"))
  in
  let rec collect_profiles acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | profile_dir :: rest ->
        let target_dirs = list_subdirectories profile_dir in
        let* acc = collect_targets acc target_dirs in
        collect_profiles acc rest
  and collect_targets acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok acc
    | target_dir :: rest ->
        let cache_dir = Path.(target_dir / Path.v "cache") in
        let hash_dirs = collect_hash_dirs cache_dir in
        let* acc =
          List.fold_left
            hash_dirs
            ~init:(Ok acc)
            ~fn:(fun acc_result dir ->
              let* acc = acc_result in
              let hash = Path.basename dir in
              on_event (GcCacheEntryScanStarted { trigger; hash; path = dir });
              let* size_bytes = path_size_bytes dir in
              on_event
                (
                  GcCacheEntryScanned {
                    trigger;
                    hash;
                    path = dir;
                    size_bytes;
                  }
                );
              Ok ({ hash; dir; size_bytes } :: acc))
        in
        collect_targets acc rest
  in
  on_event (GcCacheScanStarted { trigger; build_root = workspace.target_dir_root });
  let* entries = collect_profiles [] profile_dirs in
  on_event
    (GcCacheScanCompleted {
      trigger;
      entry_count = List.length entries;
      total_size_bytes = total_size entries;
    });
  Ok entries

let rebuild_tracked_size = fun ~trigger ~on_event ~(workspace:Workspace.t) ->
  let* entries = collect_cache_entries ~trigger ~on_event ~workspace in
  let tracked_size_bytes = total_size entries in
  let preferred_generation_hashes =
    match read_state ~workspace with
    | Ok (Some { generation_hashes = Some generation_hashes; _ }) -> generation_hashes
    | Ok (Some { generation_hashes = None; _ })
    | Ok None
    | Error _ -> []
  in
  let* discovered_generation_hashes = rebuild_generation_hashes ~workspace in
  let generation_hashes =
    preserve_generation_recency
      ~preferred:preferred_generation_hashes
      ~discovered:discovered_generation_hashes
  in
  let* () =
    write_state
      ~workspace
      {
        tracked_size_bytes;
        generation_hashes = Some generation_hashes;
        receipt_count = Some (List.length generation_hashes);
      }
  in
  Ok ({
    tracked_size_bytes;
    generation_hashes;
    cache_entries = Some entries;
    rebuilt = true;
  }: tracked_size_snapshot)

let load_or_rebuild_tracked_size = fun
  ~trigger ~on_event ~force_rebuild ~(workspace:Workspace.t) ->
  if force_rebuild then
    rebuild_tracked_size ~trigger ~on_event ~workspace
  else
    match read_state ~workspace with
    | Ok (Some state) ->
        let* generation_hashes =
          match state.generation_hashes with
          | Some generation_hashes -> Ok generation_hashes
          | None ->
              let* generation_hashes = rebuild_generation_hashes ~workspace in
              let* () =
                write_state
                  ~workspace
                  {
                    tracked_size_bytes = state.tracked_size_bytes;
                    generation_hashes = Some generation_hashes;
                    receipt_count = Some (List.length generation_hashes);
                  }
              in
              Ok generation_hashes
        in
        Ok ({
          tracked_size_bytes = state.tracked_size_bytes;
          generation_hashes;
          cache_entries = None;
          rebuilt = false;
        }: tracked_size_snapshot)
    | Ok None
    | Error _ -> rebuild_tracked_size ~trigger ~on_event ~workspace

let take = fun n list ->
  let rec loop acc remaining count =
    if count <= 0 then
      List.reverse acc
    else
      match remaining with
      | [] -> List.reverse acc
      | x :: xs -> loop (x :: acc) xs (count - 1)
  in
  loop [] list n

let drop_last = fun __tmp1 ->
  match __tmp1 with
  | [] -> []
  | list -> (
      match List.reverse list with
      | [] -> []
      | _ :: rest -> List.reverse rest
    )

let live_hashes = fun receipts ->
  let set = HashSet.create () in
  List.for_each
    receipts
    ~fn:(fun (receipt_file: receipt_file) ->
      List.for_each
        receipt_file.receipt.lanes
        ~fn:(fun (lane: generation_lane) ->
          List.for_each
            lane.hashes
            ~fn:(fun hash ->
              let _ = HashSet.insert set ~value:hash in
              ())));
  set

let evaluate_retention = fun ~policy receipts entries ->
  let initial = take policy.Riot_model.Workspace_operational_config.keep_generations receipts in
  let rec loop kept =
    let live = live_hashes kept in
    let retained_entries =
      List.filter entries ~fn:(fun (entry: cache_entry) -> HashSet.contains live ~value:entry.hash)
    in
    let retained_size = total_size retained_entries in
    if Int64.compare retained_size policy.max_size_bytes != Order.GT || kept = [] then
      (kept, live, retained_size)
    else
      loop (drop_last kept)
  in
  loop initial

let delete_path = fun path ~kind ->
  if not (path_exists path) then
    Ok ()
  else if String.equal kind "directory" then
    Fs.remove_dir_all path
    |> Result.map_err
      ~fn:(fun err ->
        "failed to remove " ^ kind ^ " " ^ Path.to_string path ^ ": " ^ IO.error_message err)
  else
    Fs.remove_file path
    |> Result.map_err
      ~fn:(fun err ->
        "failed to remove " ^ kind ^ " " ^ Path.to_string path ^ ": " ^ IO.error_message err)

let load_receipts_for_hashes = fun ~(workspace:Workspace.t) hashes ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | hash :: rest -> (
        match read_receipt_file (receipt_path ~workspace hash) with
        | Ok receipt -> loop (receipt :: acc) rest
        | Error _ as err -> err
      )
  in
  loop [] hashes

let run_gc = fun
  ~(workspace:Workspace.t) ~trigger ~on_event ~policy ~generation_hashes ~cache_entries ->
  let candidate_hashes =
    take policy.Riot_model.Workspace_operational_config.keep_generations generation_hashes
  in
  let* receipts = load_receipts_for_hashes ~workspace candidate_hashes in
  let* entries =
    match cache_entries with
    | Some entries -> Ok entries
    | None -> collect_cache_entries ~trigger ~on_event ~workspace
  in
  let size_before_bytes = total_size entries in
  let (kept_receipts, live, size_after_bytes) = evaluate_retention ~policy receipts entries in
  let kept_hashes =
    List.map kept_receipts ~fn:(fun (receipt: receipt_file) -> receipt.receipt.hash)
  in
  let kept_paths = HashSet.create () in
  List.for_each
    kept_hashes
    ~fn:(fun hash ->
      let _ = HashSet.insert kept_paths ~value:(Path.to_string (receipt_path ~workspace hash)) in
      ());
  let deleted_entries =
    List.filter
      entries
      ~fn:(fun (entry: cache_entry) -> not (HashSet.contains live ~value:entry.hash))
  in
  let deleted_receipts =
    List.filter
      (receipt_paths_desc ~workspace)
      ~fn:(fun path -> not (HashSet.contains kept_paths ~value:(Path.to_string path)))
  in
  on_event
    (
      GcPlanComputed {
        trigger;
        deleted_entries = List.length deleted_entries;
        deleted_generations = List.length deleted_receipts;
        reclaimable_bytes = Int64.sub size_before_bytes size_after_bytes;
      }
    );
  let* () =
    List.fold_left
      deleted_entries
      ~init:(Ok ())
      ~fn:(fun acc (entry: cache_entry) ->
        let* () = acc in
        on_event
          (
            GcCacheEntryDeleteStarted {
              trigger;
              hash = entry.hash;
              path = entry.dir;
              size_bytes = entry.size_bytes;
            }
          );
        delete_path entry.dir ~kind:"directory")
  in
  let* () =
    List.fold_left
      deleted_receipts
      ~init:(Ok ())
      ~fn:(fun acc receipt_path ->
        let* () = acc in
        on_event (GcGenerationDeleteStarted { trigger; path = receipt_path });
        delete_path receipt_path ~kind:"file")
  in
  let* () =
    write_state
      ~workspace
      {
        tracked_size_bytes = size_after_bytes;
        generation_hashes = Some kept_hashes;
        receipt_count = Some (List.length kept_hashes);
      }
  in
  Ok {
    ran_gc = true;
    kept_generations = List.length kept_hashes;
    deleted_generations = List.length deleted_receipts;
    deleted_entries = List.length deleted_entries;
    size_before_bytes;
    size_after_bytes;
  }

let load_policy = fun ~(workspace:Workspace.t) ->
  Riot_model.Workspace_operational_config.load ~workspace_root:workspace.root
  |> Result.map ~fn:(fun config -> config.Riot_model.Workspace_operational_config.cache)
  |> Result.map_err ~fn:Riot_model.Workspace_operational_config.message

let should_run_gc = fun ~generation_count ~policy ~tracked_size_bytes ->
  generation_count > policy.Riot_model.Workspace_operational_config.keep_generations
  || Int64.compare tracked_size_bytes policy.max_size_bytes = Order.GT

let clean_with_events = fun ~(workspace:Workspace.t) ~on_event ->
  let trigger = Manual in
  let report_error error =
    on_event (GcFailed { trigger; error });
    Error error
  in
  on_event (GcStarted { trigger });
  let* policy =
    match load_policy ~workspace with
    | Ok policy -> Ok policy
    | Error error -> report_error error
  in
  let* tracked_size =
    match load_or_rebuild_tracked_size ~trigger ~on_event ~force_rebuild:true ~workspace with
    | Ok tracked_size -> Ok tracked_size
    | Error error -> report_error error
  in
  let tracked_size_bytes = tracked_size.tracked_size_bytes in
  let generation_count = List.length tracked_size.generation_hashes in
  if not (should_run_gc ~generation_count ~policy ~tracked_size_bytes) then
    let summary = {
      ran_gc = false;
      kept_generations = generation_count;
      deleted_generations = 0;
      deleted_entries = 0;
      size_before_bytes = tracked_size_bytes;
      size_after_bytes = tracked_size_bytes;
    }
    in
    on_event (GcSkipped { trigger; summary });
    Ok summary
  else
    (
      match run_gc
        ~workspace
        ~trigger
        ~on_event
        ~policy
        ~generation_hashes:tracked_size.generation_hashes
        ~cache_entries:tracked_size.cache_entries with
      | Ok summary ->
          on_event (GcCompleted { trigger; summary });
          Ok summary
      | Error error ->
          on_event (GcFailed { trigger; error });
          Error error
    )

let clean = fun ~(workspace:Workspace.t) -> clean_with_events ~workspace ~on_event:no_event

let force_clean_with_events = fun ~(workspace:Workspace.t) ~on_event ->
  let build_root = workspace.target_dir_root in
  on_event (ForceCleanStarted { build_root });
  if not (path_exists build_root) then (
    on_event (ForceCleanCompleted { build_root });
    Ok ()
  ) else
    match Fs.remove_dir_all build_root with
    | Ok () ->
        on_event (ForceCleanCompleted { build_root });
        Ok ()
    | Error err ->
        let error =
          "failed to remove build root " ^ Path.to_string build_root ^ ": " ^ IO.error_message err
        in
        on_event (ForceCleanFailed { build_root; error });
        Error error

let force_clean = fun ~(workspace:Workspace.t) ->
  force_clean_with_events
    ~workspace
    ~on_event:no_event

let new_entry_key = fun (entry: new_cache_entry) ->
  entry.profile ^ "\000" ^ Riot_model.Target.to_string entry.target ^ "\000" ^ entry.hash

let added_size_for_new_entries = fun new_entries ->
  let seen = HashSet.create () in
  List.fold_left
    new_entries
    ~init:0L
    ~fn:(fun acc (entry: new_cache_entry) ->
      let key = new_entry_key entry in
      if HashSet.contains seen ~value:key then
        acc
      else
        (
        let _ = HashSet.insert seen ~value:key in
        Int64.add acc entry.size_bytes
        ))

let record_successful_build_with_events = fun
  ~(workspace:Workspace.t) ~on_event ~lanes ~new_entries ->
  let lanes = normalize_lanes lanes in
  let generation_hash = generation_hash_of_lanes lanes in
  let receipt = { hash = generation_hash; lanes } in
  let report_error error =
    on_event (GcFailed { trigger = Post_build; error });
    Error error
  in
  let* tracked_size =
    match load_or_rebuild_tracked_size ~trigger:Post_build ~on_event ~force_rebuild:false ~workspace with
    | Ok tracked_size -> Ok tracked_size
    | Error error -> report_error error
  in
  let* added_size_bytes =
    if tracked_size.rebuilt then
      Ok 0L
    else
      Ok (added_size_for_new_entries new_entries)
  in
  let tracked_size_bytes = Int64.add tracked_size.tracked_size_bytes added_size_bytes in
  let current_hashes = tracked_size.generation_hashes in
  match current_hashes with
  | latest_hash :: _ when String.equal latest_hash generation_hash ->
      let* () =
        if Int64.equal tracked_size_bytes tracked_size.tracked_size_bytes then
          Ok ()
        else
          write_state
            ~workspace
            {
              tracked_size_bytes;
              generation_hashes = Some current_hashes;
              receipt_count = Some (List.length current_hashes);
            }
      in
      Ok {
        ran_gc = false;
        kept_generations = List.length current_hashes;
        deleted_generations = 0;
        deleted_entries = 0;
        size_before_bytes = tracked_size_bytes;
        size_after_bytes = tracked_size_bytes;
      }
  | _ ->
      let next_hashes =
        generation_hash
        :: List.filter current_hashes ~fn:(fun hash -> not (String.equal hash generation_hash))
      in
      let* () =
        match write_state
          ~workspace
          {
            tracked_size_bytes;
            generation_hashes = Some next_hashes;
            receipt_count = Some (List.length next_hashes);
          } with
        | Ok () -> Ok ()
        | Error error -> report_error error
      in
      let* () =
        match write_receipt ~workspace receipt with
        | Ok () -> Ok ()
        | Error error -> report_error error
      in
      Ok {
        ran_gc = false;
        kept_generations = List.length next_hashes;
        deleted_generations = 0;
        deleted_entries = 0;
        size_before_bytes = tracked_size_bytes;
        size_after_bytes = tracked_size_bytes;
      }

let record_successful_build = fun ~(workspace:Workspace.t) ~lanes ~new_entries ->
  record_successful_build_with_events
    ~workspace
    ~on_event:no_event
    ~lanes
    ~new_entries
