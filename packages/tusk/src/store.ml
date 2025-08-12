(** Store - Content-addressable storage for build artifacts **)

type t = {
  root_dir : string; (* Root directory for the store *)
}

(** Create a new store at the given root directory *)
let create ~root_dir = 
  let store_dir = Filename.concat (Filename.concat root_dir "target/debug") "cache" in
  (* Create store directory if it doesn't exist *)
  System.mkdirp store_dir;
  { root_dir = store_dir }

(** Get the path for a given hash in the store *)
let get_hash_dir store hash =
  Filename.concat store.root_dir (Hasher.to_string hash)

(** Check if artifacts for a given hash exist in the store *)
let exists store hash =
  let hash_dir = get_hash_dir store hash in
  System.file_exists hash_dir

(** List all files in a hash directory *)
let list_artifacts store hash =
  let hash_dir = get_hash_dir store hash in
  if System.file_exists hash_dir then
    try
      Array.to_list (Sys.readdir hash_dir)
    with _ -> []
  else []

(** Promote artifacts from store to target directory *)
let promote_from_store store hash target_dir =
  let hash_dir = get_hash_dir store hash in
  if System.file_exists hash_dir then (
    Printf.printf "[Store] Promoting artifacts from cache: %s\n" (Hasher.to_string hash);
    flush stdout;
    
    (* Ensure target directory exists *)
    System.mkdirp target_dir;
    
    (* Copy all files from hash directory to target *)
    let files = Array.to_list (Sys.readdir hash_dir) in
    List.iter (fun file ->
      let src = Filename.concat hash_dir file in
      let dst = Filename.concat target_dir file in
      
      (* Only copy if it's a file, not directory *)
      if not (Sys.is_directory src) then (
        System.copy_file src dst;
        Printf.printf "[Store]   -> Promoted %s\n" file;
        flush stdout
      )
    ) files;
    true
  ) else false

(** Store artifacts from sandbox to content-addressable store *)
let store_artifacts store hash sandbox_dir declared_outputs =
  let hash_dir = get_hash_dir store hash in
  
  (* Create hash directory (including parent directories) *)
  System.mkdirp hash_dir;
  
  Printf.printf "[Store] Storing artifacts with hash: %s\n" (Hasher.to_string hash);
  flush stdout;
  
  (* Copy declared outputs to store *)
  List.iter (fun output_file ->
    let src = Filename.concat sandbox_dir output_file in
    if System.file_exists src then (
      let dst = Filename.concat hash_dir output_file in
      System.copy_file src dst;
      Printf.printf "[Store]   -> Stored %s\n" output_file;
      flush stdout
    ) else (
      Printf.printf "[Store]   -> Warning: Output %s not found in sandbox\n" output_file;
      flush stdout
    )
  ) declared_outputs

(** Clean up old artifacts (for future use) *)
let gc_store store ~keep_recent_days =
  Printf.printf "[Store] TODO: Implement garbage collection (keep recent %d days)\n" keep_recent_days;
  flush stdout

(** Get store statistics *)
let get_stats store =
  let count_files dir =
    if System.file_exists dir then
      try
        let subdirs = Array.to_list (Sys.readdir dir) in
        List.fold_left (fun acc subdir ->
          let subdir_path = Filename.concat dir subdir in
          if Sys.is_directory subdir_path then
            acc + Array.length (Sys.readdir subdir_path)
          else acc
        ) 0 subdirs
      with _ -> 0
    else 0
  in
  
  let total_artifacts = count_files store.root_dir in
  Printf.printf "[Store] Store statistics: %d cached artifacts\n" total_artifacts;
  flush stdout;
  total_artifacts