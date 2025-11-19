open Std
open Std.Collections
  open Iter

type event = {
  path : Path.t;
  timestamp : Time.Instant.t;
}

type t = {
  paths : Path.t list;
  mutable snapshots : (string * float) list;
}

let rec scan_files path =
  match Fs.exists path with
  | Error _ -> []
  | Ok false -> []
  | Ok true ->
      (* Try to read as directory *)
      match Fs.read_dir path with
      | Ok iter ->
          (* It's a directory *)
          let entries = MutIterator.to_list iter in
          List.concat_map (fun entry ->
            scan_files (Path.join path entry)
          ) entries
      | Error _ ->
          (* Not a directory, must be a file - check extension *)
          match Path.extension path with
          | Some ".ml" | Some ".mli" -> [path]
          | _ -> []

let create ~paths =
  let all_files = List.concat_map scan_files paths in
  let snapshots = List.filter_map (fun file ->
    match Fs.metadata file with
    | Ok metadata -> Some (Path.to_string file, Fs.Metadata.modified metadata)
    | Error _ -> None
  ) all_files in
  { paths; snapshots }

let next_event t ~timeout:_ =
  (* Scan for changes *)
  let all_files = List.concat_map scan_files t.paths in
  let changed = List.find_opt (fun file ->
    let path_str = Path.to_string file in
    match Fs.metadata file with
    | Ok metadata ->
        let mtime = Fs.Metadata.modified metadata in
        let old_mtime = List.assoc_opt path_str t.snapshots in
        (match old_mtime with
        | Some old when Float.abs (mtime -. old) > 0.001 ->
            (* Update snapshot *)
            t.snapshots <- (path_str, mtime) :: 
              (List.filter (fun (p, _) -> p != path_str) t.snapshots);
            true
        | None ->
            (* New file *)
            t.snapshots <- (path_str, mtime) :: t.snapshots;
            true
        | _ -> false)
    | Error _ -> false
  ) all_files in
  
  match changed with
  | Some path -> Some { path; timestamp = Time.Instant.now () }
  | None ->
      (* No changes, sleep a bit *)
      sleep (Time.Duration.from_secs 1);
      None

let close _t = ()
