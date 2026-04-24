open Std

type t = {
  path: Path.t;
  file: Fs.File.t;
}

type lane = {
  profile: string;
  target: Riot_model.Target.t;
}

let in_process_lock_counts = Collections.HashMap.create ()

let in_process_lock_counts_lock = Sync.LazyCell.create Actor_mutex.create

let with_in_process_lock_counts_lock = fun f ->
  let lock = Sync.LazyCell.force in_process_lock_counts_lock in
  Actor_mutex.lock lock;
  try
    let result = f () in
    Actor_mutex.unlock lock;
    result
  with
  | exn ->
      Actor_mutex.unlock lock;
      raise exn

let retry_interval = Time.Duration.from_millis 500

let path = fun ~target_dir_root ~profile ~target ->
  Path.(target_dir_root / Path.v profile / Path.v (Riot_model.Target.to_string target) / Path.v "riot.lock")

let path_exists = fun path -> Fs.exists path |> Result.unwrap_or ~default:false

let path_is_directory = fun path ->
  Fs.metadata path |> Result.map ~fn:Fs.Metadata.is_dir |> Result.unwrap_or ~default:false

let list_children = fun dir ->
  if not (path_exists dir) then
    []
  else
    match Fs.read_dir dir with
    | Error _ -> []
    | Ok reader -> Iter.MutIterator.to_list reader |> List.map ~fn:(Path.join dir)

let list_subdirectories = fun dir -> list_children dir |> List.filter ~fn:path_is_directory

let compare_lane = fun left right ->
  match String.compare left.profile right.profile with
  | Order.EQ -> Riot_model.Target.compare left.target right.target
  | order -> order

let existing_lanes = fun ~target_dir_root ->
  list_subdirectories target_dir_root
  |> List.filter ~fn:(fun dir -> not (String.equal (Path.basename dir) "cache"))
  |> List.flat_map
    ~fn:(fun profile_dir ->
      let profile = Path.basename profile_dir in
      list_subdirectories profile_dir |> List.filter_map
        ~fn:(fun target_dir ->
          let lock_path = Path.(target_dir / Path.v "riot.lock") in
          if not (path_exists lock_path) then
            None
          else
            Riot_model.Target.from_string (Path.basename target_dir)
            |> Result.map ~fn:(fun target -> { profile; target })
            |> Result.to_option))
  |> List.sort ~compare:compare_lane

let path_key = fun path -> Path.to_string path

let increment_in_process_lock_count = fun path ->
  let key = path_key path in
  with_in_process_lock_counts_lock
    (fun () ->
      match Collections.HashMap.get in_process_lock_counts ~key with
      | Some count ->
          let next = count + 1 in
          let _ = Collections.HashMap.insert in_process_lock_counts ~key ~value:next in
          next
      | None ->
          let _ = Collections.HashMap.insert in_process_lock_counts ~key ~value:1 in
          1)

let decrement_in_process_lock_count = fun path ->
  let key = path_key path in
  with_in_process_lock_counts_lock
    (fun () ->
      match Collections.HashMap.get in_process_lock_counts ~key with
      | Some count when count > 1 ->
          let next = count - 1 in
          let _ = Collections.HashMap.insert in_process_lock_counts ~key ~value:next in
          next
      | Some _ ->
          let _ = Collections.HashMap.remove in_process_lock_counts ~key in
          0
      | None ->
          0)

let has_in_process_lock = fun path ->
  let key = path_key path in
  with_in_process_lock_counts_lock
    (fun () -> Collections.HashMap.has_key in_process_lock_counts ~key)

let release = fun t ->
  let _ = Fs.File.unlock t.file in
  let _ = Fs.File.close t.file in
  ()

let lock_failure = fun action path ->
  Failure (format
    Format.[ str "Failed to "; str action; str " build lock file at "; str (Path.to_string path) ])

let rec retry = fun ~on_waiting ?(announced = false) t ->
  if not announced then
    on_waiting t.path;
  sleep retry_interval;
  match Fs.File.try_lock_exclusive t.file with
  | Ok true ->
      Ok t
  | Ok false ->
      retry ~on_waiting ~announced:true t
  | Error _ ->
      release t;
      raise (lock_failure "lock" t.path)

let wait = fun ~on_waiting ~target_dir_root ~profile ~target ->
  let build_dir =
    Path.(target_dir_root / Path.v profile / Path.v (Riot_model.Target.to_string target)) in
  let _ = Fs.create_dir_all build_dir |> Result.expect ~msg:"Failed to create build directory" in
  let path = path ~target_dir_root ~profile ~target in
  let file =
    match Fs.File.open_write path with
    | Ok file -> file
    | Error _ -> raise (lock_failure "open" path)
  in
  let t = { path; file } in
  match Fs.File.try_lock_exclusive file with
  | Ok true ->
      Ok t
  | Ok false ->
      retry ~on_waiting t
  | Error _ ->
      release t;
      raise (lock_failure "lock" path)

let acquire = fun ~on_waiting ~target_dir_root ~profile ~target fn ->
  let lock_path = path ~target_dir_root ~profile ~target in
  if has_in_process_lock lock_path then
    (
      let _ = increment_in_process_lock_count lock_path in
      try
        let result = fn () in
        let _ = decrement_in_process_lock_count lock_path in
        result
      with
      | exn ->
          let _ = decrement_in_process_lock_count lock_path in
          raise exn
    )
  else
    match wait ~on_waiting ~target_dir_root ~profile ~target with
    | Error err -> Error err
    | Ok t ->
        let _ = increment_in_process_lock_count lock_path in
        try
          let result = fn () in
          let _ = decrement_in_process_lock_count lock_path in
          release t;
          result
        with
        | exn ->
            let _ = decrement_in_process_lock_count lock_path in
            release t;
            raise exn

let acquire_existing_lanes = fun ~on_waiting ~target_dir_root fn ->
  let rec loop = function
    | [] -> fn ()
    | lane :: rest -> acquire
      ~on_waiting
      ~target_dir_root
      ~profile:lane.profile
      ~target:lane.target
      (fun () -> loop rest)
  in
  loop (existing_lanes ~target_dir_root)
