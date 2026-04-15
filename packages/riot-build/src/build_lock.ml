open Std

type t = {
  path: Path.t;
  file: Fs.File.t;
}

let reentrant_counts = Collections.HashMap.create ()

let reentrant_counts_lock = Sync.Mutex.create ()

let retry_interval = Time.Duration.from_millis 500

let path = fun ~target_dir_root ~profile ~target ->
  Path.(target_dir_root / Path.v profile / Path.v (Riot_model.Target.to_string target) / Path.v "riot.lock")

let path_key = fun path -> Path.to_string path

let increment_reentrant = fun path ->
  let key = path_key path in
  Sync.Mutex.lock reentrant_counts_lock;
  let count =
    match Collections.HashMap.get reentrant_counts ~key with
    | Some count ->
        let next = count + 1 in
        let _ = Collections.HashMap.insert reentrant_counts ~key ~value:next in
        next
    | None ->
        let _ = Collections.HashMap.insert reentrant_counts ~key ~value:1 in
        1
  in
  Sync.Mutex.unlock reentrant_counts_lock;
  count

let decrement_reentrant = fun path ->
  let key = path_key path in
  Sync.Mutex.lock reentrant_counts_lock;
  let remaining =
    match Collections.HashMap.get reentrant_counts ~key with
    | Some count when count > 1 ->
        let next = count - 1 in
        let _ = Collections.HashMap.insert reentrant_counts ~key ~value:next in
        next
    | Some _ ->
        let _ = Collections.HashMap.remove reentrant_counts ~key in
        0
    | None ->
        0
  in
  Sync.Mutex.unlock reentrant_counts_lock;
  remaining

let is_reentrant = fun path ->
  let key = path_key path in
  Sync.Mutex.lock reentrant_counts_lock;
  let held = Collections.HashMap.has_key reentrant_counts ~key in
  Sync.Mutex.unlock reentrant_counts_lock;
  held

let release = fun t ->
  let _ = Fs.File.unlock t.file in
  let _ = Fs.File.close t.file in
  ()

let lock_failure = fun action path ->
  Failure (format
    Format.[ str "Failed to "; str action; str " build lock file at "; str (Path.to_string path) ])

let rec retry = fun ?(announced = false) t ->
  if not announced then
    eprintln "build lock is taken, waiting...";
  sleep retry_interval;
  match Fs.File.try_lock_exclusive t.file with
  | Ok true ->
      Ok t
  | Ok false ->
      retry ~announced:true t
  | Error _ ->
      release t;
      raise (lock_failure "lock" t.path)

let wait = fun ~target_dir_root ~profile ~target ->
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
      retry t
  | Error _ ->
      release t;
      raise (lock_failure "lock" path)

let acquire = fun ~target_dir_root ~profile ~target fn ->
  let lock_path = path ~target_dir_root ~profile ~target in
  if is_reentrant lock_path then
    (
      let _ = increment_reentrant lock_path in
      try
        let result = fn () in
        let _ = decrement_reentrant lock_path in
        result
      with
      | exn ->
          let _ = decrement_reentrant lock_path in
          raise exn
    )
  else
    match wait ~target_dir_root ~profile ~target with
    | Error err -> Error err
    | Ok t ->
        let _ = increment_reentrant lock_path in
        try
          let result = fn () in
          let _ = decrement_reentrant lock_path in
          release t;
          result
        with
        | exn ->
            let _ = decrement_reentrant lock_path in
            release t;
            raise exn
