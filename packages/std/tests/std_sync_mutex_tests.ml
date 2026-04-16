open Std

type Message.t +=
  | Mutex_worker_done of int
  | Mutex_overlap_detected of int
  | Mutex_holder_locked
  | Mutex_holder_release
  | Mutex_holder_released
  | Mutex_unlock_failed of string
  | Mutex_unlock_returned
  | Mutex_owner_ready
  | Mutex_owner_exit

let await = fun ~what selector ->
  try Ok (receive ~selector ~timeout:(Time.Duration.from_secs 1) ())
  with
  | Receive_timeout -> Error ("timed out waiting for " ^ what)

let test_mutex_serializes_contended_access =
  Test.case "sync mutex serializes contended access" (fun _ctx ->
    let lock = Sync.Mutex.create () in
    let parent = self () in
    let active_holders = Sync.Atomic.make 0 in
    let completed = Sync.Atomic.make 0 in
    let worker_count = 4 in
    for idx = 1 to worker_count do
      ignore
        (spawn
           (fun () ->
             Sync.Mutex.lock lock;
             let previous = Sync.Atomic.fetch_and_add active_holders 1 in
             if not (Int.equal previous 0) then
               send parent (Mutex_overlap_detected idx);
             sleep (Time.Duration.from_millis 10);
             let _ = Sync.Atomic.fetch_and_add active_holders (-1) in
             let _ = Sync.Atomic.fetch_and_add completed 1 in
             Sync.Mutex.unlock lock;
             send parent (Mutex_worker_done idx);
             Ok ()))
    done;
    let rec collect remaining =
      if remaining = 0 then
        Ok ()
      else
        match await
          ~what:"mutex workers"
          (function
          | Mutex_worker_done idx -> `select (`Done idx)
          | Mutex_overlap_detected idx -> `select (`Overlap idx)
          | _ -> `skip)
        with
        | Error _ as err -> err
        | Ok (`Done _) -> collect (remaining - 1)
        | Ok (`Overlap idx) ->
            Error ("mutex allowed overlapping access for worker " ^ Int.to_string idx)
    in
    match collect worker_count with
    | Error _ as err -> err
    | Ok () ->
        if
          Sync.Atomic.get completed = worker_count
          && Sync.Atomic.get active_holders = 0
        then
          Ok ()
        else
          Error "mutex workers did not complete cleanly")

let test_mutex_try_lock_reports_held_and_free_states =
  Test.case "sync mutex try_lock reports held and free states" (fun _ctx ->
    let lock = Sync.Mutex.create () in
    let parent = self () in
    let holder =
      spawn
        (fun () ->
          Sync.Mutex.lock lock;
          send parent Mutex_holder_locked;
          receive
            ~selector:(
              function
              | Mutex_holder_release -> `select ()
              | _ -> `skip
            )
            ();
          Sync.Mutex.unlock lock;
          send parent Mutex_holder_released;
          Ok ())
    in
    match await
      ~what:"mutex holder lock"
      (function
      | Mutex_holder_locked -> `select ()
      | _ -> `skip)
    with
    | Error _ as err -> err
    | Ok () ->
        if Sync.Mutex.try_lock lock then
          Error "expected try_lock to report the held mutex as unavailable"
        else (
          send holder Mutex_holder_release;
          match await
            ~what:"mutex holder release"
            (function
            | Mutex_holder_released -> `select ()
            | _ -> `skip)
          with
          | Error _ as err -> err
          | Ok () ->
              if not (Sync.Mutex.try_lock lock) then
                Error "expected try_lock to succeed after the mutex was released"
              else (
                Sync.Mutex.unlock lock;
                Ok ()
              )
        ))

let test_mutex_unlock_requires_ownership =
  Test.case "sync mutex unlock requires ownership" (fun _ctx ->
    let lock = Sync.Mutex.create () in
    let parent = self () in
    let holder =
      spawn
        (fun () ->
          Sync.Mutex.lock lock;
          send parent Mutex_holder_locked;
          receive
            ~selector:(
              function
              | Mutex_holder_release -> `select ()
              | _ -> `skip
            )
            ();
          Sync.Mutex.unlock lock;
          send parent Mutex_holder_released;
          Ok ())
    in
    match await
      ~what:"mutex holder lock"
      (function
      | Mutex_holder_locked -> `select ()
      | _ -> `skip)
    with
    | Error _ as err -> err
    | Ok () ->
        ignore
          (spawn
             (fun () ->
               let result =
                 try
                   Sync.Mutex.unlock lock;
                   `Returned
                 with
                 | Failure reason -> `Failed reason
               in
               (match result with
               | `Returned -> send parent Mutex_unlock_returned
               | `Failed reason -> send parent (Mutex_unlock_failed reason));
               Ok ()));
        let result =
          await
            ~what:"non-owner mutex unlock result"
            (function
            | Mutex_unlock_failed reason -> `select (`Failed reason)
            | Mutex_unlock_returned -> `select `Returned
            | _ -> `skip)
        in
        send holder Mutex_holder_release;
        ignore
          (await
             ~what:"mutex holder cleanup"
             (function
             | Mutex_holder_released -> `select ()
             | _ -> `skip));
        match result with
        | Error _ as err -> err
        | Ok (`Failed reason) when String.contains reason "non-owner" -> Ok ()
        | Ok (`Failed reason) ->
            Error ("unexpected mutex unlock failure: " ^ reason)
        | Ok `Returned ->
            Error "expected non-owner mutex unlock to fail")

let test_mutex_owner_exit_releases_lock =
  Test.case "sync mutex releases ownership when the owner exits" (fun _ctx ->
    let lock = Sync.Mutex.create () in
    let parent = self () in
    let owner =
      spawn
        (fun () ->
          Sync.Mutex.lock lock;
          send parent Mutex_owner_ready;
          receive
            ~selector:(
              function
              | Mutex_owner_exit -> `select ()
              | _ -> `skip
            )
            ();
          Ok ())
    in
    let _monitor = Runtime.Actor.monitor owner in
    match await
      ~what:"mutex owner ready"
      (function
      | Mutex_owner_ready -> `select ()
      | _ -> `skip)
    with
    | Error _ as err -> err
    | Ok () ->
        send owner Mutex_owner_exit;
        (match await
           ~what:"mutex owner exit"
           (function
           | Runtime.Actor.DOWN { pid; _ } when Runtime.Pid.equal pid owner -> `select ()
           | _ -> `skip)
         with
        | Error _ as err -> err
        | Ok () ->
            if not (Sync.Mutex.try_lock lock) then
              Error "expected owner exit to release the mutex"
            else (
              Sync.Mutex.unlock lock;
              Ok ()
            )))

let name = "Sync.Mutex"

let tests = [
  test_mutex_serializes_contended_access;
  test_mutex_try_lock_reports_held_and_free_states;
  test_mutex_unlock_requires_ownership;
  test_mutex_owner_exit_releases_lock;
]

let () = Runtime.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
