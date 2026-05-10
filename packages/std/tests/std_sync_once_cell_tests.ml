open Std

let test_once_cell_starts_empty =
  Test.case
    "sync once_cell starts empty"
    (fun _ctx ->
      let cell = Sync.OnceCell.create () in
      if
        not (Sync.OnceCell.is_initialized cell)
        && Option.is_none (Sync.OnceCell.get cell)
        && Option.is_none (Sync.OnceCell.take cell)
      then
        Ok ()
      else
        Error "expected a fresh OnceCell to start empty")

let test_once_cell_set_initializes_once =
  Test.case
    "sync once_cell set initializes only once"
    (fun _ctx ->
      let cell = Sync.OnceCell.create () in
      match Sync.OnceCell.set cell "value" with
      | Error Sync.OnceCell.AlreadyInitialized -> Error "expected the first set to succeed"
      | Ok () ->
          match Sync.OnceCell.set cell "other" with
          | Ok () -> Error "expected the second set to fail"
          | Error Sync.OnceCell.AlreadyInitialized ->
              if Sync.OnceCell.is_initialized cell && Sync.OnceCell.get cell = Some "value" then
                Ok ()
              else
                Error "expected OnceCell to preserve the first value")

let test_once_cell_take_clears_storage =
  Test.case
    "sync once_cell take returns the value and clears storage"
    (fun _ctx ->
      let cell = Sync.OnceCell.create () in
      match Sync.OnceCell.set cell 9 with
      | Error Sync.OnceCell.AlreadyInitialized -> Error "expected the initial set to succeed"
      | Ok () ->
          let taken = Sync.OnceCell.take cell in
          if
            taken = Some 9
            && not (Sync.OnceCell.is_initialized cell)
            && Option.is_none (Sync.OnceCell.get cell)
          then
            Ok ()
          else
            Error "expected take to clear the OnceCell")

let test_once_cell_get_or_init_runs_once =
  Test.case
    "sync once_cell get_or_init runs the initializer once"
    (fun _ctx ->
      let cell = Sync.OnceCell.create () in
      let calls = Sync.Atomic.make 0 in
      let init () =
        let _ = Sync.Atomic.fetch_and_add calls 1 in
        "value"
      in
      let first = Sync.OnceCell.get_or_init cell init in
      let second = Sync.OnceCell.get_or_init cell init in
      if
        String.equal first "value" && String.equal second "value" && Sync.Atomic.get calls = 1
      then
        Ok ()
      else
        Error "expected get_or_init to run the initializer once")

let test_once_cell_get_or_try_init_retries_after_error =
  Test.case
    "sync once_cell get_or_try_init retries after an initialization error"
    (fun _ctx ->
      let cell = Sync.OnceCell.create () in
      let calls = Sync.Atomic.make 0 in
      let failing () =
        let _ = Sync.Atomic.fetch_and_add calls 1 in
        Error "boom"
      in
      let succeeding () =
        let _ = Sync.Atomic.fetch_and_add calls 1 in
        Ok "value"
      in
      match Sync.OnceCell.get_or_try_init cell failing with
      | Ok _ -> Error "expected the failing initializer to propagate its error"
      | Error reason when not (String.equal reason "boom") ->
          Error ("unexpected initializer error: " ^ reason)
      | Error _ ->
          match Sync.OnceCell.get_or_try_init cell succeeding with
          | Error reason -> Error ("expected the retry to succeed, got: " ^ reason)
          | Ok value ->
              if
                String.equal value "value"
                && Sync.OnceCell.get cell = Some "value"
                && Sync.Atomic.get calls = 2
              then
                Ok ()
              else
                Error "expected get_or_try_init to retry after a failed initialization")

let name = "Sync.OnceCell"

let tests = [
  test_once_cell_starts_empty;
  test_once_cell_set_initializes_once;
  test_once_cell_take_clears_storage;
  test_once_cell_get_or_init_runs_once;
  test_once_cell_get_or_try_init_retries_after_error;
]

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
