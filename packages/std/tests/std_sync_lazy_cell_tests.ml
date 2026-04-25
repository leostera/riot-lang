open Std

let test_lazy_cell_starts_uninitialized =
  Test.case "sync lazy_cell starts uninitialized"
    (fun _ctx ->
      let cell =
        Sync.LazyCell.create (fun () -> 1)
      in
      if not (Sync.LazyCell.is_initialized cell) && Option.is_none (Sync.LazyCell.take cell) then
        Ok ()
      else
        Error "expected a fresh LazyCell to start uninitialized")

let test_lazy_cell_force_computes_once =
  Test.case "sync lazy_cell force computes once and caches the value"
    (fun _ctx ->
      let calls = Sync.Atomic.make 0 in
      let cell =
        Sync.LazyCell.create
          (fun () ->
            let _ = Sync.Atomic.fetch_and_add calls 1 in
            "value")
      in
      let first = Sync.LazyCell.force cell in
      let second = Sync.LazyCell.force cell in
      if
        String.equal first "value"
        && String.equal second "value"
        && Sync.LazyCell.is_initialized cell
        && Sync.Atomic.get calls = 1
      then
        Ok ()
      else
        Error "expected force to compute once and reuse the cached value")

let test_lazy_cell_get_is_force =
  Test.case "sync lazy_cell get is an alias for force"
    (fun _ctx ->
      let calls = Sync.Atomic.make 0 in
      let cell =
        Sync.LazyCell.create
          (fun () ->
            let _ = Sync.Atomic.fetch_and_add calls 1 in
            7)
      in
      let value = Sync.LazyCell.get cell in
      if value = 7 && Sync.LazyCell.is_initialized cell && Sync.Atomic.get calls = 1 then
        Ok ()
      else
        Error "expected get to force and cache the lazy value")

let test_lazy_cell_take_returns_cached_value_and_clears_storage =
  Test.case "sync lazy_cell take returns the cached value and clears storage"
    (fun _ctx ->
      let calls = Sync.Atomic.make 0 in
      let cell =
        Sync.LazyCell.create
          (fun () ->
            let _ = Sync.Atomic.fetch_and_add calls 1 in
            9)
      in
      let _ = Sync.LazyCell.force cell in
      let taken = Sync.LazyCell.take cell in
      if taken = Some 9 && not (Sync.LazyCell.is_initialized cell) && Sync.Atomic.get calls = 1 then
        Ok ()
      else
        Error "expected take to return the cached value and clear storage")

let test_lazy_cell_recomputes_after_take =
  Test.case "sync lazy_cell recomputes after take clears the cache"
    (fun _ctx ->
      let calls = Sync.Atomic.make 0 in
      let cell =
        Sync.LazyCell.create (fun () -> Int.succ (Sync.Atomic.fetch_and_add calls 1))
      in
      let first = Sync.LazyCell.force cell in
      let _ = Sync.LazyCell.take cell in
      let second = Sync.LazyCell.force cell in
      if first = 1 && second = 2 && Sync.Atomic.get calls = 2 then
        Ok ()
      else
        Error "expected force to recompute after take clears the cache")

let name = "Sync.LazyCell"

let tests = [
  test_lazy_cell_starts_uninitialized;
  test_lazy_cell_force_computes_once;
  test_lazy_cell_get_is_force;
  test_lazy_cell_take_returns_cached_value_and_clears_storage;
  test_lazy_cell_recomputes_after_take;
]

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
