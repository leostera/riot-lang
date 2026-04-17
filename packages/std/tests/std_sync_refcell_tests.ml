open Std

let is_failure = fun exn ~message ->
  match exn with
  | Failure reason -> String.equal reason message
  | _ -> false

let test_refcell_starts_available =
  Test.case "sync refcell starts available" (fun _ctx ->
    let cell = Sync.RefCell.create 42 in
    if not (Sync.RefCell.is_borrowed cell) && Sync.RefCell.borrow_count cell = 0 then
      Ok ()
    else
      Error "expected a fresh RefCell to start unborrowed with count 0")

let test_refcell_shared_borrows_increment_count =
  Test.case "sync refcell allows shared borrows and counts them" (fun _ctx ->
    let cell = Sync.RefCell.create "value" in
    let left = Sync.RefCell.borrow cell in
    let right = Sync.RefCell.borrow cell in
    let ok =
      Sync.RefCell.is_borrowed cell
      && Sync.RefCell.borrow_count cell = 2
    in
    Sync.RefCell.release_borrow left;
    Sync.RefCell.release_borrow right;
    if ok then
      Ok ()
    else
      Error "expected two shared borrows to set borrow_count to 2")

let test_refcell_borrow_mut_rejects_shared_borrows =
  Test.case "sync refcell borrow_mut rejects shared borrows" (fun _ctx ->
    let cell = Sync.RefCell.create 1 in
    let borrow = Sync.RefCell.borrow cell in
    let outcome =
      try
        let _ = Sync.RefCell.borrow_mut cell in
        Error "expected borrow_mut to raise while shared borrowed"
      with
      | Sync.RefCell.BorrowMutError _ -> Ok ()
    in
    let try_outcome =
      match Sync.RefCell.try_borrow_mut cell with
      | Error _ -> Ok ()
      | Ok _ -> Error "expected try_borrow_mut to fail while shared borrowed"
    in
    Sync.RefCell.release_borrow borrow;
    match outcome with
    | Error _ as err -> err
    | Ok () -> try_outcome)

let test_refcell_borrow_rejects_mutable_borrow =
  Test.case "sync refcell borrow rejects a mutable borrow" (fun _ctx ->
    let cell = Sync.RefCell.create 1 in
    let borrow = Sync.RefCell.borrow_mut cell in
    let outcome =
      try
        let _ = Sync.RefCell.borrow cell in
        Error "expected borrow to raise while mutably borrowed"
      with
      | Sync.RefCell.BorrowError _ -> Ok ()
    in
    let try_outcome =
      match Sync.RefCell.try_borrow cell with
      | Error _ -> Ok ()
      | Ok _ -> Error "expected try_borrow to fail while mutably borrowed"
    in
    Sync.RefCell.release_borrow_mut borrow;
    match outcome with
    | Error _ as err -> err
    | Ok () -> try_outcome)

let test_refcell_borrow_mut_allows_read_and_write =
  Test.case "sync refcell borrow_mut allows get_mut and set_mut" (fun _ctx ->
    let cell = Sync.RefCell.create 10 in
    let borrow = Sync.RefCell.borrow_mut cell in
    let initial = Sync.RefCell.get_mut borrow in
    Sync.RefCell.set_mut borrow 99;
    let updated = Sync.RefCell.get_mut borrow in
    Sync.RefCell.release_borrow_mut borrow;
    if initial = 10 && updated = 99 && Sync.RefCell.get_unchecked cell = 99 then
      Ok ()
    else
      Error "expected borrow_mut to allow reading and updating the stored value")

let test_refcell_releases_restore_availability =
  Test.case "sync refcell release helpers restore availability" (fun _ctx ->
    let cell = Sync.RefCell.create "value" in
    let shared = Sync.RefCell.borrow cell in
    Sync.RefCell.release_borrow shared;
    if Sync.RefCell.is_borrowed cell || Sync.RefCell.borrow_count cell != 0 then
      Error "expected release_borrow to restore availability"
    else (
      let mutable_borrow = Sync.RefCell.borrow_mut cell in
      Sync.RefCell.release_borrow_mut mutable_borrow;
      if Sync.RefCell.is_borrowed cell || Sync.RefCell.borrow_count cell != 0 then
        Error "expected release_borrow_mut to restore availability"
      else
        Ok ()
    ))

let test_refcell_with_borrow_auto_releases =
  Test.case "sync refcell with_borrow acquires and releases automatically" (fun _ctx ->
    let cell = Sync.RefCell.create 21 in
    let observed = Sync.RefCell.with_borrow cell (fun value -> value) in
    if observed = 21 && not (Sync.RefCell.is_borrowed cell) && Sync.RefCell.borrow_count cell = 0 then
      Ok ()
    else
      Error "expected with_borrow to release its shared borrow automatically")

let test_refcell_with_borrow_mut_auto_releases =
  Test.case "sync refcell with_borrow_mut acquires and releases automatically" (fun _ctx ->
    let cell = Sync.RefCell.create 5 in
    let observed =
      Sync.RefCell.with_borrow_mut cell (fun get set ->
        let before = get () in
        set 8;
        before)
    in
    if
      observed = 5
      && Sync.RefCell.get_unchecked cell = 8
      && not (Sync.RefCell.is_borrowed cell)
      && Sync.RefCell.borrow_count cell = 0
    then
      Ok ()
    else
      Error "expected with_borrow_mut to release its mutable borrow automatically")

let test_refcell_with_borrow_releases_on_exception =
  Test.case "sync refcell with_borrow releases on exception" (fun _ctx ->
    let cell = Sync.RefCell.create 7 in
    let outcome =
      try
        let _ =
          Sync.RefCell.with_borrow cell (fun _ ->
            raise (Failure "boom"))
        in
        Error "expected with_borrow callback to raise"
      with
      | exn when is_failure exn ~message:"boom" -> Ok ()
      | Failure reason -> Error ("unexpected failure: " ^ reason)
    in
    match outcome with
    | Error _ as err -> err
    | Ok () ->
        if not (Sync.RefCell.is_borrowed cell) && Sync.RefCell.borrow_count cell = 0 then
          Ok ()
        else
          Error "expected with_borrow to release after an exception")

let test_refcell_with_borrow_mut_releases_on_exception =
  Test.case "sync refcell with_borrow_mut releases on exception" (fun _ctx ->
    let cell = Sync.RefCell.create 7 in
    let outcome =
      try
        let _ =
          Sync.RefCell.with_borrow_mut cell (fun _get _set ->
            raise (Failure "boom"))
        in
        Error "expected with_borrow_mut callback to raise"
      with
      | exn when is_failure exn ~message:"boom" -> Ok ()
      | Failure reason -> Error ("unexpected failure: " ^ reason)
    in
    match outcome with
    | Error _ as err -> err
    | Ok () ->
        if not (Sync.RefCell.is_borrowed cell) && Sync.RefCell.borrow_count cell = 0 then
          Ok ()
        else
          Error "expected with_borrow_mut to release after an exception")

let name = "Sync.RefCell"

let tests = [
  test_refcell_starts_available;
  test_refcell_shared_borrows_increment_count;
  test_refcell_borrow_mut_rejects_shared_borrows;
  test_refcell_borrow_rejects_mutable_borrow;
  test_refcell_borrow_mut_allows_read_and_write;
  test_refcell_releases_restore_availability;
  test_refcell_with_borrow_auto_releases;
  test_refcell_with_borrow_mut_auto_releases;
  test_refcell_with_borrow_releases_on_exception;
  test_refcell_with_borrow_mut_releases_on_exception;
]

let () =
  Runtime.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
