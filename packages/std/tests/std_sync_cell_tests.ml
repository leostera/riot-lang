open Std

let test_cell_create_and_get =
  Test.case "sync cell create stores and returns the initial value"
    (fun _ctx ->
      let cell = Sync.Cell.create 42 in
      if Sync.Cell.get cell = 42 && !cell = 42 then
        Ok ()
      else
        Error "expected Cell.create to preserve the initial value")

let test_cell_set_and_assign =
  Test.case "sync cell set and := update the stored value"
    (fun _ctx ->
      let cell = Sync.Cell.create "left" in
      Sync.Cell.set cell "middle";
      cell := "right";
      if Sync.Cell.get cell = "right" then
        Ok ()
      else
        Error "expected set and := to update the stored value")

let test_cell_update_rewrites_value =
  Test.case "sync cell update rewrites from the previous value"
    (fun _ctx ->
      let cell = Sync.Cell.create 5 in
      Sync.Cell.update cell (fun value -> value * 3);
      if Sync.Cell.get cell = 15 then
        Ok ()
      else
        Error "expected update to replace the value with the callback result")

let test_cell_incr_and_decr =
  Test.case "sync cell incr and decr adjust integer cells"
    (fun _ctx ->
      let cell = Sync.Cell.create 10 in
      Sync.Cell.incr cell;
      Sync.Cell.decr cell;
      Sync.Cell.decr cell;
      if Sync.Cell.get cell = 9 then
        Ok ()
      else
        Error "expected incr/decr to adjust integer cells")

let test_cell_replace_returns_old_value =
  Test.case "sync cell replace returns the old value and stores the new one"
    (fun _ctx ->
      let cell = Sync.Cell.create "before" in
      let old_value = Sync.Cell.replace cell "after" in
      if String.equal old_value "before" && String.equal (Sync.Cell.get cell) "after" then
        Ok ()
      else
        Error "expected replace to return the old value and store the new one")

let test_cell_take_returns_old_value_and_sets_default =
  Test.case "sync cell take returns the old value and stores the default"
    (fun _ctx ->
      let cell = Sync.Cell.create 8 in
      let taken = Sync.Cell.take cell ~default:0 in
      if taken = 8 && Sync.Cell.get cell = 0 then
        Ok ()
      else
        Error "expected take to return the previous value and install the default")

let test_cell_swap_exchanges_values =
  Test.case "sync cell swap exchanges both cell values"
    (fun _ctx ->
      let left = Sync.Cell.create "left" in
      let right = Sync.Cell.create "right" in
      Sync.Cell.swap left right;
      if String.equal (Sync.Cell.get left) "right" && String.equal (Sync.Cell.get right) "left" then
        Ok ()
      else
        Error "expected swap to exchange both cell values")

let test_cell_compare_and_swap_updates_only_on_match =
  Test.case "sync cell compare_and_swap updates only when the expected value matches"
    (fun _ctx ->
      let cell = Sync.Cell.create 3 in
      let first = Sync.Cell.compare_and_swap cell 3 7 in
      let second = Sync.Cell.compare_and_swap cell 3 9 in
      if first && not second && Sync.Cell.get cell = 7 then
        Ok ()
      else
        Error "expected compare_and_swap to update only on a matching value")

let test_cell_equal_compares_stored_values =
  Test.case "sync cell equal compares the stored values"
    (fun _ctx ->
      let left = Sync.Cell.create [ 1; 2; 3 ] in
      let right = Sync.Cell.create [ 1; 2; 3 ] in
      let different = Sync.Cell.create [ 1; 2 ] in
      if Sync.Cell.equal left right && not (Sync.Cell.equal left different) then
        Ok ()
      else
        Error "expected equal to compare stored values")

let name = "Sync.Cell"

let tests = [
  test_cell_create_and_get;
  test_cell_set_and_assign;
  test_cell_update_rewrites_value;
  test_cell_incr_and_decr;
  test_cell_replace_returns_old_value;
  test_cell_take_returns_old_value_and_sets_default;
  test_cell_swap_exchanges_values;
  test_cell_compare_and_swap_updates_only_on_match;
  test_cell_equal_compares_stored_values;
]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name ~tests ~args ()) ~args:Env.args ()
