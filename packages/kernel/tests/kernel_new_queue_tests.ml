open Std

module Test = Std.Test
module Kernel = Kernel

let test_queue_pops_in_fifo_order = fun _ctx ->
  let queue = Kernel.Queue.create () in
  Kernel.Queue.push queue ~value:1;
  Kernel.Queue.push queue ~value:2;
  Kernel.Queue.push queue ~value:3;
  match (Kernel.Queue.pop queue, Kernel.Queue.pop queue, Kernel.Queue.pop queue) with
  | (Some 1, Some 2, Some 3) -> Ok ()
  | _ -> Error "expected Kernel.Queue.pop to preserve FIFO order"

let test_queue_reports_empty_after_last_pop = fun _ctx ->
  let queue = Kernel.Queue.from_list [ "a"; "b"; ] in
  let _ = Kernel.Queue.pop queue in
  let _ = Kernel.Queue.pop queue in
  if Kernel.Queue.is_empty queue && Kernel.Queue.length queue = 0 then
    Ok ()
  else
    Error "expected Kernel.Queue to be empty after popping every value"

let test_queue_snapshot_helpers_preserve_order = fun _ctx ->
  let queue = Kernel.Queue.from_list [ 1; 2; 3; ] in
  if Kernel.Queue.to_list queue = [ 1; 2; 3; ] then
    Ok ()
  else
    Error "expected Kernel.Queue.to_list to snapshot values in FIFO order"

let tests = [
  Test.case "Queue.pop preserves FIFO order" test_queue_pops_in_fifo_order;
  Test.case "Queue reports empty after last pop" test_queue_reports_empty_after_last_pop;
  Test.case "Queue snapshots preserve FIFO order" test_queue_snapshot_helpers_preserve_order;
]

let main ~args = Test.Cli.main ~name:"kernel_new_queue_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
