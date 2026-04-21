open Std
module Test = Std.Test
module Kernel = Kernel

let test_quick_stat_is_non_negative = fun _ctx ->
  let stats = Kernel.Gc.quick_stat () in
  if
    Int.(stats.minor_collections >= 0)
    && Int.(stats.major_collections >= 0)
    && Int.(stats.compactions >= 0)
  then
    Ok ()
  else
    Error "expected Kernel.Gc.quick_stat counters to stay non-negative"

let test_full_major_keeps_counters_monotonic = fun _ctx ->
  let before = Kernel.Gc.quick_stat () in
  Kernel.Gc.full_major ();
  let after_ = Kernel.Gc.quick_stat () in
  if
    Int.(after_.minor_collections >= before.minor_collections)
    && Int.(after_.major_collections >= before.major_collections)
    && Int.(after_.compactions >= before.compactions)
  then
    Ok ()
  else
    Error "expected Kernel.Gc counters to stay monotonic across full_major"

let tests = [
  Test.case "Gc.quick_stat counters stay non-negative" test_quick_stat_is_non_negative;
  Test.case "Gc.full_major keeps counters monotonic" test_full_major_keeps_counters_monotonic;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_gc_tests" ~tests ~args ()

let () = Actors.run ~main ~args:Env.args ()
