(* Runtime module for Riot - provides reduction counting for compiler
   instrumentation. *)

module Runtime_process = Process

let reset_reductions = fun remaining ->
  let current = Scheduler.get_current_process () in
  Runtime_process.reset_reductions current remaining

let increment_reduction_count = fun () ->
  let current = Scheduler.get_current_process () in
  match Runtime_process.use_reduction current with
  | Runtime_process.Continue -> ()
  | Runtime_process.Yield -> Effects.yield ()
