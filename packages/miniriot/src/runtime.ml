(* Runtime module for Riot - provides reduction counting for compiler
   instrumentation. *)

open Kernel

let reset_reductions remaining =
  let current = Scheduler.get_current_process () in
  Process.reset_reductions current remaining

let increment_reduction_count () =
  let current = Scheduler.get_current_process () in
  match Process.use_reduction current with
  | Process.Continue ->
      ()
  | Process.Yield ->
      Effects.yield ()
