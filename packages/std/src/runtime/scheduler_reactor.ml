open Kernel
open Collections
open Sync
open Scheduler_types

let loop = fun ~(has_pending_commands:t -> bool) ~(drain_commands:t -> reactor_command list) ~(handle_command:t ->
reactor_command ->
unit) ~(process_timers:t -> unit) ~(poll_io:t -> unit) (runtime: t) ->
  Thread.DLS.set
    current_context
    (Some { scheduler = runtime; worker_id = None; current_process = None });
  while (not (Atomic.get runtime.stop)) || has_pending_commands runtime do
    List.for_each (drain_commands runtime) ~fn:(handle_command runtime);
    process_timers runtime;
    if not (Atomic.get runtime.stop) then
      poll_io runtime
  done
