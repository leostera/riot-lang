open Collections
open Scheduler_types

type deps = {
  ensure_can_run_once: unit -> unit;
  create: config:Config.t -> t;
  spawn_on_worker: t -> worker_id:Scheduler_id.t -> (unit -> (unit, Process.exit_reason) Kernel.result) -> Pid.t;
  worker_loop: t -> worker -> unit;
  reactor_loop: t -> unit;
  join_blocking_lanes: t -> unit;
}

let run = fun deps ~config ~main ->
  deps.ensure_can_run_once ();
  let t = deps.create ~config in
  let _ = deps.spawn_on_worker t ~worker_id:Scheduler_id.zero main in
  let reactor_domain =
    Kernel.Thread.spawn
      (
        fun () -> deps.reactor_loop t
      )
  in
  let worker_domains = Array.init ~count:(Kernel.Int.sub (Array.length t.workers) 1) ~fn:(
    fun idx ->
      let worker = Array.get_unchecked t.workers ~at:(Kernel.Int.add idx 1) in
      Kernel.Thread.spawn
        (
          fun () -> deps.worker_loop t worker
        )
  ) in
  deps.worker_loop t (Array.get_unchecked t.workers ~at:0);
  Array.for_each worker_domains ~fn:Kernel.Thread.join;
  Kernel.Thread.join reactor_domain;
  deps.join_blocking_lanes t;
  Kernel.Sync.Atomic.get t.status
