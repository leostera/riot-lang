open Collections
open Sync
open Scheduler_types

type deps = {
  ensure_can_run_once: unit -> unit;
  create: config:Config.t -> t;
  spawn_on_worker:
    t -> worker_id:Scheduler_id.t -> (unit -> (unit, Process.exit_reason) Kernel.result) -> Pid.t;
  worker_loop: t -> worker -> unit;
  reactor_loop: t -> unit;
  join_blocking_lanes: t -> unit;
}

let run = fun deps ~config ~main ->
  deps.ensure_can_run_once ();
  let t = deps.create ~config in
  let _ = deps.spawn_on_worker t ~worker_id:Scheduler_id.zero main in
  let reactor_domain =
    Kernel.Domain.spawn (fun () -> deps.reactor_loop t)
  in
  let worker_domains =
    Array.init (Kernel.Int.sub (Array.length t.workers) 1)
      (fun idx ->
        let worker = t.workers.(Kernel.Int.add idx 1) in
        Kernel.Domain.spawn
          (fun () ->
            deps.worker_loop t worker))
  in
  deps.worker_loop t t.workers.(0);
  Array.iter Kernel.Domain.join worker_domains;
  Kernel.Domain.join reactor_domain;
  deps.join_blocking_lanes t;
  Sync.Atomic.get t.status
