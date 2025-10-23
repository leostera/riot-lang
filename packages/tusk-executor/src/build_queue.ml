open Std
open Std.Collections
open Tusk_model

type package_task = { package : Package.t; dependencies : Package.t list }

type t = {
  ready : package_task Queue.t;
  waiting : package_task Queue.t;
  completed : (string, Package_builder.build_result) HashMap.t;
}

let create () =
  {
    ready = Queue.create ();
    waiting = Queue.create ();
    completed = HashMap.create ();
  }

let enqueue queue task =
  if List.length task.dependencies = 0 then Queue.enqueue queue.ready task
  else Queue.enqueue queue.waiting task

let mark_completed queue result =
  let _ =
    HashMap.insert queue.completed result.Package_builder.package.name result
  in

  let still_waiting = Queue.create () in
  let rec check_waiting () =
    match Queue.dequeue queue.waiting with
    | None -> ()
    | Some task ->
        let deps_ready =
          List.for_all
            (fun (dep : Package.t) ->
              match HashMap.get queue.completed dep.name with
              | Some { status = Built _ | Cached _; _ } -> true
              | Some { status = Failed _; _ } -> false
              | None -> false)
            task.dependencies
        in
        if deps_ready then Queue.enqueue queue.ready task
        else Queue.enqueue still_waiting task;
        check_waiting ()
  in
  check_waiting ();

  let rec move_back () =
    match Queue.dequeue still_waiting with
    | None -> ()
    | Some task ->
        Queue.enqueue queue.waiting task;
        move_back ()
  in
  move_back ()

let next queue = Queue.dequeue queue.ready
let requeue queue task = Queue.enqueue queue.waiting task

let stats queue =
  let ready = Queue.len queue.ready in
  let waiting = Queue.len queue.waiting in
  let completed = HashMap.len queue.completed in
  (ready, waiting, completed)
