open Std
open Std.Collections

type ('task, 'result, 'error) run_result = ('task * ('result, 'error) result) list

let run:
  concurrency:int ->
  tasks:'task list ->
  fn:('task -> ('result * 'task list, 'error) result) ->
  ('task, 'result, 'error) run_result
  = fun ~concurrency ~tasks ~fn ->
  if tasks = [] then
    []
  else
  let rec loop next_id ~queue ~acc =
    match queue with
    | [] ->
        List.sort acc ~compare:(fun (left, _) (right, _) -> Int.compare left right)
        |> List.map ~fn:(fun (_, item) -> item)
    | _ ->
          let wave_base_id = next_id in
          let wave_results =
            WorkerPool.SimpleWorkerPool.run
              ~concurrency
              ~tasks:queue
              ~fn:(fun task ->
                match fn task with
                | Ok (result, more) -> (task, Ok result, more)
                | Error error -> (task, Error error, []))
              ()
          in
          let next_id = next_id + List.length queue in
          let next_queue, completed =
            List.fold_left
              wave_results
              ~acc:([], acc)
              ~fn:(fun (queued, completed) (id, (task, outcome, more)) ->
                let queued = List.fold_left more ~acc:queued ~fn:(fun queued task -> task :: queued) in
                (queued, (wave_base_id + id, (task, outcome)) :: completed))
          in
          let completed = List.reverse completed in
          let acc = completed @ acc in
          loop next_id ~queue:(List.rev next_queue) ~acc
    in
    loop 0 ~queue:tasks ~acc:[]
