open Global
open Collections

type 'a t = {
  pid: Pid.t;
  ref: 'a Ref.t;
}

type Message.t +=
  | Reply: 'a Ref.t * 'a -> Message.t
  | Crash: 'a Ref.t * exn -> Message.t

let async = fun fn ->
  let ref = Ref.make () in
  let this = self () in
  let pid =
    spawn
      (fun () ->
        let reply =
          match fn () with
          | exception exn -> Crash (ref, exn)
          | value -> Reply (ref, value)
        in
        send this reply;
        Ok ())
  in
  { pid; ref }

let await: type res. res t -> (res, exn) result = fun t ->
  let selector: Message.t -> [`select of (res, exn) result | `skip] = fun msg ->
    match msg with
    | Crash (ref', exn) when Ref.equal t.ref ref' -> `select (Error exn)
    | Reply (ref', res) when Ref.equal t.ref ref' -> (
        match Ref.type_equal t.ref ref' with
        | Some Type.Equal -> `select (Ok res)
        | None -> panic "bad message"
      )
    | _ -> `skip
  in
  let result = receive ~selector () in
  result

(** Await multiple tasks efficiently, collecting results as they arrive *)
let rec await_all: type res. res t list -> (res, exn) result list = fun tasks ->
  match tasks with
  | [] -> []
  | _ ->
      let pending = List.enumerate tasks in
      let results = Array.make ~count:(List.length tasks) ~value:None in
      let find_task_by_ref pending ref =
        List.find pending ~fn:(fun (_index, task) -> Ref.equal task.ref ref)
      in
      let is_one_of_our_refs pending ref =
        List.any pending ~fn:(fun (_index, task) -> Ref.equal task.ref ref)
      in
      let remove_task pending task =
        List.filter
          pending
          ~fn:(fun (_index, pending_task) -> not (Ref.equal pending_task.ref task.ref))
      in
      let rec collect remaining pending =
        if remaining = 0 then
          Array.to_list (Array.map results ~fn:Option.unwrap)
        else
          (
            let selector: Message.t -> [`select of (res, exn) result * int * res t | `skip] = fun msg ->
              match msg with
              | Crash (ref', exn) when is_one_of_our_refs pending ref' -> (
                  match find_task_by_ref pending ref' with
                  | Some (index, task) -> `select (Error exn, index, task)
                  | None -> panic "task awaited but no matching task found"
                )
              | Reply (ref', res) when is_one_of_our_refs pending ref' -> (
                  match find_task_by_ref pending ref' with
                  | None -> panic "task awaited but no matching task found"
                  | Some (index, task) -> (
                      match Ref.type_equal task.ref ref' with
                      | Some Type.Equal -> `select (Ok res, index, task)
                      | None -> panic "bad message"
                    )
                )
              | _ -> `skip
            in
            let (result, index, task) = receive ~selector () in
            Array.set results ~at:index ~value:(Some result);
            collect (remaining - 1) (remove_task pending task)
          )
      in
      collect (List.length tasks) pending
