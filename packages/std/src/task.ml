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

let await : type res. res t -> (res, exn) result = fun t ->
  let selector : Message.t -> [
    `select of (res, exn) result
    | `skip
  ] = fun msg ->
    match msg with
    | Crash (ref', exn) when Ref.equal t.ref ref' ->
        `select (Error exn)
    | Reply (ref', res) when Ref.equal t.ref ref' -> (
        match Ref.type_equal t.ref ref' with
        | Some Type.Equal -> `select (Ok res)
        | None -> panic "bad message"
      )
    | _ ->
        `skip
  in
  let result = receive ~selector () in
  result

(** Await multiple tasks efficiently, collecting results as they arrive *)
let rec await_all : type res. res t list -> (res, exn) result list = fun tasks ->
  let find_task_by_ref tasks ref =
    List.find
      (fun t ->
        Ref.equal t.ref ref)
      tasks
  in
  let is_one_of_our_refs tasks ref =
    List.exists
      (fun t ->
        Ref.equal t.ref ref)
      tasks
  in
  let remove_task tasks task =
    List.filter (fun t -> not (Ref.equal t.ref task.ref)) tasks
  in
  let selector : Message.t -> [
    `select of (res, exn) result * res t
    | `skip
  ] = fun msg ->
    match msg with
    | Crash (ref', exn) when is_one_of_our_refs tasks ref' ->
        let task : res t = find_task_by_ref tasks ref' in
        `select (Error exn, task)
    | Reply (ref', res) when is_one_of_our_refs tasks ref' -> (
        let task : res t = find_task_by_ref tasks ref' in
        match Ref.type_equal task.ref ref' with
        | Some Type.Equal -> `select (Ok res, task)
        | None -> panic "bad message"
      )
    | _ ->
        `skip
  in
  let result, task = receive ~selector () in
  match remove_task tasks task with
  | [] -> [ result ]
  | tasks' -> result :: await_all tasks'
