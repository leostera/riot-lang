open Global
open Miniriot

type 'a t = { pid : Pid.t; ref : 'a Ref.t }

type Message.t +=
  | Reply : 'a Ref.t * 'a -> Message.t
  | Crash : 'a Ref.t * exn -> Message.t

let async fn =
  let ref = Ref.make () in
  let this = self () in
  Format.eprintf "[TASK DEBUG] async: parent pid=%s, creating task\n%!"
    (Pid.to_string this);
  let pid =
    spawn (fun () ->
        Format.eprintf
          "[TASK DEBUG] Task starting, will send reply to pid=%s\n%!"
          (Pid.to_string this);
        let reply =
          match fn () with
          | exception exn ->
              Format.eprintf
                "[TASK DEBUG] Task crashed with exception, sending Crash\n%!";
              Crash (ref, exn)
          | value ->
              Format.eprintf "[TASK DEBUG] Task completed, sending Reply\n%!";
              Reply (ref, value)
        in
        send this reply;
        Format.eprintf "[TASK DEBUG] Reply sent to pid=%s\n%!"
          (Pid.to_string this);
        Ok ())
  in
  Format.eprintf "[TASK DEBUG] async: task pid=%s created\n%!"
    (Pid.to_string pid);
  { pid; ref }

let await : type res. res t -> (res, exn) result =
 fun t ->
  Format.eprintf "[TASK DEBUG] await: waiting for task pid=%s\n%!"
    (Pid.to_string t.pid);
  let selector : Message.t -> [ `select of (res, exn) result | `skip ] =
   fun msg ->
    match msg with
    | Crash (ref', exn) when Ref.equal t.ref ref' ->
        Format.eprintf "[TASK DEBUG] await: received Crash message\n%!";
        `select (Error exn)
    | Reply (ref', res) when Ref.equal t.ref ref' -> (
        Format.eprintf
          "[TASK DEBUG] await: received Reply message, checking type equality\n\
           %!";
        match Ref.type_equal t.ref ref' with
        | Some Type.Equal ->
            Format.eprintf
              "[TASK DEBUG] await: type match OK, returning result\n%!";
            `select (Ok res)
        | None ->
            Format.eprintf "[TASK DEBUG] await: type mismatch!\n%!";
            panic "bad message")
    | _ ->
        Format.eprintf
          "[TASK DEBUG] await: received other message, skipping\n%!";
        `skip
  in
  let result = receive ~selector () in
  Format.eprintf "[TASK DEBUG] await: task pid=%s completed with result\n%!"
    (Pid.to_string t.pid);
  result

(** Await multiple tasks efficiently, collecting results as they arrive *)
let rec await_all : type res. res t list -> (res, exn) result list =
 fun tasks ->
  let find_task_by_ref tasks ref =
    List.find (fun t -> Ref.equal t.ref ref) tasks
  in
  let is_one_of_our_refs tasks ref =
    List.exists
      (fun t ->
        let exists = Ref.equal t.ref ref in
        Format.eprintf "[TASK DEBUG] is_one_of_our_refs(%a,%a) -> %b\n%!" Ref.pp
          t.ref Ref.pp ref exists;
        exists)
      tasks
  in
  let remove_task tasks task =
    List.filter (fun t -> not (Ref.equal t.ref task.ref)) tasks
  in

  let selector : Message.t -> [ `select of (res, exn) result * res t | `skip ] =
   fun msg ->
    match msg with
    | Crash (ref', exn) when is_one_of_our_refs tasks ref' ->
        Format.eprintf "[TASK DEBUG] await: received Crash message\n%!";
        let task : res t = find_task_by_ref tasks ref' in
        `select (Error exn, task)
    | Reply (ref', res) when is_one_of_our_refs tasks ref' -> (
        Format.eprintf
          "[TASK DEBUG] await: received Reply message, checking type equality\n\
           %!";
        let task : res t = find_task_by_ref tasks ref' in
        match Ref.type_equal task.ref ref' with
        | Some Type.Equal ->
            Format.eprintf
              "[TASK DEBUG] await: type match OK, returning result\n%!";
            `select (Ok res, task)
        | None ->
            Format.eprintf "[TASK DEBUG] await: type mismatch!\n%!";
            panic "bad message")
    | _ ->
        Format.eprintf
          "[TASK DEBUG] await: received other message: %S, skipping\n%!"
          (Marshal.to_string msg []);
        `skip
  in
  let result, task = receive ~selector () in
  Format.eprintf "[TASK DEBUG] await: task pid=%s completed with result\n%!"
    (Pid.to_string task.pid);
  match remove_task tasks task with
  | [] -> [result]
  | tasks' -> result :: await_all tasks'
