open Global
open Miniriot

type 'a t = { pid : Pid.t; ref : 'a Ref.t }

type Message.t +=
  | Reply : 'a Ref.t * 'a -> Message.t
  | Crash : 'a Ref.t * exn -> Message.t

let async fn =
  let ref = Ref.make () in
  let this = self () in
  let pid =
    spawn (fun () ->
        let reply =
          match fn () with
          | exception exn -> Crash (ref, exn)
          | value -> Reply (ref, value)
        in
        send this reply;
        Ok ())
  in
  { pid; ref }

let await : type res. res t -> (res, exn) result =
 fun t ->
  let selector : Message.t -> [ `select of (res, exn) result | `skip ] =
   fun msg ->
    match msg with
    | Crash (ref', exn) when Ref.equal t.ref ref' -> `select (Error exn)
    | Reply (ref', res) when Ref.equal t.ref ref' -> (
        match Ref.type_equal t.ref ref' with
        | Some Type.Equal -> `select (Ok res)
        | None -> panic "bad message")
    | _ -> `skip
  in
  receive ~selector ()
