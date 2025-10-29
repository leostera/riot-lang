exception Receive_timeout
exception Syscall_timeout

let yield () = Effect.perform Proc_effect.Yield

type 'msg selector = Message.t -> [ `select of 'msg | `skip ]

let receive_any ?timeout () =
  let timeout =
    match timeout with None -> `infinity | Some after -> `after after
  in
  Effect.perform
    (Proc_effect.Receive { selector = (fun msg -> `select msg); timeout })

let receive ~selector ?timeout () =
  let timeout =
    match timeout with None -> `infinity | Some after -> `after after
  in
  Effect.perform (Proc_effect.Receive { selector; timeout })

let exit () = Ok ()

let sleep _milliseconds =
  (* For now, just yield - proper timer-based sleep will be added later *)
  yield ()

let syscall ?timeout ~name ~interest ~source cb =
  let timeout =
    match timeout with None -> `infinity | Some after -> `after after
  in
  Effect.perform (Proc_effect.Syscall { name; interest; source; timeout });
  cb ()
