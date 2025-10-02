let yield () = Effect.perform Proc_effect.Yield

type 'msg selector = Message.t -> [ `select of 'msg | `skip ]

let receive_any () =
  Effect.perform (Proc_effect.Receive { selector = (fun msg -> `select msg) })

let receive ~selector () = Effect.perform (Proc_effect.Receive { selector })
let exit () = Ok ()

let sleep _milliseconds =
  (* For now, just yield - no timer support yet *)
  yield ()

let syscall ?timeout ~name ~interest ~source cb =
  let timeout =
    match timeout with None -> `infinity | Some after -> `after after
  in
  Effect.perform (Proc_effect.Syscall { name; interest; source; timeout });
  cb ()
