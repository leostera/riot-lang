module Pid = Pid
module Message = Message
module Process = Process

let enable_trace () = Trace.enable ()
let disable_trace () = Trace.disable ()

type Message.t += Exit

let run ~main = Scheduler.run ~main
let spawn fn = Scheduler.spawn (Scheduler.get_scheduler ()) fn
let self () = Scheduler.self ()
let send pid msg = Scheduler.send pid msg

let yield () = Effect.perform Proc_effect.Yield

let receive () = 
  Effect.perform (Proc_effect.Receive { selector = fun msg -> `select msg })

let selective_receive selector =
  Effect.perform (Proc_effect.Receive { selector })

let exit () = Process.Normal

let sleep _seconds = 
  (* For now, just yield - no timer support yet *)
  yield ()