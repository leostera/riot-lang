module Runtime = Runtime
module Pid = Pid
module Message = Message
module Process = Process
module File = File
module Net = Net

let enable_trace () = Trace.enable ()
let disable_trace () = Trace.disable ()

type Message.t += Exit

let run ~main = Scheduler.run ~main
let shutdown ~status = Scheduler.shutdown (Scheduler.get_scheduler ()) ~status
let spawn fn = Scheduler.spawn (Scheduler.get_scheduler ()) fn
let self () = Scheduler.self ()
let send pid msg = Scheduler.send pid msg

include Effects
