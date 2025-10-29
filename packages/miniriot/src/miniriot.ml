module Runtime = Runtime
module Pid = Pid
module Message = Message
module Process = Process
module Config = Config
module Timer_id = Timer_id

let enable_trace () = Trace.enable ()
let disable_trace () = Trace.disable ()

type Message.t += Exit

let run ~main ~args ?config () =
  let config = Option.value config ~default:Config.default in
  Kernel.Exception.record_backtrace true;
  Scheduler.run ~config ~main:(fun () -> main ~args) |> exit

let shutdown ~status = Scheduler.shutdown (Scheduler.get_scheduler ()) ~status
let spawn fn = Scheduler.spawn (Scheduler.get_scheduler ()) fn
let self () = Scheduler.self ()
let send pid msg = Scheduler.send pid msg

(* Cooperative I/O syscall for actor-aware I/O operations *)
let syscall ~name ~interest ~source ~timeout =
  Effect.perform (Proc_effect.Syscall { name; interest; source; timeout })

module Timer = struct
  type id = Timer_id.t

  let send_after target_pid (msg : Message.t) ~after =
    let sch = Scheduler.get_scheduler () in
    let now = Kernel.Time.monotonic_time_nanos () in
    let duration_nanos = Int64.of_float (after *. 1_000_000_000.0) in
    Scheduler.add_timer sch ~now ~duration_nanos ~mode:Timer.One_shot
      ~action:(Timer.Send_message (target_pid, msg))

  let send_interval target_pid (msg : Message.t) ~interval =
    let sch = Scheduler.get_scheduler () in
    let now = Kernel.Time.monotonic_time_nanos () in
    let duration_nanos = Int64.of_float (interval *. 1_000_000_000.0) in
    Scheduler.add_timer sch ~now ~duration_nanos
      ~mode:(Timer.Interval duration_nanos)
      ~action:(Timer.Send_message (target_pid, msg))

  let cancel timer_id =
    let sch = Scheduler.get_scheduler () in
    Scheduler.cancel_timer sch timer_id
end

include Effects
