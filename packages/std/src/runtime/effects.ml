open Kernel

module Exception = struct
  exception Receive_timeout

  exception Syscall_timeout
end

let yield = fun () -> Effect.perform Proc_effect.Yield

type 'msg selection = 'msg Proc_effect.selection =
  | Select of 'msg
  | Skip

type 'msg selector = Message.t -> 'msg selection

let to_proc_selector = fun selector msg ->
  match selector msg with
  | Select value -> Proc_effect.Select value
  | Skip -> Proc_effect.Skip

let receive_any = fun ?timeout () ->
  let timeout =
    match timeout with
    | None -> Proc_effect.Infinity
    | Some after -> Proc_effect.After after
  in
  Effect.perform
    (
      Proc_effect.Receive {
        selector = (fun msg -> Proc_effect.Select msg);
        timeout;
      }
    )

let receive = fun ~selector ?timeout () ->
  let timeout =
    match timeout with
    | None -> Proc_effect.Infinity
    | Some after -> Proc_effect.After after
  in
  Effect.perform (Proc_effect.Receive { selector = to_proc_selector selector; timeout })

let exit = fun () -> Ok ()

let syscall = fun ?timeout ~name ~interest ~source cb ->
  let timeout =
    match timeout with
    | None -> Proc_effect.Infinity
    | Some after -> Proc_effect.After after
  in
  Effect.perform
    (
      Proc_effect.Syscall {
        name;
        interest;
        source;
        timeout;
      }
    );
  cb ()
