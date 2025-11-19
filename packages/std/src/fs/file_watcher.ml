open Global
open Process
open Sync
module List = Collections.List

type t = Pid.t

type Message.t += | FileEvents of Event.t list

type state = { watcher: Events.t; subscriber: Pid.t }

let rec loop state =
  let events = Events.poll state.watcher |> Result.expect ~msg:"Could not read events" in
  if List.length events > 0 then
    send state.subscriber (FileEvents events);
  loop state

let init ~latency ~root:path ~subscriber = 
  let watcher = Events.create () |> Result.expect ~msg:"Failed to create file watcher" in
    
  let _watch_id = 
    Events.watch watcher ~path ~latency
    |> Result.expect ~msg:("Failed to watch: " ^ (Path.to_string path))
  in

  loop { subscriber; watcher; }

let start_link ?(latency=(Time.Duration.from_millis 1)) ~root () =
  let subscriber = self () in
  spawn_link (fun () -> init ~latency ~root ~subscriber)
