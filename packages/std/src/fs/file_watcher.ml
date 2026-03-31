open Global
open Process
open Sync
module List = Collections.List

type t = Pid.t

type Message.t +=
  | FileEvents of Event.t list

type state = {
  watcher: Events.t;
  subscriber: Pid.t;
  ignore_prefixes: Path.t list;
}

let should_ignore = fun ~ignore_prefixes path ->
  List.exists
    (fun prefix ->
      match Path.strip_prefix path ~prefix with
      | Ok _ -> true
      | Error _ -> false)
    ignore_prefixes

let rec loop = fun state ->
  let events = Events.poll state.watcher |> Result.expect ~msg:"Could not read events" in
  let filtered_events =
    List.filter
    (fun (event:Event.t) -> not
    (should_ignore ~ignore_prefixes:state.ignore_prefixes event.Event.path))
    events
  in
  if List.length filtered_events > 0 then
    send state.subscriber (FileEvents filtered_events);
  loop state

let init = fun ~latency ~root:path ~ignore_prefixes ~subscriber ->
  let watcher = Events.create () |> Result.expect ~msg:"Failed to create file watcher" in
  let _watch_id = Events.watch watcher ~path ~latency
  |> Result.expect ~msg:(((("Failed to watch: " ^ (Path.to_string path))))) in
  loop {subscriber; watcher; ignore_prefixes; }

let start_link = fun ?(latency = (Time.Duration.from_millis 1)) ?(ignore_prefixes = []) ~root () ->
  let subscriber = self () in
  spawn_link (fun () -> init ~latency ~root ~ignore_prefixes ~subscriber)
