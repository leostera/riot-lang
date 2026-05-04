open Global

module Ev = Kernel.Fs.Events

type t = {
  kernel: Ev.t;
  source: Kernel.Async.Source.t;
}

type watch_id = Ev.watch_id

type error = IO.error

let from_events_error = fun __tmp1 ->
  match __tmp1 with
  | Ev.Closed -> IO.Closed
  | Ev.AlreadyWatching -> IO.Invalid_argument
  | Ev.System error -> IO.from_system_error error

let create = fun () ->
  match Ev.create () with
  | Error error -> Error (from_events_error error)
  | Ok kernel ->
      let source = Ev.to_source kernel in
      Ok { kernel; source }

let watch = fun t ~path ~latency ->
  match Ev.watch
    t.kernel
    ~path:(Kernel.Path.from_string (Path.to_string path))
    ~latency:(Time.Duration.to_secs_float latency) with
  | Ok watch_id -> Ok watch_id
  | Error error -> Error (from_events_error error)

let unwatch = fun t watch_id ->
  match Ev.unwatch t.kernel watch_id with
  | Ok () -> Ok ()
  | Error error -> Error (from_events_error error)

let is_would_block = fun __tmp1 ->
  match __tmp1 with
  | Ev.System error -> Kernel.SystemError.would_block error
  | _ -> false

let poll = fun t ->
  let rec map_events = fun __tmp1 ->
    match __tmp1 with
    | [] -> []
    | event :: rest -> Event.from_kernel_event event :: map_events rest
  in
  let rec await_ready () =
    match Ev.poll t.kernel with
    | Ok events -> Ok (map_events events)
    | Error error when is_would_block error ->
        Runtime.syscall
          ~name:"Fs.Events.poll"
          ~interest:Kernel.Async.Interest.readable
          ~source:t.source
          await_ready
    | Error error -> Error (from_events_error error)
  in
  await_ready ()

let stop = fun t ->
  match Ev.stop t.kernel with
  | Ok () -> Ok ()
  | Error error -> Error (from_events_error error)
