open Global
open Collections
module Ev = Kernel.Fs.Events

type t = {
  kernel: Ev.t;
  source: Kernel.Async.Source.t;
  mutable buffer: bytes;
  mutable buffer_len: int;
}

type watch_id = Ev.watch_id

type error = IO.error

let create = fun () ->
  match Ev.create () with
  | Error e -> Error e
  | Ok kernel ->
      let source = Ev.to_source kernel in
      Ok {kernel; source; buffer = IO.Bytes.create 4_096; buffer_len = 0; }

let watch = fun t ~path ~latency -> Ev.watch
t.kernel
~path:(Path.to_string path)
~latency:(Time.Duration.to_secs_float latency)

let unwatch = fun t watch_id ->
  Ev.unwatch t.kernel watch_id

(* Try to parse ONE event from the buffer, return (event option, bytes_consumed) *)

let try_parse_event = fun buffer buffer_len ->
  if buffer_len < 16 then
    (None, 0)
    (* Need at least 16 bytes for len+flags+event_id *)
  else
    let path_len = Int32.to_int (IO.Bytes.get_int32_ne buffer 0) in
    let total_needed = 16 + path_len in
    if buffer_len < total_needed then
      (None, 0)
      (* Not enough for full event *)
    else
      let flags = IO.Bytes.get_int32_ne buffer 4 in
      let event_id = IO.Bytes.get_int64_ne buffer 8 in
      let path = IO.Bytes.sub_string buffer 16 path_len in
      let kernel_event = {Ev.path; flags; event_id} in
      let event = Event.from_kernel_event kernel_event in
      (Some event, total_needed)

(* Shift buffer contents left by n bytes *)

let shift_buffer = fun t n ->
  let remaining = t.buffer_len - n in
  if remaining > 0 then
    IO.Bytes.blit t.buffer n t.buffer 0 remaining;
  t.buffer_len <- remaining

(* Read from fd into buffer and try to parse events *)

let poll = fun t ->
  let rec read_and_parse = fun events ->
    match try_parse_event t.buffer t.buffer_len with
    | (Some event, consumed) ->
        shift_buffer t consumed;
        (* Return immediately with the parsed event *)
        Ok (List.rev (event :: events))
    | (None, _) ->
        (* No complete event in buffer, need to read more data *)
        let fd = Ev.get_fd t.kernel in
        let file = File.from_fd fd in
        let read_buf = IO.Bytes.create 1_024 in
        match File.read file read_buf ~offset:0 ~len:1_024 with
        | Ok 0 ->
            (* No data available, return what we have *)
            Ok (List.rev events)
        | Ok n ->
            (* Append new bytes to buffer *)
            let new_len = t.buffer_len + n in
            if new_len > IO.Bytes.length t.buffer then
              begin
                (* Need bigger buffer *)
                let new_buf = IO.Bytes.create (new_len * 2) in
                IO.Bytes.blit t.buffer 0 new_buf 0 t.buffer_len;
                IO.Bytes.blit read_buf 0 new_buf t.buffer_len n;
                t.buffer <- new_buf;
              end
            else
              begin
                IO.Bytes.blit read_buf 0 t.buffer t.buffer_len n;
              end;
              t.buffer_len <- new_len;
              read_and_parse events
        | Error (IO.Operation_would_block | IO.Resource_unavailable_try_again) ->
            (* If we have events already, return them *)
            if List.length events > 0 then
              Ok (List.rev events)
              (* Otherwise wait for readable *)
            else
              Miniriot.syscall
              ~name:"Fs.Events.read"
              ~interest:Kernel.Async.Interest.readable
              ~source:t.source
              (fun () -> read_and_parse events)
        | Error e ->
            Error e
  in
  read_and_parse []

let stop = fun t -> Ev.stop t.kernel
