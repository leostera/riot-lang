open Common
open Collections
open Sync
open Sync.Cell

type kevent

type kqueue = Fd.t

type event = {
  fd: Fd.t;
  filter: int;
  flags: int;
  token: int;
}

module FFI = struct
  external kernel_unix_kevent: max_events:int -> timeout:int64 -> kqueue -> event array = "kernel_unix_kevent"

  let kevent = fun ~max_events ~timeout kq ->
      try Ok (kernel_unix_kevent ~max_events ~timeout kq) with
      | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

  external kernel_unix_kqueue: unit -> kqueue = "kernel_unix_kqueue"

  let kqueue = fun () ->
      try Ok (kernel_unix_kqueue ()) with
      | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

  external kernel_unix_fcntl: Fd.t -> cmd:int -> arg:int -> int = "kernel_unix_fcntl"

  let fcntl = fun fd cmd arg ->
      try Ok (kernel_unix_fcntl fd ~cmd ~arg) with
      | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

  external kernel_unix_kevent_register: kqueue -> event array -> int array -> unit = "kernel_unix_kevent_register"

  let kevent_register = fun fd changes ignored_errors ->
      try Ok (kernel_unix_kevent_register fd changes ignored_errors) with
      | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)
end

module Kevent = struct
  type t = event

  let make = fun fd ~filter ~flags ~token -> {fd; filter; flags; token}

  let filter = fun t -> t.filter

  let flags = fun t -> t.flags

  let token = fun t -> Token.make t.token

  let is_readable = fun t -> filter t = Libc.evfilt_read

  let is_writable = fun t -> filter t = Libc.evfilt_write

  let is_error = fun t -> flags t land Libc.ev_error != 0

  let is_read_closed = fun t -> is_readable t && flags t land Libc.ev_eof != 0

  let is_write_closed = fun t -> is_writable t && flags t land Libc.ev_eof != 0

  let is_priority = fun _t -> false
end

module Selector = struct
  let name = "kqueue"

  type t = {
    kq: kqueue;
  }

  let make = fun () ->
      let* kq = FFI.kqueue () in
      let* _ =
        FFI.(fcntl kq Libc.f_setfd Libc.f_dupfd_cloexec)
      in
      Ok {kq}

  let select = fun ?(timeout = 500_000_000L) ?(max_events = 1_000) t ->
      let* events = FFI.kevent ~timeout ~max_events t.kq in
      let events = Array.to_list events in
      let events =
        List.map (Event.make (module Kevent)) events
      in
      Ok events

  let register = fun t ~fd ~token ~interest ->
      let token = Token.unsafe_to_int token in
      (* Use level-triggered mode (EV_ENABLE) for TTYs, edge-triggered (EV_CLEAR) for others *)
      let flags =
        if Fd.is_tty fd then
          Libc.(ev_enable lor ev_receipt lor ev_add)
        else
          Libc.(ev_clear lor ev_receipt lor ev_add)
      in
      let changes = Cell.create [] in
      (
        if Interest.is_writable interest then
          let kevent = Kevent.make fd ~filter:Libc.evfilt_write ~flags ~token in
          changes := kevent :: !changes
      );
      (
        if Interest.is_readable interest then
          let kevent = Kevent.make fd ~filter:Libc.evfilt_read ~flags ~token in
          changes := kevent :: !changes
      );
      let changes = Array.of_list !changes in
      FFI.kevent_register t.kq changes [|Libc.epipe|]

  let reregister = fun t ~fd ~token ~interest ->
      let token = Token.unsafe_to_int token in
      (* Use level-triggered mode (EV_ENABLE) for TTYs, edge-triggered (EV_CLEAR) for others *)
      let flags =
        if Fd.is_tty fd then
          Libc.(ev_enable lor ev_receipt)
        else
          Libc.(ev_clear lor ev_receipt)
      in
      let write_flags =
        if Interest.is_writable interest then
          Libc.(flags lor ev_add)
        else
          Libc.(flags lor ev_delete)
      in
      let read_flags =
        if Interest.is_readable interest then
          Libc.(flags lor ev_add)
        else
          Libc.(flags lor ev_delete)
      in
      let changes = [|
        Kevent.make fd ~filter:Libc.evfilt_write ~flags:write_flags ~token;
        Kevent.make fd ~filter:Libc.evfilt_read ~flags:read_flags ~token;

      |] in
      FFI.kevent_register t.kq changes Libc.[|epipe; enoent|]

  let deregister = fun t ~fd ->
      let flags = Libc.(ev_delete lor ev_receipt) in
      let changes = [|
        Kevent.make fd ~filter:Libc.evfilt_write ~flags ~token:0;
        Kevent.make fd ~filter:Libc.evfilt_read ~flags ~token:0;

      |] in
      FFI.kevent_register t.kq changes Libc.[|enoent|]
end

module Event = Kevent
