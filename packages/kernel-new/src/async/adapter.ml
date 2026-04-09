open Common
open Prelude

type selector = int

type event = {
  fd: int;
  filter: int;
  flags: int;
  token: Token.t;
}

module FFI = struct
  external selector_create: unit -> (selector, int) Result.t = "kernel_new_async_unix_selector_create"

  external selector_close: selector -> (unit, int) Result.t = "kernel_new_async_unix_selector_close"

  external selector_wait:
    max_events:int -> timeout_ns:int64 -> selector -> (event array, int) Result.t
    = "kernel_new_async_unix_selector_wait"

  external selector_apply: selector -> event array -> int array -> (unit, int) Result.t = "kernel_new_async_unix_selector_apply"

  let create = fun () ->
    Result.map_error Error.of_code (selector_create ())

  let wait = fun ~max_events ~timeout_ns selector ->
    Result.map_error Error.of_code (selector_wait ~max_events ~timeout_ns selector)

  let close = fun selector ->
    Result.map_error Error.of_code (selector_close selector)

  let apply = fun selector changes ignored_errors ->
    Result.map_error Error.of_code (selector_apply selector changes ignored_errors)
end

module Kevent = struct
  type t = event

  let make = fun fd ~filter ~flags ~token -> { fd; filter; flags; token }

  let token = fun event -> event.token

  let is_error = fun event -> event.flags land Libc.ev_error != 0

  let is_priority = fun _event -> false

  let is_readable = fun event -> event.filter = Libc.evfilt_read

  let is_writable = fun event -> event.filter = Libc.evfilt_write

  let is_read_closed = fun event -> is_readable event && event.flags land Libc.ev_eof != 0

  let is_write_closed = fun event -> is_writable event && event.flags land Libc.ev_eof != 0
end

module Selector = struct
  type t = selector

  let name = "kqueue"

  let make = FFI.create

  let close = FFI.close

  let select = fun ?(timeout = 500_000_000L) ?(max_events = 1_024) selector ->
    let* events = FFI.wait ~max_events ~timeout_ns:timeout selector in
    let rec to_list index acc =
      if index < 0 then
        acc
      else
        to_list (index - 1) (Event.make (module Kevent) (Array.get events index) :: acc)
    in
    Result.Ok (to_list (Array.length events - 1) [])

  let register = fun selector ~fd ~token ~interest ->
    let flags = Libc.(ev_clear lor ev_receipt lor ev_add) in
    let changes =
      match (Interest.is_readable interest, Interest.is_writable interest) with
      | true, true -> [
        Kevent.make fd ~filter:Libc.evfilt_read ~flags ~token;
        Kevent.make fd ~filter:Libc.evfilt_write ~flags ~token;
      ]
      | true, false -> [ Kevent.make fd ~filter:Libc.evfilt_read ~flags ~token ]
      | false, true -> [ Kevent.make fd ~filter:Libc.evfilt_write ~flags ~token ]
      | false, false -> []
    in
    FFI.apply selector (Array.of_list changes) [|Error.code_broken_pipe|]

  let reregister = fun selector ~fd ~token ~interest ->
    let flags = Libc.(ev_clear lor ev_receipt) in
    let write_flags =
      if Interest.is_writable interest then
        flags lor Libc.ev_add
      else
        flags lor Libc.ev_delete
    in
    let read_flags =
      if Interest.is_readable interest then
        flags lor Libc.ev_add
      else
        flags lor Libc.ev_delete
    in
    let changes = [|
      Kevent.make fd ~filter:Libc.evfilt_write ~flags:write_flags ~token;
      Kevent.make fd ~filter:Libc.evfilt_read ~flags:read_flags ~token;
    |] in
    FFI.apply selector changes [|Error.code_broken_pipe; Error.code_no_such_file_or_directory|]

  let deregister = fun selector ~fd ->
    let flags = Libc.(ev_delete lor ev_receipt) in
    let token = Token.make 0 in
    let changes = [|
      Kevent.make fd ~filter:Libc.evfilt_write ~flags ~token;
      Kevent.make fd ~filter:Libc.evfilt_read ~flags ~token;
    |] in
    FFI.apply selector changes [|Error.code_no_such_file_or_directory|]
end
