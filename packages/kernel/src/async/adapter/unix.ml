open Common
open Prelude

type selector = int

type error =
  | InvalidTimeoutNs of { timeout_ns: int64 }
  | InvalidMaxEvents of { max_events: int }
  | System of System_error.t

type event = { fd: int; filter: int; flags: int; token: Token.t }

module FFI = struct
  external selector_create: unit -> (selector, int) Result.t = "kernel_new_async_unix_selector_create"

  external selector_close: selector -> (unit, int) Result.t = "kernel_new_async_unix_selector_close"

  external selector_wait: max_events:int -> timeout_ns:int64 -> selector -> (event array, int) Result.t = "kernel_new_async_unix_selector_wait"

  external selector_apply: selector -> event array -> int array -> (unit, int) Result.t = "kernel_new_async_unix_selector_apply"

  external selector_register_process: selector -> int -> Token.t -> (unit, int) Result.t = "kernel_new_async_unix_selector_register_process"

  external selector_reregister_process: selector -> int -> Token.t -> (unit, int) Result.t = "kernel_new_async_unix_selector_reregister_process"

  external selector_deregister_process: selector -> int -> (unit, int) Result.t = "kernel_new_async_unix_selector_deregister_process"

  external selector_register_timer: selector -> int -> (int * int) -> bool -> Token.t -> (unit, int) Result.t = "kernel_new_async_unix_selector_register_timer"

  external selector_reregister_timer: selector -> int -> (int * int) -> bool -> Token.t -> (unit, int) Result.t = "kernel_new_async_unix_selector_reregister_timer"

  external selector_deregister_timer: selector -> int -> (unit, int) Result.t = "kernel_new_async_unix_selector_deregister_timer"

  let create = fun () -> Result.map_err (selector_create ()) ~fn:(
    fun code -> System (System_error.from_code code)
  )

  let wait = fun ~max_events ~timeout_ns selector -> Result.map_err (selector_wait ~max_events ~timeout_ns selector) ~fn:(
    fun code -> System (System_error.from_code code)
  )

  let close = fun selector -> Result.map_err (selector_close selector) ~fn:(
    fun code -> System (System_error.from_code code)
  )

  let apply = fun selector changes ignored_errors -> Result.map_err (selector_apply selector changes ignored_errors) ~fn:(
    fun code -> System (System_error.from_code code)
  )

  let register_process = fun selector ~pid ~token -> Result.map_err (selector_register_process selector pid token) ~fn:(
    fun code -> System (System_error.from_code code)
  )

  let reregister_process = fun selector ~pid ~token -> Result.map_err (selector_reregister_process selector pid token) ~fn:(
    fun code -> System (System_error.from_code code)
  )

  let deregister_process = fun selector ~pid -> Result.map_err (selector_deregister_process selector pid) ~fn:(
    fun code -> System (System_error.from_code code)
  )

  let register_timer = fun selector ~timer_id ~timeout_parts ~repeat ~token -> Result.map_err (selector_register_timer selector timer_id timeout_parts repeat token) ~fn:(
    fun code -> System (System_error.from_code code)
  )

  let reregister_timer = fun selector ~timer_id ~timeout_parts ~repeat ~token -> Result.map_err (selector_reregister_timer selector timer_id timeout_parts repeat token) ~fn:(
    fun code -> System (System_error.from_code code)
  )

  let deregister_timer = fun selector ~timer_id -> Result.map_err (selector_deregister_timer selector timer_id) ~fn:(
    fun code -> System (System_error.from_code code)
  )
end

module Kevent = struct
  type t = event

  let make = fun fd ~filter ~flags ~token ->
    {
      fd;
      filter;
      flags;
      token
    }

  let token = fun event -> event.token

  let is_error = fun event -> event.flags land Libc.ev_error != 0

  let is_priority = fun event -> event.filter = Libc.evfilt_proc

  let is_readable = fun event -> event.filter = Libc.evfilt_read || event.filter = Libc.evfilt_timer

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
    if timeout < 0L then
      Result.Error (InvalidTimeoutNs { timeout_ns = timeout })
    else
      if max_events <= 0 then
        Result.Error (InvalidMaxEvents { max_events })
      else
        let* events = FFI.wait ~max_events ~timeout_ns:timeout selector
        in
        let rec to_list index acc =
          if index < 0 then
            acc
          else to_list (index - 1) (Event.make (module Kevent) (Array.get_unchecked events ~at:index) :: acc)
        in
        Result.Ok (to_list (Array.length events - 1) [])

  let register = fun selector ~fd ~token ~interest ->
    let flags = Libc.(ev_clear lor ev_receipt lor ev_add) in
    let changes =
      match Interest.is_readable interest, Interest.is_writable interest with
      | true, true -> [ Kevent.make fd ~filter:Libc.evfilt_read ~flags ~token; Kevent.make fd ~filter:Libc.evfilt_write ~flags ~token ]
      | true, false -> [ Kevent.make fd ~filter:Libc.evfilt_read ~flags ~token ]
      | false, true -> [ Kevent.make fd ~filter:Libc.evfilt_write ~flags ~token ]
      | false, false -> []
    in
    FFI.apply selector (Array.from_list changes) [|System_error_code.broken_pipe|]

  let reregister = fun selector ~fd ~token ~interest ->
    let flags = Libc.(ev_clear lor ev_receipt) in
    let write_flags =
      if Interest.is_writable interest then
        flags lor Libc.ev_add
      else flags lor Libc.ev_delete
    in
    let read_flags =
      if Interest.is_readable interest then
        flags lor Libc.ev_add
      else flags lor Libc.ev_delete
    in
    let changes =
      [|
        Kevent.make fd ~filter:Libc.evfilt_write ~flags:write_flags ~token;
        Kevent.make fd ~filter:Libc.evfilt_read ~flags:read_flags ~token;
      |]
    in
    FFI.apply selector changes
      [|
        System_error_code.broken_pipe;
        System_error_code.no_such_file_or_directory;
      |]

  let deregister = fun selector ~fd ->
    let flags = Libc.(ev_delete lor ev_receipt) in
    let token = Token.make 0 in
    let changes =
      [|
        Kevent.make fd ~filter:Libc.evfilt_write ~flags ~token;
        Kevent.make fd ~filter:Libc.evfilt_read ~flags ~token;
      |]
    in
    FFI.apply selector changes [|System_error_code.no_such_file_or_directory|]

  let register_process = fun selector ~pid ~token -> FFI.register_process selector ~pid ~token

  let reregister_process = fun selector ~pid ~token -> FFI.reregister_process selector ~pid ~token

  let deregister_process = fun selector ~pid -> FFI.deregister_process selector ~pid

  let register_timer = fun selector ~timer_id ~token ~timeout_parts ~repeat -> FFI.register_timer selector ~timer_id ~timeout_parts ~repeat ~token

  let reregister_timer = fun selector ~timer_id ~token ~timeout_parts ~repeat -> FFI.reregister_timer selector ~timer_id ~timeout_parts ~repeat ~token

  let deregister_timer = fun selector ~timer_id -> FFI.deregister_timer selector ~timer_id
end
