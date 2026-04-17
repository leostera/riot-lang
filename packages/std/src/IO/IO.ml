open Prelude
module Buffer = Buffer
module Bytes = Bytes
module Iovec = Kernel.IO.Iovec
module Reader = Reader
module Writer = Writer

type error =
  | End_of_file
  | Timeout
  | Closed
  | Connection_closed
  | Process_down
  | No_info
  | Noop
  | Permission_denied
  | No_such_file_or_directory
  | Interrupted_system_call
  | Input_output_error
  | Bad_file_descriptor
  | Resource_unavailable_try_again
  | Out_of_memory
  | Permission_denied_on_file
  | Bad_address
  | Resource_busy
  | File_exists
  | Cross_device_link
  | Invalid_argument
  | Too_many_open_files_in_system
  | Too_many_open_files
  | Invalid_operation_on_device
  | File_too_large
  | No_space_left_on_device
  | Illegal_seek
  | Read_only_filesystem
  | Too_many_links
  | Broken_pipe
  | Numerical_argument_out_of_domain
  | Numerical_result_out_of_range
  | Resource_deadlock_would_occur
  | Filename_too_long
  | No_locks_available
  | Function_not_implemented
  | Directory_not_empty
  | Too_many_symbolic_links
  | Operation_would_block
  | Socket_operation_on_non_socket
  | Destination_address_required
  | Message_too_long
  | Protocol_wrong_type_for_socket
  | Protocol_not_available
  | Protocol_not_supported
  | Socket_type_not_supported
  | Operation_not_supported
  | Protocol_family_not_supported
  | Address_family_not_supported
  | Address_already_in_use
  | Cannot_assign_requested_address
  | Network_is_down
  | Network_is_unreachable
  | Network_dropped_connection_on_reset
  | Software_caused_connection_abort
  | Connection_reset_by_peer
  | No_buffer_space_available
  | Transport_endpoint_already_connected
  | Transport_endpoint_not_connected
  | Cannot_send_after_transport_endpoint_shutdown
  | Too_many_references
  | Connection_timed_out
  | Connection_refused
  | Host_is_down
  | No_route_to_host
  | Operation_already_in_progress
  | Operation_now_in_progress
  | Unknown_error of string

type nonrec 'value io_result = ('value, error) result

type file_kind =
  | Regular
  | Directory
  | Symlink
  | Block
  | Character
  | Fifo
  | Socket

let of_system_error = function
  | Kernel.SystemError.EndOfFile -> End_of_file
  | Kernel.SystemError.PermissionDenied -> Permission_denied
  | Kernel.SystemError.NoSuchFileOrDirectory -> No_such_file_or_directory
  | Kernel.SystemError.Interrupted -> Interrupted_system_call
  | Kernel.SystemError.InputOutput -> Input_output_error
  | Kernel.SystemError.BadFileDescriptor -> Bad_file_descriptor
  | Kernel.SystemError.ResourceBusy -> Resource_busy
  | Kernel.SystemError.AlreadyExists -> File_exists
  | Kernel.SystemError.InvalidArgument -> Invalid_argument
  | Kernel.SystemError.NoSpaceLeft -> No_space_left_on_device
  | Kernel.SystemError.BrokenPipe -> Broken_pipe
  | Kernel.SystemError.WouldBlock -> Operation_would_block
  | Kernel.SystemError.NotDirectory -> Invalid_operation_on_device
  | Kernel.SystemError.IsDirectory -> Invalid_operation_on_device
  | Kernel.SystemError.NotSupported -> Operation_not_supported
  | Kernel.SystemError.AddressInUse -> Address_already_in_use
  | Kernel.SystemError.AddressNotAvailable -> Cannot_assign_requested_address
  | Kernel.SystemError.ConnectionRefused -> Connection_refused
  | Kernel.SystemError.ConnectionReset -> Connection_reset_by_peer
  | Kernel.SystemError.TimedOut -> Connection_timed_out
  | Kernel.SystemError.NetworkUnreachable -> Network_is_unreachable
  | Kernel.SystemError.DestinationAddressRequired -> Destination_address_required
  | Kernel.SystemError.NotConnected -> Transport_endpoint_not_connected
  | Kernel.SystemError.ConnectionAborted -> Software_caused_connection_abort
  | Kernel.SystemError.MessageTooLong -> Message_too_long
  | Kernel.SystemError.NoSuchProcess -> Process_down
  | Kernel.SystemError.DirectoryNotEmpty -> Directory_not_empty
  | Kernel.SystemError.Unknown code -> Unknown_error ("Unknown system error code "
  ^ Kernel.Int.to_string code)

let of_async_error = function
  | Kernel.Async.InvalidTimeoutNs _ -> Invalid_argument
  | Kernel.Async.InvalidMaxEvents _ -> Invalid_argument
  | Kernel.Async.System err -> of_system_error err

let error_message = function
  | End_of_file -> "End of file"
  | Timeout -> "Timeout"
  | Closed -> "Closed"
  | Connection_closed -> "Connection closed"
  | Process_down -> "Process down"
  | No_info -> "No info"
  | Noop -> "No operation"
  | Permission_denied -> "Permission denied"
  | No_such_file_or_directory -> "No such file or directory"
  | Interrupted_system_call -> "Interrupted system call"
  | Input_output_error -> "Input/output error"
  | Bad_file_descriptor -> "Bad file descriptor"
  | Resource_unavailable_try_again -> "Resource unavailable, try again"
  | Out_of_memory -> "Out of memory"
  | Permission_denied_on_file -> "Permission denied"
  | Bad_address -> "Bad address"
  | Resource_busy -> "Resource busy"
  | File_exists -> "File exists"
  | Cross_device_link -> "Cross-device link"
  | Invalid_argument -> "Invalid argument"
  | Too_many_open_files_in_system -> "Too many open files in system"
  | Too_many_open_files -> "Too many open files"
  | Invalid_operation_on_device -> "Invalid operation on device"
  | File_too_large -> "File too large"
  | No_space_left_on_device -> "No space left on device"
  | Illegal_seek -> "Illegal seek"
  | Read_only_filesystem -> "Read-only filesystem"
  | Too_many_links -> "Too many links"
  | Broken_pipe -> "Broken pipe"
  | Numerical_argument_out_of_domain -> "Numerical argument out of domain"
  | Numerical_result_out_of_range -> "Numerical result out of range"
  | Resource_deadlock_would_occur -> "Resource deadlock would occur"
  | Filename_too_long -> "Filename too long"
  | No_locks_available -> "No locks available"
  | Function_not_implemented -> "Function not implemented"
  | Directory_not_empty -> "Directory not empty"
  | Too_many_symbolic_links -> "Too many symbolic links"
  | Operation_would_block -> "Operation would block"
  | Socket_operation_on_non_socket -> "Socket operation on non-socket"
  | Destination_address_required -> "Destination address required"
  | Message_too_long -> "Message too long"
  | Protocol_wrong_type_for_socket -> "Protocol wrong type for socket"
  | Protocol_not_available -> "Protocol not available"
  | Protocol_not_supported -> "Protocol not supported"
  | Socket_type_not_supported -> "Socket type not supported"
  | Operation_not_supported -> "Operation not supported"
  | Protocol_family_not_supported -> "Protocol family not supported"
  | Address_family_not_supported -> "Address family not supported"
  | Address_already_in_use -> "Address already in use"
  | Cannot_assign_requested_address -> "Cannot assign requested address"
  | Network_is_down -> "Network is down"
  | Network_is_unreachable -> "Network is unreachable"
  | Network_dropped_connection_on_reset -> "Network dropped connection on reset"
  | Software_caused_connection_abort -> "Software caused connection abort"
  | Connection_reset_by_peer -> "Connection reset by peer"
  | No_buffer_space_available -> "No buffer space available"
  | Transport_endpoint_already_connected -> "Transport endpoint already connected"
  | Transport_endpoint_not_connected -> "Transport endpoint not connected"
  | Cannot_send_after_transport_endpoint_shutdown -> "Cannot send after transport endpoint shutdown"
  | Too_many_references -> "Too many references"
  | Connection_timed_out -> "Connection timed out"
  | Connection_refused -> "Connection refused"
  | Host_is_down -> "Host is down"
  | No_route_to_host -> "No route to host"
  | Operation_already_in_progress -> "Operation already in progress"
  | Operation_now_in_progress -> "Operation now in progress"
  | Unknown_error message -> message

module Stdin = struct
  module Runtime_actor = Runtime.Actor
  module Runtime_atomic = Kernel.Atomic

  type nonrec error = error
  type t = { pid: Runtime.Pid.t }

  type request_id = int

  type request =
    | Read of {
        reply_to: Runtime.Pid.t;
        request_id: request_id;
        buffer: Bytes.t;
        offset: int;
        len: int;
      }
    | Read_vectored of {
        reply_to: Runtime.Pid.t;
        request_id: request_id;
        bufs: Iovec.t;
      }
    | Read_line of {
        reply_to: Runtime.Pid.t;
        request_id: request_id;
      }
    | Read_to_string of {
        reply_to: Runtime.Pid.t;
        request_id: request_id;
        len: int;
      }

  type Runtime.Message.t +=
    | IO_stdin_request of request
    | IO_stdin_read_result of {
        request_id: request_id;
        count: int;
      }
    | IO_stdin_line_result of {
        request_id: request_id;
        line: string;
      }
    | IO_stdin_string_result of {
        request_id: request_id;
        data: string;
      }
    | IO_stdin_error of {
        request_id: request_id;
        error: error;
      }

  type state = {
    chunk_size: int;
    mutable leftover: Bytes.t option;
  }

  type copy_progress = { mutable copied: int }

  let default_chunk_size = 4_096

  let request_ids = Runtime_atomic.make 0

  let next_request_id = fun () ->
    Int.succ (Runtime_atomic.fetch_and_add request_ids 1)

  let normalize_chunk_size = fun chunk_size ->
    if chunk_size <= 0 then
      default_chunk_size
    else
      chunk_size

  let validate_slice = fun buffer ~offset ~len ->
    let buffer_len = Bytes.length buffer in
    if offset < 0 || offset > buffer_len || len < 0 || offset + len > buffer_len then
      Error Invalid_argument
    else
      Ok ()

  let store_leftover = fun state bytes ~offset ~len ->
    state.leftover <-
      if len <= 0 then
        None
      else
        Some (Bytes.sub_unchecked bytes ~offset ~len)

  let consume_leftover = fun state buffer ~offset ~len ->
    match state.leftover with
    | None -> 0
    | Some leftover ->
        let available = Bytes.length leftover in
        let copied = min len available in
        if copied > 0 then
          Bytes.blit_unchecked leftover ~src_offset:0 ~dst:buffer ~dst_offset:offset ~len:copied;
        store_leftover state leftover ~offset:copied ~len:(available - copied);
        copied

  let consume_leftover_vectored = fun state bufs ->
    match state.leftover with
    | None -> 0
    | Some leftover ->
        let available = Bytes.length leftover in
        let copied = { copied = 0 } in
        Iovec.for_each bufs
          ~fn:(fun { Kernel.IO.Iovec.buffer; offset; length } ->
            let remaining = available - copied.copied in
            if remaining > 0 then
              let chunk_len = min length remaining in
              Bytes.blit_unchecked
                leftover
                ~src_offset:copied.copied
                ~dst:buffer
                ~dst_offset:offset
                ~len:chunk_len;
              copied.copied <- copied.copied + chunk_len);
        store_leftover state leftover ~offset:copied.copied ~len:(available - copied.copied);
        copied.copied

  let read_kernel = fun buffer ~offset ~len ->
    match Kernel.IO.Stdin.read ~pos:offset ~len buffer with
    | Ok value -> Ok value
    | Error (Kernel.IO.Stdin.System error) -> Error (of_system_error error)
    | Error (Kernel.IO.Stdin.InvalidSlice _) -> Error Invalid_argument

  let read_kernel_vectored = fun bufs ->
    match Kernel.IO.Stdin.read_vectored bufs with
    | Ok value -> Ok value
    | Error (Kernel.IO.Stdin.System error) -> Error (of_system_error error)
    | Error (Kernel.IO.Stdin.InvalidSlice _) -> Error Invalid_argument

  let find_newline = fun bytes ~len ->
    let rec loop index =
      if index >= len then
        None
      else if Bytes.get_unchecked bytes ~at:index = '\n' then
        Some index
      else
        loop (index + 1)
    in
    loop 0

  let handle_read = fun state buffer ~offset ~len ->
    let copied = consume_leftover state buffer ~offset ~len in
    if copied > 0 then
      Ok copied
    else
      read_kernel buffer ~offset ~len

  let handle_read_vectored = fun state bufs ->
    let copied = consume_leftover_vectored state bufs in
    if copied > 0 then
      Ok copied
    else
      read_kernel_vectored bufs

  let handle_read_line = fun state ->
    let line = Buffer.create ~size:state.chunk_size in
    let chunk = Bytes.create ~size:state.chunk_size in
    let rec loop () =
      match state.leftover with
      | Some leftover -> (
          let available = Bytes.length leftover in
          match find_newline leftover ~len:available with
          | Some newline_index ->
              let line_len = newline_index + 1 in
              Buffer.add_subbytes line leftover 0 line_len;
              store_leftover state leftover ~offset:line_len ~len:(available - line_len);
              Ok (Buffer.contents line)
          | None ->
              Buffer.add_subbytes line leftover 0 available;
              state.leftover <- None;
              loop ()
        )
      | None -> (
          match read_kernel chunk ~offset:0 ~len:state.chunk_size with
          | Ok 0 -> Ok (Buffer.contents line)
          | Ok read_count ->
              state.leftover <- Some (Bytes.sub_unchecked chunk ~offset:0 ~len:read_count);
              loop ()
          | Error error -> Error error
        )
    in
    loop ()

  let handle_read_to_string = fun state ~len ->
    if len < 0 then
      Error Invalid_argument
    else if len = 0 then
      Ok ""
    else
      let data = Bytes.create ~size:len in
      let rec loop total =
        if total = len then
          Ok (Bytes.to_string data)
        else
          let copied = consume_leftover state data ~offset:total ~len:(len - total) in
          if copied > 0 then
            loop (total + copied)
          else
            match read_kernel data ~offset:total ~len:(len - total) with
            | Ok 0 -> Ok (Bytes.to_string (Bytes.sub_unchecked data ~offset:0 ~len:total))
            | Ok read_count -> loop (total + read_count)
            | Error error -> Error error
      in
      loop 0

  let send_count_result = fun reply_to request_id result ->
    match result with
    | Ok count -> Runtime.send reply_to (IO_stdin_read_result { request_id; count })
    | Error error -> Runtime.send reply_to (IO_stdin_error { request_id; error })

  let send_line_result = fun reply_to request_id result ->
    match result with
    | Ok line -> Runtime.send reply_to (IO_stdin_line_result { request_id; line })
    | Error error -> Runtime.send reply_to (IO_stdin_error { request_id; error })

  let send_string_result = fun reply_to request_id result ->
    match result with
    | Ok data -> Runtime.send reply_to (IO_stdin_string_result { request_id; data })
    | Error error -> Runtime.send reply_to (IO_stdin_error { request_id; error })

  let rec loop = fun state ->
    let selector msg =
      match msg with
      | IO_stdin_request request -> `select request
      | _ -> `skip
    in
    match Runtime.receive ~selector () with
    | Read { reply_to; request_id; buffer; offset; len } ->
        send_count_result reply_to request_id (handle_read state buffer ~offset ~len);
        loop state
    | Read_vectored { reply_to; request_id; bufs } ->
        send_count_result reply_to request_id (handle_read_vectored state bufs);
        loop state
    | Read_line { reply_to; request_id } ->
        send_line_result reply_to request_id (handle_read_line state);
        loop state
    | Read_to_string { reply_to; request_id; len } ->
        send_string_result reply_to request_id (handle_read_to_string state ~len);
        loop state

  let open_ = fun ?(chunk_size = default_chunk_size) () ->
    let chunk_size = normalize_chunk_size chunk_size in
    {
      pid = Runtime.spawn_blocked (fun () ->
        loop { chunk_size; leftover = None });
    }

  let await = fun t request_id ~selector ->
    let monitor = Runtime_actor.monitor t.pid in
    let receive_selector msg =
      match selector msg with
      | Some result -> `select result
      | None -> (
          match msg with
          | Runtime.Actor.DOWN { ref; pid; _ }
            when ref = monitor && Runtime.Pid.equal pid t.pid -> `select (Error Process_down)
          | _ -> `skip
        )
    in
    let result = Runtime.receive ~selector:receive_selector () in
    Runtime_actor.demonitor monitor;
    result

  let read = fun (t : t) ?(offset = 0) ?len buffer ->
    let len =
      match len with
      | Some len -> len
      | None -> Bytes.length buffer - offset
    in
    match validate_slice buffer ~offset ~len with
    | Error _ as error -> error
    | Ok () ->
        if len = 0 then
          Ok 0
        else
          let request_id = next_request_id () in
          Runtime.send
            t.pid
            (IO_stdin_request (Read { reply_to = Runtime.self (); request_id; buffer; offset; len }));
          await t request_id
            ~selector:(function
              | IO_stdin_read_result { request_id = got; count } when Int.equal got request_id ->
                  Some (Ok count)
              | IO_stdin_error { request_id = got; error } when Int.equal got request_id ->
                  Some (Error error)
              | _ -> None)

  let read_vectored = fun (t : t) bufs ->
    if Iovec.length bufs = 0 then
      Ok 0
    else
      let request_id = next_request_id () in
      Runtime.send
        t.pid
        (IO_stdin_request (Read_vectored { reply_to = Runtime.self (); request_id; bufs }));
      await t request_id
        ~selector:(function
          | IO_stdin_read_result { request_id = got; count } when Int.equal got request_id ->
              Some (Ok count)
          | IO_stdin_error { request_id = got; error } when Int.equal got request_id ->
              Some (Error error)
          | _ -> None)

  let read_line = fun (t : t) ->
    let request_id = next_request_id () in
    Runtime.send t.pid (IO_stdin_request (Read_line { reply_to = Runtime.self (); request_id }));
    await t request_id
      ~selector:(function
        | IO_stdin_line_result { request_id = got; line } when Int.equal got request_id ->
            Some (Ok line)
        | IO_stdin_error { request_id = got; error } when Int.equal got request_id ->
            Some (Error error)
        | _ -> None)

  let read_to_string = fun (t : t) ~len ->
    let request_id = next_request_id () in
    Runtime.send
      t.pid
      (IO_stdin_request (Read_to_string { reply_to = Runtime.self (); request_id; len }));
    await t request_id
      ~selector:(function
        | IO_stdin_string_result { request_id = got; data } when Int.equal got request_id ->
            Some (Ok data)
        | IO_stdin_error { request_id = got; error } when Int.equal got request_id ->
            Some (Error error)
        | _ -> None)

  let to_reader = fun stdin ->
    let read_bytes = read in
    let module Read = struct
      type nonrec t = t

      type err = error

      let read = fun stdin ?timeout:_ buf -> read_bytes stdin buf

      let read_vectored = fun stdin bufs -> read_vectored stdin bufs
    end in
    Reader.of_read_src (module Read) stdin
end

let stdin = Stdin.open_

module Stdout = struct
  type nonrec error = error

  let write = fun ?offset ?len buffer ->
    let source = Kernel.IO.Stdout.to_source () in
    let rec loop () =
      match Kernel.IO.Stdout.write ?pos:offset ?len buffer with
      | Ok value -> Ok value
      | Error (Kernel.IO.Stdout.System error) when Kernel.SystemError.would_block error -> Runtime.syscall
        ~name:"IO.Stdout.write"
        ~interest:Kernel.Async.Interest.writable
        ~source
        loop
      | Error (Kernel.IO.Stdout.System error) -> Error (of_system_error error)
      | Error (Kernel.IO.Stdout.InvalidSlice _) -> Error Invalid_argument
    in
    loop ()

  let write_vectored = fun bufs ->
    let source = Kernel.IO.Stdout.to_source () in
    let rec loop () =
      match Kernel.IO.Stdout.write_vectored bufs with
      | Ok value -> Ok value
      | Error (Kernel.IO.Stdout.System error) when Kernel.SystemError.would_block error -> Runtime.syscall
        ~name:"IO.Stdout.write_vectored"
        ~interest:Kernel.Async.Interest.writable
        ~source
        loop
      | Error (Kernel.IO.Stdout.System error) -> Error (of_system_error error)
      | Error (Kernel.IO.Stdout.InvalidSlice _) -> Error Invalid_argument
    in
    loop ()

  let flush = fun () ->
    let source = Kernel.IO.Stdout.to_source () in
    let rec loop () =
      match Kernel.IO.Stdout.flush () with
      | Ok () -> Ok ()
      | Error (Kernel.IO.Stdout.System error) when Kernel.SystemError.would_block error -> Runtime.syscall
        ~name:"IO.Stdout.flush"
        ~interest:Kernel.Async.Interest.writable
        ~source
        loop
      | Error (Kernel.IO.Stdout.System error) -> Error (of_system_error error)
      | Error (Kernel.IO.Stdout.InvalidSlice _) -> Error Invalid_argument
    in
    loop ()
end

module Stderr = struct
  type nonrec error = error

  let write = fun ?offset ?len buffer ->
    let source = Kernel.IO.Stderr.to_source () in
    let rec loop () =
      match Kernel.IO.Stderr.write ?pos:offset ?len buffer with
      | Ok value -> Ok value
      | Error (Kernel.IO.Stderr.System error) when Kernel.SystemError.would_block error -> Runtime.syscall
        ~name:"IO.Stderr.write"
        ~interest:Kernel.Async.Interest.writable
        ~source
        loop
      | Error (Kernel.IO.Stderr.System error) -> Error (of_system_error error)
      | Error (Kernel.IO.Stderr.InvalidSlice _) -> Error Invalid_argument
    in
    loop ()

  let write_vectored = fun bufs ->
    let source = Kernel.IO.Stderr.to_source () in
    let rec loop () =
      match Kernel.IO.Stderr.write_vectored bufs with
      | Ok value -> Ok value
      | Error (Kernel.IO.Stderr.System error) when Kernel.SystemError.would_block error -> Runtime.syscall
        ~name:"IO.Stderr.write_vectored"
        ~interest:Kernel.Async.Interest.writable
        ~source
        loop
      | Error (Kernel.IO.Stderr.System error) -> Error (of_system_error error)
      | Error (Kernel.IO.Stderr.InvalidSlice _) -> Error Invalid_argument
    in
    loop ()

  let flush = fun () ->
    let source = Kernel.IO.Stderr.to_source () in
    let rec loop () =
      match Kernel.IO.Stderr.flush () with
      | Ok () -> Ok ()
      | Error (Kernel.IO.Stderr.System error) when Kernel.SystemError.would_block error -> Runtime.syscall
        ~name:"IO.Stderr.flush"
        ~interest:Kernel.Async.Interest.writable
        ~source
        loop
      | Error (Kernel.IO.Stderr.System error) -> Error (of_system_error error)
      | Error (Kernel.IO.Stderr.InvalidSlice _) -> Error Invalid_argument
    in
    loop ()
end

let read = Reader.read

let read_vectored = Reader.read_vectored

let read_to_end = Reader.read_to_end

let write = Writer.write

let write_all = Writer.write_all

let write_owned_vectored = Writer.write_owned_vectored

let write_all_vectored = Writer.write_all_vectored

let flush = Writer.flush
