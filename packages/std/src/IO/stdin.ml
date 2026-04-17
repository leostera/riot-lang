open Prelude

module Buffer = Buffer
module Bytes = Bytes
module Error = Error
module Iovec = Kernel.IO.Iovec
module Reader = Reader
module Runtime_actor = Runtime.Actor
module Runtime_atomic = Kernel.Atomic

type error = Error.t
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
    Error Error.Invalid_argument
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
  | Error (Kernel.IO.Stdin.System error) -> Error (Error.of_system_error error)
  | Error (Kernel.IO.Stdin.InvalidSlice _) -> Error Error.Invalid_argument

let read_kernel_vectored = fun bufs ->
  match Kernel.IO.Stdin.read_vectored bufs with
  | Ok value -> Ok value
  | Error (Kernel.IO.Stdin.System error) -> Error (Error.of_system_error error)
  | Error (Kernel.IO.Stdin.InvalidSlice _) -> Error Error.Invalid_argument

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
    Error Error.Invalid_argument
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
          when ref = monitor && Runtime.Pid.equal pid t.pid -> `select (Error Error.Process_down)
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
  Reader.make
    ~read:(fun stdin ?timeout:_ buf -> read_bytes stdin buf)
    ~read_vectored:(fun stdin bufs -> read_vectored stdin bufs)
    ~read_line
    ~read_to_string
    stdin
