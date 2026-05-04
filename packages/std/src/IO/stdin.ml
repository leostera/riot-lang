open Prelude

module Buffer = Buffer
module Bytes = Bytes
module Error = Error
module IoVec = IoVec
module Reader = Reader
module Runtime_actor = Runtime.Actor
module Runtime_atomic = Kernel.Atomic

type error = Error.t

type 'value result = ('value, error) Result.t

type t = {
  pid: Runtime.Pid.t;
}

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
      bufs: IoVec.t;
    }

type Runtime.Message.t +=
  | IO_stdin_request of request
  | IO_stdin_read_result of {
      request_id: request_id;
      count: int;
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

let next_request_id = fun () -> Int.succ (Runtime_atomic.fetch_and_add request_ids 1)

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
  state.leftover <- if len <= 0 then
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
      IoVec.for_each
        bufs
        ~fn:(fun segment ->
          let remaining = available - copied.copied in
          if remaining > 0 then
            let length = IoVec.IoSlice.length segment in
            let chunk_len = min length remaining in
            IoVec.IoSlice.blit_from_bytes_unchecked
              leftover
              ~src_off:copied.copied
              segment
              ~dst_off:0
              ~len:chunk_len;
          copied.copied <- copied.copied + chunk_len);
      store_leftover state leftover ~offset:copied.copied ~len:(available - copied.copied);
      copied.copied

let read_kernel = fun buffer ~offset ~len ->
  match Kernel.IO.Stdin.read ~pos:offset ~len buffer with
  | Ok value -> Ok value
  | Error (Kernel.IO.Stdin.System error) -> Error (Error.from_system_error error)
  | Error (Kernel.IO.Stdin.InvalidSlice _) -> Error Error.Invalid_argument

let read_kernel_vectored = fun bufs ->
  match Kernel.IO.Stdin.read_vectored bufs with
  | Ok value -> Ok value
  | Error (Kernel.IO.Stdin.System error) -> Error (Error.from_system_error error)
  | Error (Kernel.IO.Stdin.InvalidSlice _) -> Error Error.Invalid_argument

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

let send_count_result = fun reply_to request_id result ->
  match result with
  | Ok count -> Runtime.send reply_to (IO_stdin_read_result { request_id; count })
  | Error error -> Runtime.send reply_to (IO_stdin_error { request_id; error })

let rec loop = fun state ->
  let selector msg =
    match msg with
    | IO_stdin_request request -> Runtime.Select request
    | _ -> Runtime.Skip
  in
  match Runtime.receive ~selector () with
  | Read {
      reply_to;
      request_id;
      buffer;
      offset;
      len;
    } ->
      send_count_result
        reply_to
        request_id
        (handle_read state buffer ~offset ~len);
      loop state
  | Read_vectored { reply_to; request_id; bufs } ->
      send_count_result
        reply_to
        request_id
        (handle_read_vectored state bufs);
      loop state

let open_ = fun ?(chunk_size = default_chunk_size) () ->
  let chunk_size = normalize_chunk_size chunk_size in
  {
    pid = Runtime.spawn_blocked (fun () -> loop { chunk_size; leftover = None });
  }

let await = fun t request_id ~selector ->
  let monitor = Runtime_actor.monitor t.pid in
  let receive_selector msg =
    match selector msg with
    | Some result -> Runtime.Select result
    | None -> (
        match msg with
        | Runtime.Actor.DOWN { ref; pid; _ } when ref = monitor && Runtime.Pid.equal pid t.pid ->
            Runtime.Select (Error Error.Process_down)
        | _ -> Runtime.Skip
      )
  in
  let result = Runtime.receive ~selector:receive_selector () in
  Runtime_actor.demonitor monitor;
  result

let read_bytes = fun (t: t) ?(offset = 0) ?len buffer ->
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
          (
            IO_stdin_request (
              Read {
                reply_to = Runtime.self ();
                request_id;
                buffer;
                offset;
                len;
              }
            )
          );
      await
        t
        request_id
        ~selector:(fun __tmp1 ->
          match __tmp1 with
          | IO_stdin_read_result { request_id = got; count } when Int.equal got request_id ->
              Some (Ok count)
          | IO_stdin_error { request_id = got; error } when Int.equal got request_id ->
              Some (Error error)
          | _ -> None)

let read_vectored = fun (t: t) ~into ->
  if IoVec.length into = 0 then
    Ok 0
  else
    let request_id = next_request_id () in
    Runtime.send
      t.pid
      (IO_stdin_request (Read_vectored { reply_to = Runtime.self (); request_id; bufs = into }));
  await
    t
    request_id
    ~selector:(fun __tmp1 ->
      match __tmp1 with
      | IO_stdin_read_result { request_id = got; count } when Int.equal got request_id ->
          Some (Ok count)
      | IO_stdin_error { request_id = got; error } when Int.equal got request_id ->
          Some (Error error)
      | _ -> None)

let commit_into = fun into count ->
  match Buffer.commit into count with
  | Ok () -> Ok count
  | Error error -> Kernel.SystemError.panic ("IO.Stdin.read: " ^ Kernel.IO.Error.message error)

let read = fun (t: t) ~into ->
  if Buffer.writable_bytes into = 0 then (
    match Buffer.ensure_free into default_chunk_size with
    | Ok () -> ()
    | Error error -> Kernel.SystemError.panic ("IO.Stdin.read: " ^ Kernel.IO.Error.message error)
  );
  let writable = Buffer.writable into in
  let bufs = IoVec.from_slices [|writable|] in
  match read_vectored t ~into:bufs with
  | Ok count -> commit_into into count
  | Error _ as error -> error

let to_reader = fun stdin ->
  let module Source = struct
    type nonrec t = t

    let read = fun stdin ~into -> read stdin ~into

    let read_vectored = fun stdin ~into -> read_vectored stdin ~into

    let is_read_vectored = fun _ -> true
  end in
  Reader.from_source (module Source) stdin
