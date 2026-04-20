open Prelude

module IoSlice = Kernel.IO.Iovec.IoSlice

type ('src, 'err) t = {
  reader: ('src, 'err) Reader.t;
  buffer: Buffer.t;
  chunk_size: int;
  mutable eof: bool;
}

type copy_progress = {
  mutable copied: int;
}

let default_chunk_size = 4_096

let normalize_chunk_size = fun chunk_size ->
  if chunk_size <= 0 then
    default_chunk_size
  else
    chunk_size

let panic_buffer_error = fun fn error ->
  Kernel.SystemError.panic ("IO.BufferedReader." ^ fn ^ ": " ^ Kernel.IO.Error.message error)

let ensure_buffer_free = fun state needed ->
  match Buffer.ensure_free state.buffer needed with
  | Ok () ->
      ()
  | Error error ->
      panic_buffer_error "ensure_free" error

let commit_buffer = fun state len ->
  match Buffer.commit state.buffer len with
  | Ok () ->
      ()
  | Error error ->
      panic_buffer_error "commit" error

let consume_buffer = fun state len ->
  match Buffer.consume state.buffer ~len with
  | Ok () ->
      ()
  | Error error ->
      panic_buffer_error "consume" error

let fill_once = fun state ?timeout () ->
  if state.eof then
    Ok 0
  else (
    ensure_buffer_free state state.chunk_size;
    let writable = Buffer.writable state.buffer in
    let writable =
      if IoSlice.length writable > state.chunk_size then
        IoSlice.sub_unchecked writable ~off:0 ~len:state.chunk_size
      else
        writable
    in
    let bufs = Iovec.from_slices [| writable |] in
    match Reader.read_vectored state.reader bufs with
    | Ok 0 ->
        state.eof <- true;
        Ok 0
    | Ok count ->
        commit_buffer state count;
        Ok count
    | Error err ->
        Error err
  )

let refill = fun state ?timeout () ->
  if Buffer.readable_bytes state.buffer > 0 then
    Ok (Buffer.readable_bytes state.buffer)
  else if state.eof then
    Ok 0
  else
    fill_once state ?timeout ()

let peek_slice = fun state ->
  match refill state () with
  | Ok 0 ->
      Ok None
  | Ok _ ->
      Ok (Some (Buffer.readable state.buffer))
  | Error err ->
      Error err

let consume = fun state ~len -> consume_buffer state len

let rec read_slice = fun state ~delim ->
  let readable = Buffer.readable state.buffer in
  match IoSlice.index_char readable delim with
  | Some index ->
      let taken = IoSlice.sub_unchecked readable ~off:0 ~len:(index + 1) in
      consume_buffer state (index + 1);
      Ok (Some taken)
  | None ->
      if state.eof then
        if IoSlice.length readable = 0 then
          Ok None
        else (
          let taken = readable in
          consume_buffer state (IoSlice.length taken);
          Ok (Some taken)
        )
      else
        match fill_once state () with
        | Ok 0 ->
            read_slice state ~delim
        | Ok _ ->
            read_slice state ~delim
        | Error err ->
            Error err

let read_line_slice = fun state -> read_slice state ~delim:'\n'

let rec raw_read = fun state ?timeout buffer ->
  let requested = Bytes.length buffer in
  if requested = 0 then
    Ok 0
  else if Buffer.readable_bytes state.buffer > 0 then (
    let readable = Buffer.readable state.buffer in
    let copied = min requested (IoSlice.length readable) in
    IoSlice.blit_to_bytes_unchecked readable ~src_off:0 buffer ~dst_off:0 ~len:copied;
    consume_buffer state copied;
    Ok copied
  ) else if requested >= state.chunk_size then
    Reader.read state.reader ?timeout buffer
  else
    match refill state ?timeout () with
    | Ok 0 ->
        Ok 0
    | Ok _ ->
        raw_read state ?timeout buffer
    | Error err ->
        Error err

and raw_read_vectored = fun state bufs ->
  if Iovec.length bufs = 0 then
    Ok 0
  else if Buffer.readable_bytes state.buffer > 0 then (
    let readable = Buffer.readable state.buffer in
    let progress = { copied = 0 } in
    Iovec.for_each bufs
      ~fn:(fun segment ->
        let remaining = IoSlice.length readable - progress.copied in
        if remaining > 0 then
          let len = min remaining (IoSlice.length segment) in
          IoSlice.blit_unchecked ~src:readable ~src_off:progress.copied ~dst:segment ~dst_off:0 ~len;
          progress.copied <- progress.copied + len);
    consume_buffer state progress.copied;
    Ok progress.copied
  ) else if Iovec.length bufs >= state.chunk_size then
    Reader.read_vectored state.reader bufs
  else
    match refill state () with
    | Ok 0 ->
        Ok 0
    | Ok _ ->
        raw_read_vectored state bufs
    | Error err ->
        Error err

let of_reader = fun ?(chunk_size = default_chunk_size) reader ->
  let chunk_size = normalize_chunk_size chunk_size in
  { reader; buffer = Buffer.create ~size:chunk_size; chunk_size; eof = false }

let to_reader value =
  Reader.make
    ~read:(fun state ?timeout buffer -> raw_read state ?timeout buffer)
    ~read_vectored:raw_read_vectored
    value

let read = fun value ?timeout buffer -> Reader.read (to_reader value) ?timeout buffer

let read_vectored = fun value bufs -> Reader.read_vectored (to_reader value) bufs

let read_char = fun value -> Reader.read_char (to_reader value)

let read_line = fun value ->
  match read_line_slice value with
  | Ok None ->
      Ok ""
  | Ok (Some slice) ->
      Ok (IoSlice.to_string slice)
  | Error err ->
      Error err

let read_to_string = fun value ~len -> Reader.read_to_string (to_reader value) ~len
