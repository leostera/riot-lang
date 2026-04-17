open Prelude
module String = Kernel.String
let panic = Kernel.SystemError.panic

module type Read = sig
  type t
  type err
  val read: t -> ?timeout:int64 -> bytes -> (int, err) result

  val read_vectored: t -> Iovec.t -> (int, err) result
end

type ('src, 'err) read = (module Read with type t = 'src and type err = 'err)

type ('src, 'err) ops = {
  read: 'src -> ?timeout:int64 -> bytes -> (int, 'err) result;
  read_vectored: 'src -> Iovec.t -> (int, 'err) result;
}

type ('src, 'err) t =
  | Reader of {
      ops: ('src, 'err) ops;
      src: 'src;
    }

type ('src, 'err) buffered = {
  reader: ('src, 'err) t;
  chunk: bytes;
  mutable offset: int;
  mutable length: int;
  mutable eof: bool;
}

type offset_state = {
  mutable offset: int;
}

type read_state = {
  mutable total: int;
  mutable continue: bool;
}

let default_chunk_size = 4_096

let normalize_chunk_size = fun chunk_size ->
  if chunk_size <= 0 then
    default_chunk_size
  else
    chunk_size

let default_read_char =
 fun
   (read : 'src -> ?timeout:int64 -> bytes -> (int, 'err) result)
   (src : 'src)
 ->
  let buf = Bytes.create ~size:1 in
  match read src buf with
  | Ok 0 -> Ok None
  | Ok _ -> Ok (Some (Bytes.get_unchecked buf ~at:0))
  | Error err -> Error err

let default_read_line =
 fun
   (read : 'src -> ?timeout:int64 -> bytes -> (int, 'err) result)
   (src : 'src)
 ->
  let buffer = Buffer.create ~size:128 in
  let rec loop () =
    match default_read_char read src with
    | Ok None -> Ok (Buffer.contents buffer)
    | Ok (Some char) ->
        Buffer.add_char buffer char;
        if char = '\n' then
          Ok (Buffer.contents buffer)
        else
          loop ()
    | Error err -> Error err
  in
  loop ()

let default_read_to_string =
 fun
   (read : 'src -> ?timeout:int64 -> bytes -> (int, 'err) result)
   (src : 'src)
   ~len
 ->
  if len < 0 then
    panic "Reader.read_to_string: negative length";
  if len = 0 then
    Ok ""
  else
    let out = Bytes.create ~size:len in
    let rec loop total =
      if total = len then
        Ok (Bytes.to_string out)
      else
        let remaining = len - total in
        let chunk = Bytes.create ~size:remaining in
        match read src chunk with
        | Ok 0 -> Ok (Bytes.to_string (Bytes.sub_unchecked out ~offset:0 ~len:total))
        | Ok count ->
            Bytes.blit_unchecked
              chunk
              ~src_offset:0
              ~dst:out
              ~dst_offset:total
              ~len:count;
            loop (total + count)
        | Error err -> Error err
    in
    loop 0

let make =
 fun
   ~(read : 'src -> ?timeout:int64 -> bytes -> (int, 'err) result)
   ~(read_vectored : 'src -> Iovec.t -> (int, 'err) result)
   (src : 'src)
 ->
  Reader { ops = { read; read_vectored }; src }

let of_read_src: type src err. (src, err) read -> src -> (src, err) t = fun (module R) src ->
  make
    ~read:(fun src ?timeout buf -> R.read src ?timeout buf)
    ~read_vectored:R.read_vectored
    src

let read: type src err. (src, err) t -> ?timeout:int64 -> bytes -> (int, err) result =
 fun (Reader { ops; src }) ?timeout buf -> ops.read src ?timeout buf

let read_vectored: type src err. (src, err) t -> Iovec.t -> (int, err) result =
 fun (Reader { ops; src }) bufs -> ops.read_vectored src bufs

let read_char: type src err. (src, err) t -> (char option, err) result =
 fun (Reader { ops; src }) -> default_read_char ops.read src

let read_line: type src err. (src, err) t -> (string, err) result =
 fun (Reader { ops; src }) -> default_read_line ops.read src

let read_to_string: type src err. (src, err) t -> len:int -> (string, err) result =
 fun (Reader { ops; src }) ~len -> default_read_to_string ops.read src ~len

let buffered_available = fun state -> state.length - state.offset

let buffered_consume = fun state buffer ~dst_offset ~len ->
  let available = buffered_available state in
  let copied = min len available in
  if copied > 0 then
    Bytes.blit_unchecked state.chunk ~src_offset:state.offset ~dst:buffer ~dst_offset ~len:copied;
  state.offset <- state.offset + copied;
  copied

let buffered_consume_vectored = fun state bufs ->
  let progress: read_state = { total = 0; continue = true } in
  Iovec.for_each bufs
    ~fn:(fun { Iovec.buffer; offset; length } ->
      let available = buffered_available state in
      if available > 0 then
        let chunk_len = min length available in
        Bytes.blit_unchecked
          state.chunk
          ~src_offset:state.offset
          ~dst:buffer
          ~dst_offset:offset
          ~len:chunk_len;
        state.offset <- state.offset + chunk_len;
        progress.total <- progress.total + chunk_len);
  progress.total

let buffered_refill = fun state ?timeout () ->
  if buffered_available state > 0 then
    Ok (buffered_available state)
  else if state.eof then
    Ok 0
  else
    match read state.reader ?timeout state.chunk with
    | Ok 0 ->
        state.offset <- 0;
        state.length <- 0;
        state.eof <- true;
        Ok 0
    | Ok count ->
        state.offset <- 0;
        state.length <- count;
        Ok count
    | Error err -> Error err

let buffered_read_raw = fun state ?timeout buffer ->
  let requested = Bytes.length buffer in
  if requested = 0 then
    Ok 0
  else
    let copied = buffered_consume state buffer ~dst_offset:0 ~len:requested in
    if copied > 0 then
      Ok copied
    else if requested >= Bytes.length state.chunk then
      read state.reader ?timeout buffer
    else
      match buffered_refill state ?timeout () with
      | Ok available ->
          if available = 0 then
            Ok 0
          else
            Ok (buffered_consume state buffer ~dst_offset:0 ~len:requested)
      | Error err -> Error err

let buffered_read_vectored_raw = fun state bufs ->
  if Iovec.length bufs = 0 then
    Ok 0
  else
    let copied = buffered_consume_vectored state bufs in
    if copied > 0 then
      Ok copied
    else if Iovec.length bufs >= Bytes.length state.chunk then
      read_vectored state.reader bufs
    else
      match buffered_refill state () with
      | Ok available ->
          if available = 0 then
            Ok 0
          else
            Ok (buffered_consume_vectored state bufs)
      | Error err -> Error err

let buffered = fun ?(chunk_size = default_chunk_size) reader ->
  let state =
    {
      reader;
      chunk = Bytes.create ~size:(normalize_chunk_size chunk_size);
      offset = 0;
      length = 0;
      eof = false;
    }
  in
  make
    ~read:(fun state ?timeout buffer -> buffered_read_raw state ?timeout buffer)
    ~read_vectored:buffered_read_vectored_raw
    state

let read_to_end: type src err. (src, err) t -> buf:Buffer.t -> (int, err) result =
 fun reader ~buf:out ->
  let chunk = Bytes.create ~size:1_024 in
  let rec loop total =
    match read reader chunk with
    | Ok 0 ->
        Ok total
    | Ok len ->
        Buffer.add_subbytes out chunk 0 len;
        loop (total + len)
    | Error err ->
        Error err
  in
  loop 0

let map_err: type src a b. (src, a) t -> fn:(a -> b) -> (src, b) t =
 fun (Reader { ops; src }) ~fn ->
  let ops =
    {
      read = (fun value ?timeout buf ->
        match ops.read value ?timeout buf with
        | Ok read -> Ok read
        | Error err -> Error (fn err));
      read_vectored = (fun value bufs ->
        match ops.read_vectored value bufs with
        | Ok read -> Ok read
        | Error err -> Error (fn err));
    }
  in
  Reader { ops; src }

let empty =
  let module Empty = struct
    type t = unit
    type err = unit
    let read = fun () ?timeout:_ _ -> Ok 0
    let read_vectored = fun () _ -> Ok 0
  end in
  of_read_src (module Empty) ()

let from_bytes = fun data ->
  let state = { offset = 0 } in
  let module Bytes_read = struct
    type t = bytes
    type err = unit

    let read = fun source ?timeout:_ buf ->
      let remaining = Bytes.length source - state.offset in
      if remaining = 0 then
        Ok 0
      else
        let to_read = min (Bytes.length buf) remaining in
        Bytes.blit_unchecked source ~src_offset:state.offset ~dst:buf ~dst_offset:0 ~len:to_read;
        state.offset <- state.offset + to_read;
        Ok to_read

    let read_vectored = fun source iov ->
      let progress = { total = 0; continue = true } in
      Iovec.for_each iov
        ~fn:(fun { Iovec.buffer; offset; length } ->
          if progress.continue then
            let remaining = Bytes.length source - state.offset in
            if remaining = 0 then
              progress.continue <- false
            else
              let to_read = min length remaining in
              Bytes.blit_unchecked
                source
                ~src_offset:state.offset
                ~dst:buffer
                ~dst_offset:offset
                ~len:to_read;
              state.offset <- state.offset + to_read;
              progress.total <- progress.total + to_read);
      Ok progress.total
  end in
  of_read_src (module Bytes_read) data

let from_string = fun source ->
  let state = { offset = 0 } in
  let module String_read = struct
    type t = string
    type err = unit

    let read = fun value ?timeout:_ buf ->
      let remaining = String.length value - state.offset in
      if remaining = 0 then
        Ok 0
      else
        let to_read = min (Bytes.length buf) remaining in
        Bytes.blit_string value ~src_offset:state.offset ~dst:buf ~dst_offset:0 ~len:to_read;
        state.offset <- state.offset + to_read;
        Ok to_read

    let read_vectored = fun value iov ->
      let progress = { total = 0; continue = true } in
      Iovec.for_each iov
        ~fn:(fun { Iovec.buffer; offset; length } ->
          if progress.continue then
            let remaining = String.length value - state.offset in
            if remaining = 0 then
              progress.continue <- false
            else
              let to_read = min length remaining in
              Bytes.blit_string value ~src_offset:state.offset ~dst:buffer ~dst_offset:offset ~len:to_read;
              state.offset <- state.offset + to_read;
              progress.total <- progress.total + to_read);
      Ok progress.total
  end in
  of_read_src (module String_read) source
