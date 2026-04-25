open Prelude
open Types

module IoVec = IoVec

module IoSlice = IoSlice

type 'value result = ('value, Error.t) Result.t

let default_chunk_size = 4_096

let panic_buffer_error = fun fn error -> Kernel.SystemError.panic ("IO.Reader." ^ fn ^ ": " ^ Kernel.IO.Error.message error)

module type Read = sig
  type t

  val read: t -> into:Buffer.t -> int result

  val read_vectored: t -> into:IoVec.t -> int result

  val is_read_vectored: t -> bool
end

type 'src source = (module Read with type t = 'src)

type t =
  | Reader : ('src source * 'src) -> t

type byte_result = u8 result

type chain_state = { mutable current_left: bool; left: t; right: t }

type take_state = { reader: t; mutable remaining: int }

let ensure_writable = fun into needed ->
  if Buffer.writable_bytes into = 0 then
    match Buffer.ensure_free into (Int.max needed default_chunk_size) with
    | Ok () -> ()
    | Error error -> panic_buffer_error "ensure_writable" error

let commit_into = fun into count ->
  match Buffer.commit into count with
  | Ok () -> ()
  | Error error -> panic_buffer_error "commit" error

let limited_writable = fun into limit ->
  ensure_writable into limit;
  let writable = Buffer.writable into in
  if IoSlice.length writable > limit then
    IoSlice.sub_unchecked writable ~off:0 ~len:limit
  else writable

let from_source = fun source src -> Reader (source, src)

let read: t -> into:Buffer.t -> int result = fun (Reader (((module Source) as source), src)) ~into ->
  let _ = source in Source.read src ~into

let read_vectored: t -> into:IoVec.t -> int result = fun (Reader (((module Source) as source), src)) ~into ->
  let _ = source in Source.read_vectored src ~into

let is_read_vectored: t -> bool = fun (Reader (((module Source) as source), src)) ->
  let _ = source in Source.is_read_vectored src

let read_to_end = fun reader ~into ->
  let rec loop total =
    match read reader ~into with
    | Ok 0 -> Ok total
    | Ok count -> loop (total + count)
    | Error err -> Error err
  in
  loop 0

let read_to_string = fun reader ~into ->
  let scratch = Buffer.create ~size:default_chunk_size in
  let rec loop total =
    Buffer.clear scratch;
    match read reader ~into:scratch with
    | Ok 0 -> Ok total
    | Ok count ->
        StringBuilder.add_string into (Buffer.to_string scratch);
        loop (total + count)
    | Error err -> Error err
  in
  loop 0

let read_exact = fun reader ~into ~len ->
  if len < 0 then
    Error Error.Invalid_argument
  else
    let rec loop remaining =
      if remaining = 0 then
        Ok ()
      else
        let writable = limited_writable into remaining in
        let bufs = IoVec.from_slices [|writable|] in
        match read_vectored reader ~into:bufs with
        | Ok 0 -> Error Error.Unexpected_end_of_file
        | Ok count ->
            commit_into into count;
            loop (remaining - count)
        | Error _ as error -> error
    in
    loop len

let bytes: t -> byte_result Iter.Iterator.t = fun reader ->
  let module ByteIter = struct
    type state = { reader: t; done_: bool }

    type item = byte_result

    let next = fun state ->
      if state.done_ then
        (None, state)
      else
        let scratch = Buffer.create ~size:1 in
        match read_exact state.reader ~into:scratch ~len:1 with
        | Ok () ->
            let byte = Buffer.get_unchecked scratch ~at:0 in (Some (Ok byte), state)
        | Error Error.Unexpected_end_of_file -> None, { state with done_ = true }
        | Error err -> Some (Error err), { state with done_ = true }

    let size = fun _ -> 0
  end in
  Iter.Iterator.make (module ByteIter) { reader; done_ = false }

let chain: t -> t -> t = fun left right ->
  let reader_read = read in
  let reader_read_vectored = read_vectored in
  let reader_is_read_vectored = is_read_vectored in
  let module Chain = struct
    type t = chain_state

    let rec read = fun state ~into ->
      if state.current_left then
        match reader_read state.left ~into with
        | Ok 0 ->
            state.current_left <- false;
            read state ~into
        | Ok _ as ok -> ok
        | Error _ as error -> error
      else reader_read state.right ~into

    let rec read_vectored = fun state ~into ->
      if state.current_left then
        match reader_read_vectored state.left ~into with
        | Ok 0 ->
            state.current_left <- false;
            read_vectored state ~into
        | Ok _ as ok -> ok
        | Error _ as error -> error
      else reader_read_vectored state.right ~into

    let is_read_vectored = fun state ->
      if state.current_left then
        reader_is_read_vectored state.left
      else reader_is_read_vectored state.right
  end in
  from_source (module Chain) { current_left = true; left; right }

let take: t -> limit:int -> t = fun reader ~limit ->
  let reader_read_vectored = read_vectored in
  let reader_is_read_vectored = is_read_vectored in
  let module Take = struct
    type t = take_state

    let read = fun state ~into ->
      if state.remaining <= 0 then
        Ok 0
      else
        let writable = limited_writable into state.remaining in
        let bufs = IoVec.from_slices [|writable|] in
        match reader_read_vectored state.reader ~into:bufs with
        | Ok count ->
            commit_into into count;
            state.remaining <- state.remaining - count;
            Ok count
        | Error _ as error -> error

    let read_vectored = fun state ~into ->
      if state.remaining <= 0 then
        Ok 0
      else
        match IoVec.sub ~len:(Int.min state.remaining (IoVec.length into)) into with
        | Ok limited -> begin
          match reader_read_vectored state.reader ~into:limited with
          | Ok count ->
              state.remaining <- state.remaining - count;
              Ok count
          | Error _ as error -> error
        end
        | Error error -> Kernel.SystemError.panic ("IO.Reader.take.read_vectored: " ^ Kernel.IO.Error.message error)

    let is_read_vectored = fun state -> reader_is_read_vectored state.reader
  end in
  from_source (module Take) { reader; remaining = Int.max 0 limit }

let empty =
  let module Empty = struct
    type t = unit

    let read = fun () ~into:_ -> Ok 0

    let read_vectored = fun () ~into:_ -> Ok 0

    let is_read_vectored = fun () -> true
  end in
  from_source (module Empty) ()

module BytesSource = struct
  type t = { mutable offset: int; source: bytes }

  type copy_progress = { mutable copied: int }

  let copy_into_iovec = fun state ~source into ->
    let available = Bytes.length source - state.offset in
    let progress = { copied = 0 } in
    IoVec.for_each into ~fn:(
      fun segment ->
        let remaining = available - progress.copied in
        if remaining > 0 then
          let len = Int.min remaining (IoSlice.length segment) in IoSlice.blit_from_bytes_unchecked source ~src_off:(state.offset + progress.copied) segment ~dst_off:0 ~len;
        progress.copied <- progress.copied + len
    );
    state.offset <- state.offset + progress.copied;
    progress.copied

  let read = fun state ~into ->
    let remaining = Bytes.length state.source - state.offset in
    if remaining = 0 then
      Ok 0
    else
      let writable = limited_writable into remaining in
      let count = Int.min remaining (IoSlice.length writable) in IoSlice.blit_from_bytes_unchecked state.source ~src_off:state.offset writable ~dst_off:0 ~len:count;
    commit_into into count;
    state.offset <- state.offset + count;
    Ok count

  let read_vectored = fun state ~into -> Ok (copy_into_iovec state ~source:state.source into)

  let is_read_vectored = fun _ -> false
end

let from_bytes = fun source -> from_source (module BytesSource) BytesSource.{ offset = 0; source }

let from_string = fun source -> from_bytes (Bytes.from_string source)
