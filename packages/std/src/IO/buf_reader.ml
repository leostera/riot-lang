open Prelude
open Types

module IoVec = IoVec

module IoSlice = IoSlice

type 'value result = ('value, Error.t) Result.t

type copy_progress = { mutable copied: int }

type t = { mutable reader: Reader.t; buffer: Buffer.t; size: int; mutable eof: bool }

let default_size = 4_096

let normalize_size = fun size ->
  if size <= 0 then
    default_size
  else size

let panic_buffer_error = fun fn error -> Kernel.SystemError.panic ("IO.BufReader." ^ fn ^ ": " ^ Kernel.IO.Error.message error)

let compact = fun state -> Buffer.compact state.buffer

let fill_once = fun state ->
  if state.eof then
    Ok 0
  else
    (
      compact state;
      (
        if Buffer.writable_bytes state.buffer = 0 then
          match Buffer.ensure_free state.buffer state.size with
          | Ok () -> ()
          | Error error -> panic_buffer_error "fill.ensure_free" error
      );
      match Reader.read state.reader ~into:state.buffer with
      | Ok 0 ->
          state.eof <- true;
          Ok 0
      | Ok count -> Ok count
      | Error _ as error -> error
    )

let fill = fun state ->
  if Buffer.readable_bytes state.buffer > 0 then
    Ok (Buffer.readable_bytes state.buffer)
  else
    match fill_once state with
    | Ok 0 -> Error Error.End_of_file
    | Ok _ -> Ok (Buffer.readable_bytes state.buffer)
    | Error _ as error -> error

let buffered = fun state ->
  match fill state with
  | Ok _ -> Ok (Buffer.readable state.buffer)
  | Error _ as error -> error

let ensure_available = fun state needed ->
  if needed < 0 then
    Error Error.Invalid_argument
  else
    if needed = 0 then
      Ok ()
    else
      if needed > state.size then
        Error Error.Buffer_full
      else
        let rec loop () =
          if Buffer.readable_bytes state.buffer >= needed then
            Ok ()
          else
            if state.eof then
              Error Error.End_of_file
            else
              match fill_once state with
              | Ok 0 -> Error Error.End_of_file
              | Ok _ -> loop ()
              | Error _ as error -> error
        in
        loop ()

let from_reader = fun ?(size = default_size) reader ->
  let size = normalize_size size in
  {
    reader;
    buffer = Buffer.create ~size;
    size;
    eof = false
  }

let size = fun value -> value.size

let reset = fun value ~reader ->
  value.reader <- reader;
  value.eof <- false;
  Buffer.clear value.buffer

let peek = fun value ~len ->
  match ensure_available value len with
  | Ok () ->
      let readable = Buffer.readable value.buffer in
      Ok (
        if IoSlice.length readable > len then
          IoSlice.sub_unchecked readable ~off:0 ~len
        else readable
      )
  | Error _ as error -> error

let consume = fun value ~len ->
  if len < 0 then
    Error Error.Invalid_argument
  else
    let available = Buffer.readable_bytes value.buffer in
    let count = Int.min len available in
    match Buffer.consume value.buffer ~len:count with
    | Ok () -> Ok count
    | Error error -> panic_buffer_error "consume" error

let rec read = fun value ~into ->
  if Buffer.readable_bytes value.buffer > 0 then
    (
      let readable = Buffer.readable value.buffer in
      let writable =
        if Buffer.writable_bytes into = 0 then
          (
            match Buffer.ensure_free into value.size with
            | Ok () -> Buffer.writable into
            | Error error -> panic_buffer_error "read.ensure_free" error
          )
        else Buffer.writable into
      in
      let count = Int.min (IoSlice.length readable) (IoSlice.length writable) in
      match Buffer.append_subslice into readable ~off:0 ~len:count with
      | Ok () -> begin
        match Buffer.consume value.buffer ~len:count with
        | Ok () -> Ok count
        | Error error -> panic_buffer_error "read.consume" error
      end
      | Error error -> panic_buffer_error "read.append" error
    )
  else
    match fill_once value with
    | Ok 0 -> Error Error.End_of_file
    | Ok _ -> read value ~into
    | Error _ as error -> error

let read_byte = fun value ->
  match peek value ~len:1 with
  | Ok slice ->
      let byte = IoSlice.get_unchecked slice ~at:0 in
      begin
        match consume value ~len:1 with
        | Ok _ -> Ok byte
        | Error _ as error -> error
      end
  | Error _ as error -> error

let utf8_width = fun byte ->
  let code = Char.code byte in
  if code land 0x80 = 0 then
    1
  else
    if code land 0xe0 = 0xc0 then
      2
    else
      if code land 0xf0 = 0xe0 then
        3
      else
        if code land 0xf8 = 0xf0 then
          4
        else 0

let read_rune = fun value ->
  match peek value ~len:1 with
  | Error _ as error -> error
  | Ok first ->
      let width = utf8_width (IoSlice.get_unchecked first ~at:0) in
      if width = 0 then
        Error Error.Invalid_data
      else
        match peek value ~len:width with
        | Error _ as error -> error
        | Ok slice ->
            let encoded = IoSlice.to_string slice in
            begin
              match Unicode.Utf8.decode_rune encoded 0 with
              | Some (rune, len) -> begin
                match consume value ~len with
                | Ok _ -> Ok rune
                | Error _ as error -> error
              end
              | None -> Error Error.Invalid_data
            end

let read_slice = fun value ~until ->
  let rec loop () =
    let readable = Buffer.readable value.buffer in
    match IoSlice.index_char readable until with
    | Some index ->
        let slice = IoSlice.sub_unchecked readable ~off:0 ~len:(index + 1) in
        let _ =
          match Buffer.consume value.buffer ~len:(index + 1) with
          | Ok () -> ()
          | Error error -> panic_buffer_error "read_slice.consume" error
        in
        Ok slice
    | None ->
        if value.eof then
          if IoSlice.length readable = 0 then
            Error Error.End_of_file
          else
            let tail = readable in
            let _ =
              match Buffer.consume value.buffer ~len:(IoSlice.length tail) with
              | Ok () -> ()
              | Error error -> panic_buffer_error "read_slice.consume_tail" error
            in
            Ok tail
        else
          if Buffer.readable_bytes value.buffer >= value.size then
            Error Error.Buffer_full
          else
            match fill_once value with
            | Ok 0 -> loop ()
            | Ok _ -> loop ()
            | Error _ as error -> error
  in
  loop ()

let read_line = fun value -> read_slice value ~until:'\n'

let read_string = fun value ~until ->
  match read_slice value ~until with
  | Ok slice -> Ok (IoSlice.to_string slice)
  | Error _ as error -> error

let to_reader: t -> Reader.t = fun value ->
  let module Source = struct
    type nonrec t = t

    let read = fun source ~into ->
      match read source ~into with
      | Error Error.End_of_file -> Ok 0
      | Ok _ as ok -> ok
      | Error _ as error -> error

    let read_vectored = fun source ~into ->
      let tmp = Buffer.create ~size:(IoVec.length into) in
      match read source ~into:tmp with
      | Error Error.End_of_file -> Ok 0
      | Ok count ->
          let readable = Buffer.readable tmp in
          let progress = { copied = 0 } in
          IoVec.for_each into ~fn:(
            fun segment ->
              let remaining = count - progress.copied in
              if remaining > 0 then
                let len = Int.min remaining (IoSlice.length segment) in IoSlice.blit_unchecked ~src:readable ~src_off:progress.copied ~dst:segment ~dst_off:0 ~len;
              progress.copied <- progress.copied + len
          );
          Ok count
      | Error _ as error -> error

    let is_read_vectored = fun _ -> false
  end in
  Reader.from_source (module Source) value
