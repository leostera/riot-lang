open Prelude

module KernelBuffer = Kernel.IO.Buffer
module IoVec = IoVec
module IoSlice = IoSlice

type t = KernelBuffer.t
type error = Kernel.IO.Error.t

let panic_result = fun fn -> function
  | Ok value -> value
  | Error error -> Kernel.SystemError.panic ("IO.Buffer." ^ fn ^ ": " ^ Kernel.IO.Error.message error)

let panic_invalid_range = fun fn ~offset ~length ~total ->
  Kernel.SystemError.panic
    (Kernel.String.concat
      ""
      [
        "IO.Buffer.";
        fn;
        " received an invalid range: offset=";
        Kernel.Int.to_string offset;
        " length=";
        Kernel.Int.to_string length;
        " total=";
        Kernel.Int.to_string total;
      ])

let create = fun ~size ->
  let size = Kernel.Int.max 0 size in
  panic_result "create" (KernelBuffer.create ~size ())

let from_string = fun source ->
  let buffer = create ~size:(Kernel.String.length source) in
  let _ = KernelBuffer.append_string buffer source |> panic_result "from_string" in
  buffer

let from_bytes = fun source ->
  let buffer = create ~size:(Kernel.Bytes.length source) in
  let _ = KernelBuffer.append_bytes buffer source |> panic_result "from_bytes" in
  buffer

let from_slice = fun source ->
  let buffer = create ~size:(IoSlice.length source) in
  let _ = KernelBuffer.append_slice buffer source |> panic_result "from_slice" in
  buffer

let create_result = KernelBuffer.create

let length = KernelBuffer.length

let readable_bytes = KernelBuffer.readable_bytes

let capacity = KernelBuffer.capacity

let writable_bytes = KernelBuffer.writable_bytes

let clear = KernelBuffer.clear

let compact = KernelBuffer.compact

let ensure_free = KernelBuffer.ensure_free

let readable = KernelBuffer.readable

let writable = KernelBuffer.writable

let commit = KernelBuffer.commit

let consume = KernelBuffer.consume

let append_string = KernelBuffer.append_string

let append_bytes = KernelBuffer.append_bytes

let append_slice = KernelBuffer.append_slice

let to_iovec = KernelBuffer.to_iovec

let to_bytes = KernelBuffer.to_bytes

let to_string = KernelBuffer.to_string

let contents = to_string

let get = fun buffer ~at ->
  if at < 0 || at >= length buffer then
    None
  else
    Some (IoSlice.get_unchecked (readable buffer) ~at)

let get_unchecked = fun buffer ~at ->
  readable buffer |> IoSlice.get_unchecked ~at

let add_char = fun buffer value ->
  let _ = ensure_free buffer 1 |> panic_result "add_char.ensure_free" in
  let dst = writable buffer in
  IoSlice.set_unchecked dst ~at:0 value;
  let _ = commit buffer 1 |> panic_result "add_char.commit" in
  ()

let add_string = fun buffer source ->
  let _ = append_string buffer source |> panic_result "add_string" in
  ()

let add_bytes = fun buffer source ->
  let _ = append_bytes buffer source |> panic_result "add_bytes" in
  ()

let add_subbytes = fun buffer source offset slice_length ->
  let source_length = Kernel.Bytes.length source in
  if offset < 0 || slice_length < 0 || offset > source_length - slice_length then
    panic_invalid_range "add_subbytes" ~offset ~length:slice_length ~total:source_length
  else if slice_length = 0 then
    ()
  else (
    let _ = ensure_free buffer slice_length |> panic_result "add_subbytes.ensure_free" in
    let dst = writable buffer in
    IoSlice.blit_from_bytes_unchecked source ~src_off:offset dst ~dst_off:0 ~len:slice_length;
    let _ = commit buffer slice_length |> panic_result "add_subbytes.commit" in
    ()
  )

let add_substring = fun buffer source offset slice_length ->
  let source_length = Kernel.String.length source in
  if offset < 0 || slice_length < 0 || offset > source_length - slice_length then
    panic_invalid_range "add_substring" ~offset ~length:slice_length ~total:source_length
  else if slice_length = 0 then
    ()
  else (
    let _ = ensure_free buffer slice_length |> panic_result "add_substring.ensure_free" in
    let dst = writable buffer in
    IoSlice.blit_from_string_unchecked source ~src_off:offset dst ~dst_off:0 ~len:slice_length;
    let _ = commit buffer slice_length |> panic_result "add_substring.commit" in
    ()
  )

let add_utf_8_uchar = fun buffer rune -> add_string buffer (Kernel.Unicode.Rune.to_string rune)
