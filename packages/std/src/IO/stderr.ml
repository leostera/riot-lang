open Prelude

module Bytes = Bytes
module Error = Error
module Iovec = Kernel.IO.Iovec
module Writer = Writer

type t = unit
type error = Error.t

let write = fun ?offset ?len buffer ->
  let source = Kernel.IO.Stderr.to_source () in
  let rec loop () =
    match Kernel.IO.Stderr.write ?pos:offset ?len buffer with
    | Ok value -> Ok value
    | Error (Kernel.IO.Stderr.System error) when Kernel.SystemError.would_block error ->
        Runtime.syscall
          ~name:"IO.Stderr.write"
          ~interest:Kernel.Async.Interest.writable
          ~source
          loop
    | Error (Kernel.IO.Stderr.System error) -> Error (Error.of_system_error error)
    | Error (Kernel.IO.Stderr.InvalidSlice _) -> Error Error.Invalid_argument
  in
  loop ()

let write_vectored = fun bufs ->
  if Iovec.length bufs = 0 then
    Ok 0
  else
    let source = Kernel.IO.Stderr.to_source () in
    let rec loop () =
      match Kernel.IO.Stderr.write_vectored bufs with
      | Ok value -> Ok value
      | Error (Kernel.IO.Stderr.System error) when Kernel.SystemError.would_block error ->
          Runtime.syscall
            ~name:"IO.Stderr.write_vectored"
            ~interest:Kernel.Async.Interest.writable
            ~source
            loop
      | Error (Kernel.IO.Stderr.System error) -> Error (Error.of_system_error error)
      | Error (Kernel.IO.Stderr.InvalidSlice _) -> Error Error.Invalid_argument
    in
    loop ()

let flush = fun () ->
  let source = Kernel.IO.Stderr.to_source () in
  let rec loop () =
    match Kernel.IO.Stderr.flush () with
    | Ok () -> Ok ()
    | Error (Kernel.IO.Stderr.System error) when Kernel.SystemError.would_block error ->
        Runtime.syscall
          ~name:"IO.Stderr.flush"
          ~interest:Kernel.Async.Interest.writable
          ~source
          loop
    | Error (Kernel.IO.Stderr.System error) -> Error (Error.of_system_error error)
    | Error (Kernel.IO.Stderr.InvalidSlice _) -> Error Error.Invalid_argument
  in
  loop ()

let to_writer = fun () ->
  let module Write = struct
    type nonrec t = t
    type nonrec err = error

    let write = fun () ~buf ->
      write (Bytes.from_string buf)

    let write_owned_vectored = fun () ~bufs ->
      write_vectored bufs

    let flush = fun () ->
      flush ()
  end in
  Writer.of_write_src (module Write) ()
