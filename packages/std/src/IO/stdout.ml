open Prelude

module Bytes = Bytes
module Error = Error
module Iovec = Kernel.IO.Iovec
module Writer = Writer

type t = unit
type error = Error.t

let write = fun ?offset ?len buffer ->
  let source = Kernel.IO.Stdout.to_source () in
  let rec loop () =
    match Kernel.IO.Stdout.write ?pos:offset ?len buffer with
    | Ok value -> Ok value
    | Error (Kernel.IO.Stdout.System error) when Kernel.SystemError.would_block error ->
        Runtime.syscall
          ~name:"IO.Stdout.write"
          ~interest:Kernel.Async.Interest.writable
          ~source
          loop
    | Error (Kernel.IO.Stdout.System error) -> Error (Error.of_system_error error)
    | Error (Kernel.IO.Stdout.InvalidSlice _) -> Error Error.Invalid_argument
  in
  loop ()

let write_vectored = fun bufs ->
  let source = Kernel.IO.Stdout.to_source () in
  let rec loop () =
    match Kernel.IO.Stdout.write_vectored bufs with
    | Ok value -> Ok value
    | Error (Kernel.IO.Stdout.System error) when Kernel.SystemError.would_block error ->
        Runtime.syscall
          ~name:"IO.Stdout.write_vectored"
          ~interest:Kernel.Async.Interest.writable
          ~source
          loop
    | Error (Kernel.IO.Stdout.System error) -> Error (Error.of_system_error error)
    | Error (Kernel.IO.Stdout.InvalidSlice _) -> Error Error.Invalid_argument
  in
  loop ()

let flush = fun () ->
  let source = Kernel.IO.Stdout.to_source () in
  let rec loop () =
    match Kernel.IO.Stdout.flush () with
    | Ok () -> Ok ()
    | Error (Kernel.IO.Stdout.System error) when Kernel.SystemError.would_block error ->
        Runtime.syscall
          ~name:"IO.Stdout.flush"
          ~interest:Kernel.Async.Interest.writable
          ~source
          loop
    | Error (Kernel.IO.Stdout.System error) -> Error (Error.of_system_error error)
    | Error (Kernel.IO.Stdout.InvalidSlice _) -> Error Error.Invalid_argument
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
