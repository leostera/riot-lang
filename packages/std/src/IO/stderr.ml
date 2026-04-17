open Prelude

module Bytes = Bytes
module Error = Error
module Iovec = Kernel.IO.Iovec

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
