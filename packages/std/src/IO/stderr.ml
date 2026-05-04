open Prelude

module Buffer = Buffer
module Error = Error
module IoVec = IoVec
module Writer = Writer

type t = unit

type error = Error.t

type 'value result = ('value, error) Result.t

let write = fun ~from ->
  let source = Kernel.IO.Stderr.to_source () in
  let rec loop () =
    match Kernel.IO.Stderr.write_vectored (Buffer.to_iovec from) with
    | Ok value -> Ok value
    | Error (Kernel.IO.Stderr.System error) when Kernel.SystemError.would_block error ->
        Runtime.syscall
          ~name:"IO.Stderr.write"
          ~interest:Kernel.Async.Interest.writable
          ~source
          loop
    | Error (Kernel.IO.Stderr.System error) -> Error (Error.from_system_error error)
    | Error (Kernel.IO.Stderr.InvalidSlice _) -> Error Error.Invalid_argument
  in
  loop ()

let write_vectored = fun ~from ->
  if IoVec.length from = 0 then
    Ok 0
  else
    let source = Kernel.IO.Stderr.to_source () in
    let rec loop () =
      match Kernel.IO.Stderr.write_vectored from with
      | Ok value -> Ok value
      | Error (Kernel.IO.Stderr.System error) when Kernel.SystemError.would_block error ->
          Runtime.syscall
            ~name:"IO.Stderr.write_vectored"
            ~interest:Kernel.Async.Interest.writable
            ~source
            loop
      | Error (Kernel.IO.Stderr.System error) -> Error (Error.from_system_error error)
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
    | Error (Kernel.IO.Stderr.System error) -> Error (Error.from_system_error error)
    | Error (Kernel.IO.Stderr.InvalidSlice _) -> Error Error.Invalid_argument
  in
  loop ()

let to_writer = fun () ->
  let module Write = struct
    type nonrec t = t

    let write = fun () ~from -> write ~from

    let write_vectored = fun () ~from -> write_vectored ~from

    let flush = fun () -> flush ()
  end in
  Writer.from_sink (module Write) ()
