open Prelude

module Buffer = Buffer
module Error = Error
module IoVec = IoVec
module Writer = Writer

type t = unit

type error = Error.t

type 'value result = ('value, error) Result.t

let write = fun ~from ->
  let source = Kernel.IO.Stdout.to_source () in
  let rec loop () =
    match Kernel.IO.Stdout.write_vectored (Buffer.to_iovec from) with
    | Ok value -> Ok value
    | Error (Kernel.IO.Stdout.System error) when Kernel.SystemError.would_block error ->
        Runtime.syscall
          ~name:"IO.Stdout.write"
          ~interest:Kernel.Async.Interest.writable
          ~source
          loop
    | Error (Kernel.IO.Stdout.System error) -> Error (Error.from_system_error error)
    | Error (Kernel.IO.Stdout.InvalidSlice _) -> Error Error.Invalid_argument
  in
  loop ()

let write_vectored = fun ~from ->
  if IoVec.length from = 0 then
    Ok 0
  else
    let source = Kernel.IO.Stdout.to_source () in
    let rec loop () =
      match Kernel.IO.Stdout.write_vectored from with
      | Ok value -> Ok value
      | Error (Kernel.IO.Stdout.System error) when Kernel.SystemError.would_block error ->
          Runtime.syscall
            ~name:"IO.Stdout.write_vectored"
            ~interest:Kernel.Async.Interest.writable
            ~source
            loop
      | Error (Kernel.IO.Stdout.System error) -> Error (Error.from_system_error error)
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
    | Error (Kernel.IO.Stdout.System error) -> Error (Error.from_system_error error)
    | Error (Kernel.IO.Stdout.InvalidSlice _) -> Error Error.Invalid_argument
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
