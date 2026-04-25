open Prelude

type slice_validation = { pos: int; len: int; buffer_len: int }

type error =
  | InvalidSlice of { pos: int; len: int; buffer_len: int }
  | System of System_error.t

let validate_slice = fun buf ~pos ~len ->
  if pos < 0 || len < 0 || pos + len > Bytes.length buf then
    Result.Error ({ pos; len; buffer_len = Bytes.length buf } : slice_validation)
  else Result.Ok ()

let error_to_string = fun value ->
  match value with
  | InvalidSlice { pos; len; buffer_len } ->
      String.concat ""
        [
          "invalid buffer slice: pos=";
          Int.to_string pos;
          ", len=";
          Int.to_string len;
          ", buffer_len=";
          Int.to_string buffer_len;
        ]
  | System error -> System_error.to_string error

module FFI = struct
  external read: int -> bytes -> int -> int -> (int, int) Result.t = "kernel_new_fs_file_read"

  external write: int -> bytes -> int -> int -> (int, int) Result.t = "kernel_new_fs_file_write"

  external readv: int -> IoVec.t -> (int, int) Result.t = "kernel_new_fs_file_readv"

  external writev: int -> IoVec.t -> (int, int) Result.t = "kernel_new_fs_file_writev"

  external print: int -> string -> (unit, int) Result.t = "kernel_new_stdio_print"

  external println: int -> string -> (unit, int) Result.t = "kernel_new_stdio_println"
end

let to_source = fun fd ->
  let module Source = struct
    type nonrec t = int

    let register = fun fd selector token interest -> Async.Adapter.Selector.register selector ~fd ~token ~interest

    let reregister = fun fd selector token interest -> Async.Adapter.Selector.reregister selector ~fd ~token ~interest

    let deregister = fun fd selector -> Async.Adapter.Selector.deregister selector ~fd
  end in
  Async.Source.make (module Source) fd

let map_system_error = fun result -> Result.map_err result ~fn:(
  fun code -> System (System_error.from_code code)
)

let validate_buffer_write = fun ?(pos = 0) ?len buffer ->
  let len =
    match len with
    | Some len -> len
    | None -> Bytes.length buffer - pos
  in
  match validate_slice buffer ~pos ~len with
  | Result.Ok () -> Result.Ok (pos, len)
  | Result.Error { pos; len; buffer_len } -> Result.Error (InvalidSlice { pos; len; buffer_len })

module Stdin = struct
  type nonrec error = error =
    | InvalidSlice of { pos: int; len: int; buffer_len: int }
    | System of System_error.t

  let error_to_string = error_to_string

  let read = fun ?pos ?len buffer ->
    match validate_buffer_write ?pos ?len buffer with
    | Result.Ok (pos, len) -> FFI.read 0 buffer pos len |> map_system_error
    | Result.Error error -> Result.Error error

  let read_vectored = fun iovec -> FFI.readv 0 iovec |> map_system_error

  let flush = fun () -> Result.Ok ()

  let to_source = fun () -> to_source 0
end

module Stdout = struct
  type nonrec error = error =
    | InvalidSlice of { pos: int; len: int; buffer_len: int }
    | System of System_error.t

  let error_to_string = error_to_string

  let write = fun ?pos ?len buffer ->
    match validate_buffer_write ?pos ?len buffer with
    | Result.Ok (pos, len) -> FFI.write 1 buffer pos len |> map_system_error
    | Result.Error error -> Result.Error error

  let write_vectored = fun iovec -> FFI.writev 1 iovec |> map_system_error

  let print = fun message -> FFI.print 1 message |> map_system_error

  let println = fun message -> FFI.println 1 message |> map_system_error

  let flush = fun () -> Result.Ok ()

  let to_source = fun () -> to_source 1
end

module Stderr = struct
  type nonrec error = error =
    | InvalidSlice of { pos: int; len: int; buffer_len: int }
    | System of System_error.t

  let error_to_string = error_to_string

  let write = fun ?pos ?len buffer ->
    match validate_buffer_write ?pos ?len buffer with
    | Result.Ok (pos, len) -> FFI.write 2 buffer pos len |> map_system_error
    | Result.Error error -> Result.Error error

  let write_vectored = fun iovec -> FFI.writev 2 iovec |> map_system_error

  let print = fun message -> FFI.print 2 message |> map_system_error

  let println = fun message -> FFI.println 2 message |> map_system_error

  let flush = fun () -> Result.Ok ()

  let to_source = fun () -> to_source 2
end
