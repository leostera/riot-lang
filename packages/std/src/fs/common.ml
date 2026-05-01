open Global

type error = IO.error

(** Convert error to human-readable message *)
let error_message = fun err -> IO.error_message err

let of_file_error = fun __tmp1 ->
  match __tmp1 with
  | Kernel.Fs.File.InvalidSlice _ -> IO.Invalid_argument
  | Kernel.Fs.File.System error -> IO.of_system_error error

let of_read_dir_error = fun __tmp1 ->
  match __tmp1 with
  | Kernel.Fs.ReadDir.Closed -> IO.Closed
  | Kernel.Fs.ReadDir.File error -> of_file_error error

let convert_kernel_result: 'a. ('a, Kernel.Fs.File.error) Kernel.Result.t -> ('a, error) result = fun
  __tmp1 ->
  match __tmp1 with
  | Ok value -> Ok value
  | Error error -> Error (of_file_error error)

let convert_read_dir_result: 'a. ('a, Kernel.Fs.ReadDir.error) Kernel.Result.t -> ('a, error) result = fun
  __tmp1 ->
  match __tmp1 with
  | Ok value -> Ok value
  | Error error -> Error (of_read_dir_error error)
