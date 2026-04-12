open Kernel

type error = IO.error

(** Convert error to human-readable message *)
let error_message = fun err -> IO.error_message err

let of_file_error = function
  | Kernel.Fs.File.InvalidSlice _ -> IO.Invalid_argument
  | Kernel.Fs.File.System error -> IO.of_system_error error

let of_read_dir_error = function
  | Kernel.Fs.ReadDir.Closed -> IO.Closed
  | Kernel.Fs.ReadDir.File error -> of_file_error error

let convert_kernel_result = Result.map_error of_file_error

let convert_read_dir_result = Result.map_error of_read_dir_error
