open Global0
open IO

type decoder

type error =
  | Invalid_data
  | Need_dictionary
  | Buffer_error
  | Out_of_memory
  | Unknown_error of string

type status =
  | Need_input
  | Need_output
  | Finished

type step = {
  consumed: int;
  produced: int;
  status: status;
}

external _create_decoder: unit -> decoder = "kernel_gzip_create_decoder"

external _decode_raw:
  decoder ->
  bytes ->
  int ->
  int ->
  bytes ->
  int ->
  int ->
  int * int * int * int = "kernel_gzip_decode_bytecode" "kernel_gzip_decode"

external _close_decoder: decoder -> unit = "kernel_gzip_close_decoder"

let error_of_code = function
  | 0 -> None
  | 1 -> Some Invalid_data
  | 2 -> Some Need_dictionary
  | 3 -> Some Buffer_error
  | 4 -> Some Out_of_memory
  | code -> Some (Unknown_error ("unknown gzip error code " ^ Int.to_string code))

let status_of_code = function
  | 0 -> Need_input
  | 1 -> Need_output
  | 2 -> Finished
  | code -> panic ("invalid gzip status code " ^ Int.to_string code)

let check_slice = fun label buffer ~pos ~len ->
  if pos < 0 || len < 0 || pos + len > Bytes.length buffer then
    Stdlib.invalid_arg (label ^ ": invalid slice")

let create_decoder = fun () ->
  try Ok (_create_decoder ()) with
  | Failure msg -> Error (Unknown_error msg)

let decode = fun decoder ~src ~src_pos ~src_len ~dst ~dst_pos ~dst_len ->
  check_slice "Kernel.Compress.Gzip.decode src" src ~pos:src_pos ~len:src_len;
  check_slice "Kernel.Compress.Gzip.decode dst" dst ~pos:dst_pos ~len:dst_len;
  let (error_code, consumed, produced, status_code) =
    _decode_raw decoder src src_pos src_len dst dst_pos dst_len
  in
  match error_of_code error_code with
  | Some err -> Error err
  | None ->
      Ok { consumed; produced; status = status_of_code status_code }

let close_decoder = fun decoder -> _close_decoder decoder
