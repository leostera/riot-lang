open Global0
open IO

type encoder

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

type flush =
  | No_flush
  | Sync_flush
  | Finish

external _create_encoder: int -> encoder = "kernel_gzip_create_encoder"

external _create_decoder: unit -> decoder = "kernel_gzip_create_decoder"

external _encode_raw:
  encoder -> bytes -> int -> int -> bytes -> int -> int -> int -> int * int * int * int
  = "kernel_gzip_encode_bytecode" "kernel_gzip_encode"

external _decode_raw: decoder -> bytes -> int -> int -> bytes -> int -> int -> int * int * int * int
  = "kernel_gzip_decode_bytecode" "kernel_gzip_decode"

external _close_encoder: encoder -> unit = "kernel_gzip_close_encoder"

external _close_decoder: decoder -> unit = "kernel_gzip_close_decoder"

let error_of_code = function
  | 0 -> None
  | 1 -> Some Invalid_data
  | 2 -> Some Need_dictionary
  | 3 -> Some Buffer_error
  | 4 -> Some Out_of_memory
  | code -> Some (Unknown_error (Format.format Format.[ str "unknown gzip error code "; int code ]))

let status_of_code = function
  | 0 -> Need_input
  | 1 -> Need_output
  | 2 -> Finished
  | code -> panic (Format.format Format.[ str "invalid gzip status code "; int code ])

let flush_to_code = function
  | No_flush -> 0
  | Sync_flush -> 1
  | Finish -> 2

let check_slice = fun label buffer ~pos ~len ->
  if pos < 0 || len < 0 || pos + len > Bytes.length buffer then
    Stdlib.invalid_arg (Format.format Format.[ str label; str ": invalid slice" ])

let create_encoder = fun ?(level = (-1)) () ->
  try Ok (_create_encoder level) with
  | Failure msg -> Error (Unknown_error msg)

let create_decoder = fun () ->
  try Ok (_create_decoder ()) with
  | Failure msg -> Error (Unknown_error msg)

let encode = fun encoder ~src ~src_pos ~src_len ~dst ~dst_pos ~dst_len ~flush ->
  check_slice "Kernel.Compress.Gzip.encode src" src ~pos:src_pos ~len:src_len;
  check_slice "Kernel.Compress.Gzip.encode dst" dst ~pos:dst_pos ~len:dst_len;
  let (error_code, consumed, produced, status_code) = _encode_raw
    encoder
    src
    src_pos
    src_len
    dst
    dst_pos
    dst_len
    (flush_to_code flush) in
  match error_of_code error_code with
  | Some err -> Error err
  | None -> Ok { consumed; produced; status = status_of_code status_code }

let decode = fun decoder ~src ~src_pos ~src_len ~dst ~dst_pos ~dst_len ->
  check_slice "Kernel.Compress.Gzip.decode src" src ~pos:src_pos ~len:src_len;
  check_slice "Kernel.Compress.Gzip.decode dst" dst ~pos:dst_pos ~len:dst_len;
  let (error_code, consumed, produced, status_code) = _decode_raw
    decoder
    src
    src_pos
    src_len
    dst
    dst_pos
    dst_len in
  match error_of_code error_code with
  | Some err -> Error err
  | None -> Ok { consumed; produced; status = status_of_code status_code }

let close_encoder = fun encoder -> _close_encoder encoder

let close_decoder = fun decoder -> _close_decoder decoder
