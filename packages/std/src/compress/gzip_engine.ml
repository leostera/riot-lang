open Global

let panic = Kernel.SystemError.panic

module Bytes = IO.Bytes

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

external create_encoder_raw: int -> encoder = "std_gzip_create_encoder"

external create_decoder_raw: unit -> decoder = "std_gzip_create_decoder"

external encode_raw:
  encoder -> bytes -> int -> int -> bytes -> int -> int -> int -> int * int * int * int
  = "std_gzip_encode_bytecode" "std_gzip_encode"

external decode_raw: decoder -> bytes -> int -> int -> bytes -> int -> int -> int * int * int * int
  = "std_gzip_decode_bytecode" "std_gzip_decode"

external close_encoder_raw: encoder -> unit = "std_gzip_close_encoder"

external close_decoder_raw: decoder -> unit = "std_gzip_close_decoder"

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

let flush_to_code = function
  | No_flush -> 0
  | Sync_flush -> 1
  | Finish -> 2

let check_slice = fun label buffer ~pos ~len ->
  if pos < 0 || len < 0 || pos + len > Bytes.length buffer then
    raise (Invalid_argument (label ^ ": invalid slice"))

let create_encoder = fun ?(level = (-1)) () ->
  try Ok (create_encoder_raw level) with
  | Failure msg -> Error (Unknown_error msg)

let create_decoder = fun () ->
  try Ok (create_decoder_raw ()) with
  | Failure msg -> Error (Unknown_error msg)

let encode = fun encoder ~src ~src_pos ~src_len ~dst ~dst_pos ~dst_len ~flush ->
  check_slice "Std.Compress.Gzip_engine.encode src" src ~pos:src_pos ~len:src_len;
  check_slice "Std.Compress.Gzip_engine.encode dst" dst ~pos:dst_pos ~len:dst_len;
  let error_code, consumed, produced, status_code =
    encode_raw encoder src src_pos src_len dst dst_pos dst_len (flush_to_code flush)
  in
  match error_of_code error_code with
  | Some error -> Error error
  | None -> Ok { consumed; produced; status = status_of_code status_code }

let decode = fun decoder ~src ~src_pos ~src_len ~dst ~dst_pos ~dst_len ->
  check_slice "Std.Compress.Gzip_engine.decode src" src ~pos:src_pos ~len:src_len;
  check_slice "Std.Compress.Gzip_engine.decode dst" dst ~pos:dst_pos ~len:dst_len;
  let error_code, consumed, produced, status_code =
    decode_raw decoder src src_pos src_len dst dst_pos dst_len
  in
  match error_of_code error_code with
  | Some error -> Error error
  | None -> Ok { consumed; produced; status = status_of_code status_code }

let close_encoder = fun encoder -> close_encoder_raw encoder

let close_decoder = fun decoder -> close_decoder_raw decoder
