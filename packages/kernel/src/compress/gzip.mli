open Global0

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

val create_decoder: unit -> (decoder, error) result

val decode:
  decoder ->
  src:bytes ->
  src_pos:int ->
  src_len:int ->
  dst:bytes ->
  dst_pos:int ->
  dst_len:int ->
  (step, error) result

val close_decoder: decoder -> unit
