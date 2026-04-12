open Global

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
val create_encoder: ?level:int -> unit -> (encoder, error) result

val create_decoder: unit -> (decoder, error) result

val encode:
  encoder ->
  src:bytes ->
  src_pos:int ->
  src_len:int ->
  dst:bytes ->
  dst_pos:int ->
  dst_len:int ->
  flush:flush ->
  (step, error) result

val decode:
  decoder ->
  src:bytes ->
  src_pos:int ->
  src_len:int ->
  dst:bytes ->
  dst_pos:int ->
  dst_len:int ->
  (step, error) result

val close_encoder: encoder -> unit

val close_decoder: decoder -> unit
