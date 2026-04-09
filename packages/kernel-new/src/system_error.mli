type t =
  | End_of_file
  | Permission_denied
  | No_such_file_or_directory
  | Interrupted
  | Input_output
  | Bad_file_descriptor
  | Resource_busy
  | Already_exists
  | Invalid_argument
  | No_space_left
  | Broken_pipe
  | Would_block
  | Not_directory
  | Is_directory
  | Not_supported
  | Address_in_use
  | Address_not_available
  | Connection_refused
  | Connection_reset
  | Timed_out
  | Network_unreachable
  | Destination_address_required
  | Not_connected
  | Connection_aborted
  | Message_too_long
  | No_such_process
  | Directory_not_empty
  | Unknown of int
val code_end_of_file: int

val code_permission_denied: int

val code_no_such_file_or_directory: int

val code_interrupted: int

val code_input_output: int

val code_bad_file_descriptor: int

val code_resource_busy: int

val code_already_exists: int

val code_invalid_argument: int

val code_no_space_left: int

val code_broken_pipe: int

val code_would_block: int

val code_not_directory: int

val code_is_directory: int

val code_not_supported: int

val code_address_in_use: int

val code_address_not_available: int

val code_connection_refused: int

val code_connection_reset: int

val code_timed_out: int

val code_network_unreachable: int

val code_destination_address_required: int

val code_not_connected: int

val code_connection_aborted: int

val code_message_too_long: int

val code_no_such_process: int

val code_directory_not_empty: int

val of_code: int -> t

val to_string: t -> string

val is_would_block: t -> bool

external panic: string -> 'a = "kernel_new_panic"
