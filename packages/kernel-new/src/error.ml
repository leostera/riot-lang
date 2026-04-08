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
  | Unknown of int

let code_end_of_file = 1

let code_permission_denied = 2

let code_no_such_file_or_directory = 3

let code_interrupted = 4

let code_input_output = 5

let code_bad_file_descriptor = 6

let code_resource_busy = 7

let code_already_exists = 8

let code_invalid_argument = 9

let code_no_space_left = 10

let code_broken_pipe = 11

let code_would_block = 12

let code_not_directory = 13

let code_is_directory = 14

let code_not_supported = 15

let code_address_in_use = 16

let code_address_not_available = 17

let code_connection_refused = 18

let code_connection_reset = 19

let code_timed_out = 20

let code_network_unreachable = 21

let of_code = function
  | 1 -> End_of_file
  | 2 -> Permission_denied
  | 3 -> No_such_file_or_directory
  | 4 -> Interrupted
  | 5 -> Input_output
  | 6 -> Bad_file_descriptor
  | 7 -> Resource_busy
  | 8 -> Already_exists
  | 9 -> Invalid_argument
  | 10 -> No_space_left
  | 11 -> Broken_pipe
  | 12 -> Would_block
  | 13 -> Not_directory
  | 14 -> Is_directory
  | 15 -> Not_supported
  | 16 -> Address_in_use
  | 17 -> Address_not_available
  | 18 -> Connection_refused
  | 19 -> Connection_reset
  | 20 -> Timed_out
  | 21 -> Network_unreachable
  | code -> Unknown code

let to_string = function
  | End_of_file -> "end of file"
  | Permission_denied -> "permission denied"
  | No_such_file_or_directory -> "no such file or directory"
  | Interrupted -> "interrupted system call"
  | Input_output -> "input/output error"
  | Bad_file_descriptor -> "bad file descriptor"
  | Resource_busy -> "resource busy"
  | Already_exists -> "already exists"
  | Invalid_argument -> "invalid argument"
  | No_space_left -> "no space left on device"
  | Broken_pipe -> "broken pipe"
  | Would_block -> "operation would block"
  | Not_directory -> "not a directory"
  | Is_directory -> "is a directory"
  | Not_supported -> "operation not supported"
  | Address_in_use -> "address already in use"
  | Address_not_available -> "address not available"
  | Connection_refused -> "connection refused"
  | Connection_reset -> "connection reset by peer"
  | Timed_out -> "timed out"
  | Network_unreachable -> "network unreachable"
  | Unknown _ -> "unknown kernel error"

external panic : string -> 'a = "kernel_new_panic"
