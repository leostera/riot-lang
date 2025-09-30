type error = SystemError of string

val kernel_error_to_string :
  [> `Closed
  | `Connection_closed
  | `Eof
  | `Exn of exn
  | `No_info
  | `Noop
  | `Process_down
  | `Timeout
  | `Unix_error of Unix.error
  | `Would_block ] ->
  string

val convert_kernel_result :
  ( 'a,
    [> `Closed
    | `Connection_closed
    | `Eof
    | `Exn of exn
    | `No_info
    | `Noop
    | `Process_down
    | `Timeout
    | `Unix_error of Unix.error
    | `Would_block ] )
  result ->
  ('a, error) result
