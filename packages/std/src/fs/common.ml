type error = SystemError of string

(** Helper to convert Kernel.IO errors to our error type *)
let kernel_error_to_string = function
  | `Noop -> "No operation"
  | `Eof -> "End of file"
  | `Timeout -> "Timeout"
  | `Process_down -> "Process down"
  | `Closed -> "Closed"
  | `Connection_closed -> "Connection closed"
  | `Exn exn -> Printexc.to_string exn
  | `No_info -> "No info"
  | `Would_block -> "Would block"
  | `Unix_error err -> Kernel.IO.unix_error_message err
  | _ -> "Unknown error"

let convert_kernel_result = function
  | Ok v -> Ok v
  | Error e -> Error (SystemError (kernel_error_to_string e))
