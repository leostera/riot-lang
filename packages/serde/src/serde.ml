open Std

include Fast

module Fast = Fast

let error_to_string = function
  | `invalid_field_type -> "invalid_field_type"
  | `missing_field -> "missing_field"
  | `no_more_data -> "no_more_data"
  | `unimplemented -> "unimplemented"
  | `invalid_tag -> "invalid_tag"
  | `Msg str -> String.concat "" [ "\""; str; "\"" ]
  | `Io_error err -> IO.error_message err

let pp_err _fmt error =
  ignore (error_to_string error)
