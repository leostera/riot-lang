type error = Unix.error

type file_kind =
  | Regular
  | Directory
  | Symlink
  | Block
  | Character
  | Fifo
  | Socket

val file_kind_of_unix : Unix.file_kind -> file_kind
val file_kind_to_unix : file_kind -> Unix.file_kind
val unix_error_message : error -> string
