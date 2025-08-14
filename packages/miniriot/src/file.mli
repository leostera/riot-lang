(** File system operations for Miniriot *)

type error =
  [ `File_not_found
  | `Permission_denied
  | `Is_a_directory
  | `Not_a_directory
  | `Already_exists
  | `No_space
  | `Unknown of string ]
(** File operation errors *)

val exists : path:string -> bool
(** Check if a file exists at the given path *)

val read : path:string -> (string, error) result
(** Read the entire contents of a file *)

val write : path:string -> content:string -> (unit, error) result
(** Write content to a file, creating or truncating as needed *)

val remove : path:string -> (unit, error) result
(** Remove a file *)