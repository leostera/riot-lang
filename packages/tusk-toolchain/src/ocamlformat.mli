type t

val make : Std.Path.t -> t
val path : t -> Std.Path.t

type format_result =
  | Formatted of { code : string; changed : bool }
  | Error of string

val find_ocamlformat_config : Std.Path.t -> Std.Path.t option
val format_file : t -> file_path:Std.Path.t -> check_only:bool -> format_result

val format_code :
  t -> code:string -> file_path:Std.Path.t option -> format_result
