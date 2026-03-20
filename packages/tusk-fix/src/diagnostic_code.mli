open Std

type t =
  | DirectUnixUsage
  | DirectSysUsage
  | DirectStdlibUsage
  | DirectPervasivesUsage

type entry = {
  code : t;
  title : string;
  body : string;
}

val to_id : t -> string
val of_id : string -> t option
val title : t -> string
val body : t -> string
val rule_id : t -> string
val message : t -> string
val no_stdlib_code_for_module : string -> t option
val explain : string -> entry option
val format_explanation : entry -> string
