open Std

type t =
  | DirectUnixUsage
  | DirectSysUsage
  | DirectStdlibUsage
  | DirectPervasivesUsage
  | PackageProvided of package_entry

and package_entry = {
  id : string;
  rule_id : string;
  title : string;
  body : string;
  message : string;
}

type entry = {
  code : t;
  title : string;
  body : string;
}

val to_id : t -> string
val of_id : string -> t option
val register_package_code : package_entry -> unit
val register_package_codes : package_entry list -> unit
val clear_package_codes : unit -> unit
val title : t -> string
val body : t -> string
val rule_id : t -> string
val message : t -> string
val no_stdlib_code_for_module : string -> t option
val explain : string -> entry option
val format_explanation : entry -> string
