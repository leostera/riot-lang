open Std

type request = {
  package_filter: string option;
  query: string option;
}
val parse_request: pattern:string option -> legacy_package:string option -> request

val extra_args: request -> string list -> string list
