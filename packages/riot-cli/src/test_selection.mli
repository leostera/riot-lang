open Std

type size_filter =
  | All
  | Small
  | Long

type request = {
  package_filter: string option;
  suite_filter: string option;
  query: string option;
  size_filter: size_filter;
  flaky_only: bool;
}
val parse_request:
  pattern:string option -> legacy_package:string option -> size_filter:size_filter -> flaky_only:bool -> request

val extra_args:
  ?small_test_timeout:Time.Duration.t option -> ?flaky_max_retries:int -> request -> string list -> string list
