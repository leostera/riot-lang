open Std

type size_filter =
  | All
  | Small
  | Large
type request = {
  package_filter: Riot_model.Package_name.t option;
  suite_filter: string option;
  query: string option;
  size_filter: size_filter;
  flaky_only: bool;
}
val parse_request:
  pattern:string option ->
  legacy_package:Riot_model.Package_name.t option ->
  size_filter:size_filter ->
  flaky_only:bool ->
  (request, string) result

val extra_args:
  ?small_test_timeout:Time.Duration.t option ->
  ?flaky_max_retries:int ->
  request ->
  string list ->
  string list
