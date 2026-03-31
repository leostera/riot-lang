open Std

type package = string
type version = Version.t
type version_ranges = version Ranges.t
type dependency_list = (package * version_ranges) list
type dependencies =
  Available of dependency_list
  | Unavailable of string
type 'error t = {
  choose_version: package -> version_ranges -> (version option, 'error) result;
  get_dependencies: package -> version -> (dependencies, 'error) result;
}
type offline
val create_offline: unit -> offline

val add_package: offline -> package -> version -> dependency_list -> unit

val to_provider: offline -> string t
