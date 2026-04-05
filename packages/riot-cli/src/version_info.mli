open Std

type t = {
  release_id: string;
  build_sha: string;
  notes_url: string option;
  compare_url: string option;
  issues_url: string option;
}
val metadata_path: unit -> (Path.t, string) result

val of_path: Path.t -> (t, string) result

val of_json_string: string -> (t, string) result

val of_version_string: string -> t option

val write_path: path:Path.t -> t -> (unit, string) result

val same_identity: t -> t -> bool

val read_installed: unit -> t option

val write_installed: t -> (unit, string) result

val version_string_of: t -> string

val version_string: unit -> string

val release_label: t -> string

val agent_string: unit -> string
