open Std

type section =
  | Runtime
  | Build
  | Dev
val section_name: section -> string

type error =
  | ReadFailed of { path: Path.t; error: IO.error }
  | WriteFailed of { path: Path.t; error: IO.error }
val error_message: error -> string

val update_dependency_section:
  manifest_path:Path.t ->
  section:section ->
  dependencies:Riot_model.Package.dependency list ->
  (unit, error) result
