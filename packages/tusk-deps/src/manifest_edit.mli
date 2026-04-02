open Std

type section =
  | Runtime
  | Build
  | Dev
val section_name: section -> string

val update_dependency_section:
  manifest_path:Path.t ->
  section:section ->
  dependencies:Tusk_model.Package.dependency list ->
  (unit, string) result
