open Std

val new_package:
  workspace:Riot_model.Workspace.t ->
  path:Path.t ->
  name:Riot_model.Package_name.t ->
  is_library:bool ->
  ((Path.t * Riot_model.Package_name.t), string) result
