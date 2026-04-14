open Std

val new_package:
  workspace:Riot_model.Workspace.t ->
  path:string ->
  name:string ->
  is_library:bool ->
  ((string * string), string) result
