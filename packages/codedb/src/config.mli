open Std
open Tusk_model

type t

val create :
  workspace_root:Path.t ->
  toolchain:Tusk_toolchain.t ->
  workspace:Workspace.t ->
  db_path:Path.t ->
  ?watch:bool ->
  unit ->
  t

val workspace_root : t -> Path.t
val toolchain : t -> Tusk_toolchain.t
val workspace : t -> Workspace.t
val db_path : t -> Path.t
val watch : t -> bool
