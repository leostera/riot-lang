open Std

type source = Version of string | Path of Path.t | Url of Net.Uri.t
type t = { version : string; source : source }

val default_ocaml_version : string
val default : t
val from_workspace : Workspace.t -> t
