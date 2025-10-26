open Std

val dot_tusk : Path.t
val toolchains_dir : Toolchain_config.t -> Path.t
val project_dir : Workspace.t -> Path.t
val ensure_created : unit -> (unit, exn) result
