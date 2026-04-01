open Std

val needs_refresh: workspace_root:Path.t -> manifest_paths:Path.t list -> (bool, string) result
