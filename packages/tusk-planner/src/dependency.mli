(** Dependency information for build planning
    
    Represents a package dependency with its library path for linking.
*)

open Std
open Tusk_model

type t = {
  package : Package.t;
  library_path : Path.t;
}

val make : package:Package.t -> library_path:Path.t -> t
