(* OCaml module representation *)

type t

val create : package_name:string -> path:string -> t
val module_name : t -> string
val namespaced_name : t -> string
val path : t -> string
val cmi : t -> string
val cmo : t -> string
val eq : t -> t -> bool
val kind : t -> [ `implementation | `interface ]
val is_aliases : t -> bool
val dependencies : t -> Workspace.package -> string list
val make_alias_module : Workspace.package -> string list -> t

val make_library_interface :
  Workspace.package -> string -> t list -> string list -> exists:bool -> t
