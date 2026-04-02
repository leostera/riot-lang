open Std

type t
val create: ?riot_home:Path.t -> registry_name:string -> unit -> (t, string) result

val riot_home: t -> Path.t

val registry_name: t -> string

val registry_dir: t -> Path.t

val index_dir: t -> Path.t

val archive_dir: t -> Path.t

val archive_path: t -> package_name:string -> version:string -> Path.t

val src_dir: t -> Path.t

val package_src_dir: t -> package_name:string -> version:string -> Path.t
