open Std

val normalized_name: string -> string

val package_prefix: string -> Path.t

val package_relpath: string -> Path.t

val package_cache_path: Registry_cache.t -> package_name:string -> Path.t
