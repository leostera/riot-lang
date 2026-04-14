open Std

type cross_config = {
  target: Target.t;
  sysroot: Path.t option;
  bin_dir: Path.t option;
  bin_prefix: string;
}

type compilation_mode =
  | HostOnly
  | Cross of cross_config

type t = {
  host: Target.t;
  compilation_mode: compilation_mode;
  profile: Profile.t;
  available_parallelism: int;
  session_id: Session_id.t;
}

val make:
  session_id:Session_id.t ->
  profile:Profile.t ->
  ?compilation_mode:compilation_mode ->
  ?available_parallelism:int ->
  unit ->
  t

val host: t -> Target.t

val compilation_mode: t -> compilation_mode

(** Get target platform name for package.target.* lookups *)
val target_platform_name: t -> string

(** Get host platform name *)
val host_platform_name: t -> string

(** Check if cross-compiling *)
val is_cross_compile: t -> bool

(** Get sysroot if cross-compiling *)
val sysroot: t -> Path.t option

(** Get target triplet *)
val target_triplet: t -> System.TargetTriple.t

(** Hash build context into a Sha256 hasher state *)
val hash: Crypto.Sha256.state -> t -> unit
