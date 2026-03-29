open Std

type t = {
  host_triplet : Kernel.System.Host.t;
  target : Target.t;
  (* Changed from target_triplet *)
  profile : Profile.t;
  available_parallelism : int;
  session_id : Session_id.t;
}
val make : session_id:Session_id.t ->
profile:Profile.t ->
?target:Target.t ->
?available_parallelism:int ->
unit ->
t

(** Get target platform name for package.target.* lookups *)
val target_platform_name : t -> string

(** Get host platform name *)
val host_platform_name : t -> string

(** Check if cross-compiling *)
val is_cross_compile : t -> bool

(** Get sysroot if cross-compiling *)
val sysroot : t -> Path.t option

(** Get target triplet *)
val target_triplet : t -> Kernel.System.Host.t

(** Hash build context into a Sha256 hasher state *)
val hash : Crypto.Sha256.state -> t -> unit
