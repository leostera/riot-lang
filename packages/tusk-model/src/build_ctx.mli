open Std

type t = {
  host_triplet : Kernel.System.Host.t;
  target_triplet : Kernel.System.Host.t;
  profile : Profile.t;
  available_parallelism : int;
  session_id : Session_id.t;
}

val make : 
  session_id:Session_id.t -> 
  profile:Profile.t ->
  ?available_parallelism:int -> 
  unit -> t

(** Get target platform name for package.target.* lookups *)
val target_platform_name : t -> string

(** Get host platform name *)
val host_platform_name : t -> string

(** Hash build context into a Sha256 hasher state *)
val hash : Crypto.Sha256.state -> t -> unit
