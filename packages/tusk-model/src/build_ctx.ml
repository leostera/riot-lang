open Std

type t = {
  host_triplet : System.Host.t;
  target_triplet : System.Host.t;
  profile : Profile.t;
  available_parallelism : int;
  session_id : Session_id.t;
}

let make ~session_id ~profile ?(available_parallelism = System.available_parallelism) () =
  let host_triplet = System.Host.current in
  let target_triplet = host_triplet in  (* Cross-compilation: future *)
  {
    host_triplet;
    target_triplet;
    profile;
    available_parallelism;
    session_id;
  }

(** Get target platform name for package.target.* lookups *)
let target_platform_name ctx =
  match ctx.target_triplet.os with
  | "darwin" -> "macos"
  | "linux" -> "linux"
  | "windows" -> "windows"
  | other -> other

let host_platform_name ctx =
  match ctx.host_triplet.os with
  | "darwin" -> "macos"
  | "linux" -> "linux"
  | "windows" -> "windows"
  | other -> other

(** Hash build context into a Sha256 hasher state *)
let hash state ctx =
  let module H = Crypto.Sha256 in
  H.write_string state (System.Host.to_string ctx.host_triplet);
  H.write_string state (System.Host.to_string ctx.target_triplet);
  (* Session ID excluded - it's for tracking/logging, not a build input *)
  Profile.hash state ctx.profile
