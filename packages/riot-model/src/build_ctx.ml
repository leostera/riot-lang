open Std

type t = {
  host_triplet: System.Host.t;
  target: Target.t;  (* Changed from target_triplet *)
  profile: Profile.t;
  available_parallelism: int;
  session_id: Session_id.t;
}

let make = fun ~session_id ~profile ?target ?(available_parallelism = System.available_parallelism) () ->
  let host_triplet = System.Host.current in
  (* Use provided target or default to Host (native compilation) *)
  let target =
    match target with
    | Some t -> t
    | None -> Target.Host
  in
  {
    host_triplet;
    target;
    profile;
    available_parallelism;
    session_id;
  }

(** Get target platform name for package.target.* lookups *)
let target_platform_name = fun ctx -> Target.platform_name ctx.target

let host_platform_name = fun ctx ->
  match ctx.host_triplet.os with
  | "darwin" -> "macos"
  | "linux" -> "linux"
  | "windows" -> "windows"
  | other -> other

(** Check if cross-compiling *)
let is_cross_compile = fun ctx -> Target.is_cross ctx.target

(** Get sysroot if cross-compiling *)
let sysroot = fun ctx -> Target.sysroot ctx.target

(** Get target triplet *)
let target_triplet = fun ctx -> Target.triplet ctx.target

(** Hash build context into a Sha256 hasher state *)
let hash = fun state ctx ->
  let module H = Crypto.Sha256 in
  H.write state (System.Host.to_string ctx.host_triplet);
  Target.hash state ctx.target;
  (* Session ID excluded - it's for tracking/logging, not a build input *)
  Profile.hash state ctx.profile
