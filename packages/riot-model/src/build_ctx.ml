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
  parallelism: int;
  session_id: Session_id.t;
}

let make = fun ~session_id ~profile ?(compilation_mode = HostOnly) ?(parallelism = Thread.available_parallelism) () ->
  let host = Target.current in
  {
    host;
    compilation_mode;
    profile;
    parallelism;
    session_id;
  }

let host = fun ctx -> ctx.host

let compilation_mode = fun ctx -> ctx.compilation_mode

let target_triplet = fun ctx ->
  match ctx.compilation_mode with
  | HostOnly -> ctx.host
  | Cross config -> config.target

(** Get target platform name for package.target.* lookups *)
let target_platform_name = fun ctx -> Target.platform_name (target_triplet ctx)

let host_platform_name = fun ctx ->
  match ctx.host.os with
  | "darwin" -> "macos"
  | "linux" -> "linux"
  | "windows" -> "windows"
  | other -> other

(** Check if cross-compiling *)
let is_cross_compile = fun ctx ->
  match ctx.compilation_mode with
  | HostOnly -> false
  | Cross _ -> true

(** Get sysroot if cross-compiling *)
let sysroot = fun ctx ->
  match ctx.compilation_mode with
  | HostOnly -> None
  | Cross config -> config.sysroot

(** Hash build context into a Sha256 hasher state *)
let hash = fun state ctx ->
  let module H = Crypto.Sha256 in
  Target.hash state ctx.host;
  (
    match ctx.compilation_mode with
    | HostOnly -> H.write state "host-only"
    | Cross config ->
        H.write state "cross";
        Target.hash state config.target;
        (
          match config.sysroot with
          | Some sysroot -> H.write state (Path.to_string sysroot)
          | None -> H.write state "no-sysroot"
        );
        (
          match config.bin_dir with
          | Some bin_dir -> H.write state (Path.to_string bin_dir)
          | None -> H.write state "no-bin-dir"
        );
        H.write state config.bin_prefix
  );
  (* Session ID excluded - it's for tracking/logging, not a build input *)
  Profile.hash state ctx.profile
