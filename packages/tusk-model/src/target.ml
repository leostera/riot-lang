open Std

(** Build target - either native (Host) or cross-compilation (Cross) *)
type t =
  | Host
  (** Native compilation - build for the current machine *)
  | Cross of cross_config

(** Cross-compilation - build for a different architecture *)
and cross_config = {
  target_triplet: System.Host.t;
  sysroot: Path.t option;  (** Detected sysroot for cross-compiler *)
  bin_dir: Path.t option;  (** Directory containing cross-compiler binaries *)
  bin_prefix: string;  (** Binary prefix (e.g., "aarch64-linux-gnu-") *)
}

(** Smart constructor that auto-detects sysroot and toolchain *)
let make_cross = fun ~target_triplet ->
    (* Note: We can't call Tusk_toolchain here due to circular dependency.
     Detection will be done at Build_ctx creation time. *)
    Cross {target_triplet; sysroot = None; bin_dir = None; bin_prefix = ""; }

(** Create Cross target with explicit configuration *)
let make_cross_with_config = fun ~target_triplet ~sysroot ~bin_dir ~bin_prefix ->
    Cross {target_triplet; sysroot; bin_dir; bin_prefix; }

(** Get target triplet (works for both Host and Cross) *)
let triplet = function
  | Host -> System.Host.current
  | Cross cfg -> cfg.target_triplet

(** Check if this is cross-compilation *)
let is_cross = function
  | Host -> false
  | Cross _ -> true

(** Get sysroot (None for native builds) *)
let sysroot = function
  | Host -> None
  | Cross cfg -> cfg.sysroot

(** Get binary directory *)
let bin_dir = function
  | Host -> None
  | Cross cfg -> cfg.bin_dir

(** Get binary prefix *)
let bin_prefix = function
  | Host -> ""
  | Cross cfg -> cfg.bin_prefix

(** Platform name for target-specific config lookups *)
let platform_name = fun t ->
    let triplet = triplet t in
    match triplet.System.Host.os with
    | "darwin" -> "macos"
    | "linux" -> "linux"
    | "windows" -> "windows"
    | other -> other

(** Hash target into a Sha256 hasher state *)
let hash = fun state t ->
    let module H = Crypto.Sha256 in
    match t with
    | Host ->
        H.write_string state "Host";
        H.write_string state (System.Host.to_string System.Host.current)
    | Cross cfg ->
        H.write_string state "Cross";
        H.write_string state (System.Host.to_string cfg.target_triplet);
        (
          match cfg.sysroot with
          | Some sr -> H.write_string state (Path.to_string sr)
          | None -> ()
        );
        H.write_string state cfg.bin_prefix
