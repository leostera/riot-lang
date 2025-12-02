open Std

(** Build target - either native (Host) or cross-compilation (Cross) *)

type t =
  | Host
      (** Native compilation - build for the current machine *)
  | Cross of cross_config
      (** Cross-compilation - build for a different architecture *)

and cross_config = {
  target_triplet : System.Host.t;
  sysroot : Path.t option;    (** Detected sysroot for cross-compiler *)
  bin_dir : Path.t option;    (** Directory containing cross-compiler binaries *)
  bin_prefix : string;        (** Binary prefix (e.g., "aarch64-linux-gnu-") *)
}

(** Create a Cross target (sysroot will be None, should be populated by caller) *)
val make_cross : target_triplet:System.Host.t -> t

(** Create Cross target with explicit configuration *)
val make_cross_with_config :
  target_triplet:System.Host.t ->
  sysroot:Path.t option ->
  bin_dir:Path.t option ->
  bin_prefix:string ->
  t

(** Get target triplet (works for both Host and Cross) *)
val triplet : t -> System.Host.t

(** Check if this is cross-compilation *)
val is_cross : t -> bool

(** Get sysroot (None for native builds) *)
val sysroot : t -> Path.t option

(** Get binary directory *)
val bin_dir : t -> Path.t option

(** Get binary prefix *)
val bin_prefix : t -> string

(** Platform name for target-specific config lookups 
    
    Examples:
    - darwin → "macos"
    - linux → "linux"
    - windows → "windows"
*)
val platform_name : t -> string

(** Hash target into a Sha256 hasher state *)
val hash : Crypto.Sha256.state -> t -> unit
