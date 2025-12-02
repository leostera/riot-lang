open Std

(** Cross-compilation toolchain detection and configuration *)

type detection_result = {
  sysroot : Path.t option;  (** Detected sysroot path *)
  bin_dir : Path.t option;  (** Directory containing cross-compiler binaries *)
  bin_prefix : string;      (** Binary prefix (e.g., "aarch64-linux-gnu-") *)
  c_compiler : Path.t option;  (** Path to C compiler *)
}

(** Derive binary prefix from target triplet
    
    Examples:
    - aarch64-unknown-linux-gnu → "aarch64-linux-gnu-"
    - x86_64-unknown-linux-gnu → "x86_64-linux-gnu-"
    - arm-unknown-linux-gnueabihf → "arm-linux-gnueabihf-"
*)
val bin_prefix_of_triplet : System.Host.t -> string

(** Find C compiler in PATH using binary prefix *)
val find_c_compiler : string -> Path.t option

(** Detect sysroot from C compiler by running `gcc -print-sysroot` *)
val detect_sysroot : Path.t -> Path.t option

(** Get bin directory from compiler path *)
val bin_dir_of_compiler : Path.t -> Path.t option

(** Detect cross-compilation toolchain for target
    
    This function:
    1. Derives the binary prefix from target triplet
    2. Searches for the cross-compiler in PATH
    3. Queries the compiler for its sysroot
    4. Returns all detected information
    
    If the cross-compiler is not found, returns a minimal config
    with bin_prefix set but other fields as None.
*)
val detect : target_triplet:System.Host.t -> detection_result

(** Get full path to a cross-compilation binary
    
    Example:
    {[
      binary_path config "gcc"
        → Some "/opt/homebrew/bin/aarch64-linux-gnu-gcc"
      
      binary_path config "ld"
        → Some "/opt/homebrew/bin/aarch64-linux-gnu-ld"
    ]}
*)
val binary_path : detection_result -> string -> Path.t option
