open Std

(** Cross-compilation toolchain detection and configuration *)

type detection_result = {
  sysroot : Path.t option;
  bin_dir : Path.t option;
  bin_prefix : string;
  c_compiler : Path.t option;
}

(** Derive binary prefix from target triplet *)
let bin_prefix_of_triplet triplet =
  let open System.Host in
  match (triplet.architecture, triplet.os, triplet.vendor) with
  | ("aarch64", "linux", _) -> "aarch64-linux-gnu-"
  | ("x86_64", "linux", _) -> "x86_64-linux-gnu-"
  | ("arm", "linux", _) -> "arm-linux-gnueabihf-"
  | ("riscv64", "linux", _) -> "riscv64-linux-gnu-"
  | _ -> ""  (* Unknown - will try to detect *)

(** Find C compiler in PATH *)
let find_c_compiler bin_prefix =
  let gcc_name = bin_prefix ^ "gcc" in
  let check_cmd = Command.make 
    ~args:["-c"; "command -v " ^ gcc_name] 
    "sh" in
  
  match Command.output check_cmd with
  | Ok output when output.status = 0 ->
      let cc_path = String.trim output.stdout in
      Some (Path.v cc_path)
  | _ -> None

(** Detect sysroot from C compiler *)
let detect_sysroot cc_path =
  let sysroot_cmd = Command.make 
    ~args:["-print-sysroot"] 
    (Path.to_string cc_path) in
  
  match Command.output sysroot_cmd with
  | Ok output when output.status = 0 ->
      let sysroot_str = String.trim output.stdout in
      if sysroot_str = "" || sysroot_str = "/" then None
      else Some (Path.v sysroot_str)
  | _ -> None

(** Get bin directory from compiler path *)
let bin_dir_of_compiler cc_path =
  Path.parent cc_path

(** Detect cross-compilation toolchain for target *)
let detect ~target_triplet =
  let bin_prefix = bin_prefix_of_triplet target_triplet in
  
  match find_c_compiler bin_prefix with
  | None ->
      (* No cross-compiler found - return minimal config *)
      Log.warn ("Cross-compiler not found for " ^ System.Host.to_string target_triplet);
      Log.warn ("Expected: " ^ bin_prefix ^ "gcc in PATH");
      {
        sysroot = None;
        bin_dir = None;
        bin_prefix;
        c_compiler = None;
      }
  
  | Some cc_path ->
      let sysroot = detect_sysroot cc_path in
      let bin_dir = bin_dir_of_compiler cc_path in
      
      (match sysroot with
      | Some sr -> 
          Log.info ("✓ Found sysroot: " ^ Path.to_string sr)
      | None -> 
          Log.warn ("⚠ No sysroot found for cross-compiler"));
      
      {
        sysroot;
        bin_dir;
        bin_prefix;
        c_compiler = Some cc_path;
      }

(** Get full path to a cross-compilation binary *)
let binary_path config bin_name =
  match config.bin_dir with
  | Some dir -> Some Path.(dir / v (config.bin_prefix ^ bin_name))
  | None -> None
