open Std
open Riot_model

(** Cross-compilation toolchain detection and configuration *)
type detection_result = {
  sysroot: Path.t option;
  bin_dir: Path.t option;
  bin_prefix: string;
  c_compiler: Path.t option;
}

(** Derive binary prefix from target triplet *)
let bin_prefix_of_triplet = fun triplet ->
  let open System.TargetTriple in
  match (triplet.architecture, triplet.os, triplet.vendor) with
  | ("aarch64", "linux", _) -> "aarch64-linux-gnu-"
  | ("x86_64", "linux", _) -> "x86_64-linux-gnu-"
  | ("arm", "linux", _) -> "arm-linux-gnueabihf-"
  | ("riscv64", "linux", _) -> "riscv64-linux-gnu-"
  | _ -> ""

(* Unknown - will try to detect *)
(** Find C compiler in PATH *)

let find_c_compiler = fun bin_prefix ->
  let gcc_name = bin_prefix ^ "gcc" in
  let check_cmd = Command.make ~args:[ "-c"; "command -v " ^ gcc_name ] "sh" in
  match Command.output check_cmd with
  | Ok output when output.status = 0 ->
      let cc_path = String.trim output.stdout in
      Some (Path.v cc_path)
  | _ -> None

(** Detect sysroot from C compiler *)
let detect_sysroot = fun cc_path ->
  let sysroot_cmd = Command.make ~args:[ "-print-sysroot" ] (Path.to_string cc_path) in
  match Command.output sysroot_cmd with
  | Ok output when output.status = 0 ->
      let sysroot_str = String.trim output.stdout in
      if sysroot_str = "" || sysroot_str = "/" then
        None
      else
        Some (Path.v sysroot_str)
  | _ -> None

(** Get bin directory from compiler path *)
let bin_dir_of_compiler = Path.parent

let env_sysroot = fun () ->
  let from_env name =
    match Env.get Env.String ~var:name with
    | Some path when not (String.equal path "") -> Some (Path.v path)
    | _ -> None
  in
  match from_env "CROSS_SYSROOT" with
  | Some path -> Some path
  | None -> from_env "SYSROOT"

let first_existing = fun paths ->
  let rec loop remaining =
    match remaining with
    | [] -> None
    | path :: rest ->
        match Fs.exists path with
        | Ok true -> Some path
        | _ -> loop rest
  in
  loop paths

let bundled_c_compiler = fun ~toolchain_root ~bin_prefix ->
  first_existing
    [
      Path.(toolchain_root / v "bin" / v (bin_prefix ^ "gcc"));
      Path.(toolchain_root / v "gcc" / v "bin" / v (bin_prefix ^ "gcc"));
    ]

let bundled_sysroot = fun ~toolchain_root ~target_triplet ->
  let target = System.TargetTriple.to_string target_triplet in
  first_existing
    [
      Path.(toolchain_root / v "sysroot");
      Path.(toolchain_root / v ("sysroot-" ^ target));
      Path.(toolchain_root / v "gcc" / v target / v "sysroot");
    ]

(** Detect cross-compilation toolchain for target *)
let detect = fun ?toolchain_root () ~target_triplet ->
  let bin_prefix = bin_prefix_of_triplet target_triplet in
  let bundled_cc =
    match toolchain_root with
    | Some root -> bundled_c_compiler ~toolchain_root:root ~bin_prefix
    | None -> None
  in
  let cc_path =
    match bundled_cc with
    | Some _ as cc -> cc
    | None -> find_c_compiler bin_prefix
  in
  let explicit_sysroot = env_sysroot () in
  let bundled_sr =
    match toolchain_root with
    | Some root -> bundled_sysroot ~toolchain_root:root ~target_triplet
    | None -> None
  in
  match cc_path with
  | None ->
      (* No cross-compiler found - return minimal config *)
      Log.warn ("Cross-compiler not found for " ^ System.TargetTriple.to_string target_triplet);
      Log.warn ("Expected: " ^ bin_prefix ^ "gcc in PATH");
      {
        sysroot =
          (
            match explicit_sysroot with
            | Some path -> Some path
            | None -> bundled_sr
          );
        bin_dir = None;
        bin_prefix;
        c_compiler = None;
      }
  | Some cc_path ->
      let sysroot =
        match explicit_sysroot with
        | Some path -> Some path
        | None ->
            match bundled_sr with
            | Some path -> Some path
            | None -> detect_sysroot cc_path
      in
      let bin_dir = bin_dir_of_compiler cc_path in
      (
        match bundled_cc with
        | Some path when Path.equal path cc_path ->
            Log.info ("✓ Using bundled cross-compiler: " ^ Path.to_string path)
        | _ -> Log.info ("✓ Using PATH cross-compiler: " ^ Path.to_string cc_path)
      );
      (
        match sysroot with
        | Some sr -> Log.info ("✓ Found sysroot: " ^ Path.to_string sr)
        | None -> Log.warn "⚠ No sysroot found for cross-compiler"
      );
      {
        sysroot;
        bin_dir;
        bin_prefix;
        c_compiler = Some cc_path;
      }

(** Get full path to a cross-compilation binary *)
let binary_path = fun config bin_name ->
  config.bin_dir
  |> Option.map ~fn:(fun dir -> Path.(dir / v (config.bin_prefix ^ bin_name)))
