open Std

(** Toolchain bootstrapping and management

    This package provides an abstraction layer over OCaml compiler tooling.
    Currently shells out to ocamlc/ocamldep, but designed to support:
    - In-process RAML compiler calls via FFI
    - Multiple compiler backends
    - Easier testing and mocking *)
type source =
  Version of string
  | Path of Path.t
  | Url of Net.Uri.t

module Ocamldep = Ocamldep
module Ocamlc = Ocamlc
module Ocamlformat = Ocamlformat
module CrossCompilingToolchain = Cross_compiling_toolchain

type t = {
  version: string;
  source: source;
  target: string;  (* Target triple this toolchain compiles for *)
  ocamlc: Ocamlc.t;
  ocamlopt: Path.t;
  ocamldep: Ocamldep.t;
  ocamlformat: Ocamlformat.t;
}

let default_ocaml_version = "5.5.0-riot.1"

let toolchain_base_dir = Path.(Tusk_model.Tusk_dirs.dot_tusk / Path.v "toolchains")

let get_host_triple = fun () ->
  match System.os_type with
  | "Unix" ->
      (* Use the host triplet from System module *)
      System.Host.to_string System.host_triplet
  | _ -> "x86_64-unknown-linux"

let get_toolchain_path = fun version ->
  let host_triple = get_host_triple () in
  Path.(toolchain_base_dir / Path.v version / Path.v host_triple)

let get_toolchain_path_for_target = fun version target ->
  Path.(toolchain_base_dir / Path.v version / Path.v target)

let local_compiler_path = fun () -> Path.v "./vendor/ocaml/compiler"

let make_toolchain = fun version source ~target ->
  let toolchain_path = get_toolchain_path_for_target version target in
  let bin_dir = Path.(toolchain_path / Path.v "bin") in
  let bin_path bin = Path.(bin_dir / Path.v bin) in
  {
    version;
    source;
    target;
    ocamlc = Ocamlc.make (bin_path "ocamlopt.opt");
    ocamlopt = bin_path "ocamlopt.opt";
    ocamldep = Ocamldep.make (bin_path "ocamldep.opt");
    ocamlformat = Ocamlformat.make (bin_path "ocamlformat");
  }

let ocamlc = fun t -> t.ocamlc

let ocamlopt_path = fun t -> t.ocamlopt

let ocamldep = fun t -> t.ocamldep

let ocamlformat = fun t -> t.ocamlformat

let check_binaries_exist = fun toolchain ->
  let ocamlc_path = Ocamlc.path toolchain.ocamlc in
  let ocamldep_path = Ocamldep.path toolchain.ocamldep in
  match (Fs.exists ocamlc_path, Fs.exists toolchain.ocamlopt, Fs.exists ocamldep_path) with
  | Ok true, Ok true, Ok true -> Ok ()
  | Ok false, _, _ -> Error ("ocamlc not found at " ^ Path.to_string ocamlc_path)
  | _, Ok false, _ -> Error ("ocamlopt not found at " ^ Path.to_string toolchain.ocamlopt)
  | _, _, Ok false -> Error ("ocamldep not found at " ^ Path.to_string ocamldep_path)
  | (Error err, _, _)
  | (_, Error err, _)
  | (_, _, Error err) -> Error ("Failed to check binaries: " ^ IO.error_message err)

let path_exists = fun path ->
  match Fs.exists path with
  | Ok true -> true
  | _ -> false

let dir_exists = fun path ->
  match Fs.is_dir path with
  | Ok true -> true
  | _ -> false

let write_path_fingerprint = fun hasher path ->
  Crypto.Sha256.write_string hasher (Path.to_string path);
  match Fs.metadata path with
  | Ok metadata ->
      Crypto.Sha256.write_string hasher (Int.to_string (Fs.Metadata.len metadata));
      Crypto.Sha256.write_string hasher (Float.to_string (Fs.Metadata.modified metadata))
  | Error _ -> Crypto.Sha256.write_string hasher "missing"

let first_existing = fun paths ->
  List.find_map
    (fun path ->
      if path_exists path then
        Some path
      else
        None)
    paths

let parse_json_field_string = fun json fields ->
  let rec resolve = fun value ->
    function
    | [] -> (
        match Data.Json.get_string value with
        | Some string -> Some string
        | None -> None
      )
    | field :: rest -> (
        match Data.Json.get_field field value with
        | Some next -> resolve next rest
        | None -> None
      )
  in
  resolve json fields

let read_manifest_fingerprint = fun toolchain_path ->
  let manifest_path = Path.(toolchain_path / Path.v "manifest.json") in
  let parse_fingerprint json =
    let open Data.Json in parse_json_field_string json [ "toolchain_fingerprint" ]
    |> Option.or_ (parse_json_field_string json [ "fingerprint"; "value" ]) in
  match Fs.exists manifest_path with
  | Ok true -> (
      match Fs.read manifest_path with
      | Ok raw -> (
          match Data.Json.of_string raw with
          | Ok manifest -> parse_fingerprint manifest
          | Error _ -> None
        )
      | Error _ -> None
    )
  | Ok false
  | Error _ -> None

let manifest_error = fun target manifest_path ->
  "Missing or invalid manifest.json at "
  ^ manifest_path
  ^ " for "
  ^ target
  ^ ". Reinstall this toolchain from a republished archive that includes manifest.json "
  ^ "(the release artifact must include a stable toolchain_fingerprint)."

let ensure_manifest_present = fun toolchain_path target source ->
  match source with
  | Path _ -> Ok ()
  | _ -> (
      match read_manifest_fingerprint toolchain_path with
      | Some fingerprint when not (String.equal (String.trim fingerprint) "") -> Ok ()
      | _ -> Error (manifest_error
        target
        (Path.to_string Path.(toolchain_path / Path.v "manifest.json")))
    )

let sysroot_candidates = fun ~toolchain_path ~target ->
  [
    Path.(toolchain_path / Path.v "sysroot");
    Path.(toolchain_path / Path.v ("sysroot-" ^ target));
    Path.(toolchain_path / Path.v "gcc" / Path.v target / Path.v "sysroot");
  ]

let explicit_sysroot_override = fun () ->
  let present name =
    match Env.var Env.String ~name with
    | Some value -> not (String.equal value "")
    | None -> false
  in
  present "CROSS_SYSROOT" || present "SYSROOT"

let missing_cross_components = fun ~toolchain_path ~target ->
  match Kernel.System.Host.from_string target with
  | Error _ -> []
  | Ok target_triplet ->
      let bin_prefix = CrossCompilingToolchain.bin_prefix_of_triplet target_triplet in
      let compiler_candidates = [
        Path.(toolchain_path / Path.v "bin" / Path.v (bin_prefix ^ "gcc"));
        Path.(toolchain_path / Path.v "gcc" / Path.v "bin" / Path.v (bin_prefix ^ "gcc"));
      ] in
      let sysroot_candidates = sysroot_candidates ~toolchain_path ~target in
      let compiler_missing =
        if List.exists path_exists compiler_candidates then
          []
        else
          [ "cross-compiler" ]
      in
      let sysroot_missing =
        if List.exists dir_exists sysroot_candidates then
          []
        else
          [ "sysroot" ]
      in
      compiler_missing @ sysroot_missing

let validate_toolchain_install = fun ~version ~target ~source ->
  let toolchain_path = get_toolchain_path_for_target version target in
  let toolchain = make_toolchain version source ~target in
  match check_binaries_exist toolchain with
  | Error _ -> Error [ "binaries" ]
  | Ok () ->
      let missing =
        if target = get_host_triple () || explicit_sysroot_override () then
          []
        else
          missing_cross_components ~toolchain_path ~target
      in
      if List.length missing > 0 then
        Error missing
      else
        match ensure_manifest_present toolchain_path target source with
        | Error msg -> Error [ msg ]
        | Ok () -> Ok toolchain

let reset_toolchain_install = fun path ->
  match Fs.exists path with
  | Ok true -> (
      match Fs.remove_dir_all path with
      | Ok () -> Ok ()
      | Error err -> Error ("Failed to remove existing toolchain at "
      ^ Path.to_string path
      ^ ": "
      ^ IO.error_message err)
    )
  | Ok false ->
      Ok ()
  | Error err ->
      Error ("Failed to inspect existing toolchain at "
      ^ Path.to_string path
      ^ ": "
      ^ IO.error_message err)

let download_and_install_toolchain = fun version ~host ~target ->
  let toolchain_path = get_toolchain_path_for_target version target in
  (* Determine URL pattern based on host vs cross-compilation *)
  let (binary_url, tar_filename, description) =
    if host = target then
      let url = "https://cdn.pkgs.ml/ocaml/ocaml-" ^ version ^ "-" ^ host ^ ".tar.gz" in
      let filename = "ocaml-" ^ version ^ "-" ^ host ^ ".tar.gz" in
      (url, filename, "native")
    else
      (* Cross-compilation toolchain *)
      let url = "https://cdn.pkgs.ml/ocaml/ocaml-" ^ version ^ "-" ^ host ^ "-x-" ^ target ^ ".tar.gz" in
      let filename = "ocaml-" ^ version ^ "-" ^ host ^ "-x-" ^ target ^ ".tar.gz" in
      (url, filename, "cross-compilation from " ^ host ^ " to " ^ target)
  in
  println ("📥 Downloading OCaml " ^ version ^ " for " ^ target ^ " (" ^ description ^ ")...");
  (* Create parent directories *)
  (
    match Path.parent toolchain_path with
    | Some parent ->
        let _ = Fs.create_dir_all parent in
        ()
    | None -> ()
  );
  (* Download URL *)
  let temp_dir = Path.(Tusk_model.Tusk_dirs.dot_tusk / Path.v "tmp") in
  let _ = Fs.create_dir_all temp_dir in
  let tar_path = Path.(temp_dir / Path.v tar_filename) in
  (* Download using curl *)
  let download_cmd = Command.make ~args:[ "-L"; "-o"; Path.to_string tar_path; binary_url ] "curl" in
  match Command.output download_cmd with
  | Error (Command.SystemError msg) ->
      Error ("Failed to download toolchain: " ^ msg)
  | Ok output when output.Command.status != 0 ->
      Error ("Failed to download toolchain from " ^ binary_url ^ "\nHTTP error or file not found")
  | Ok _ ->
      println "✓ Download complete, extracting...";
      (
        match reset_toolchain_install toolchain_path with
        | Error msg -> Error msg
        | Ok () ->
            (* Create toolchain directory *)
            let _ = Fs.create_dir_all toolchain_path in
            (* Extract using tar *)
            let extract_cmd = Command.make
              ~args:[ "-xzf"; Path.to_string tar_path; "-C"; Path.to_string toolchain_path ]
              "tar" in
            match Command.output extract_cmd with
            | Error (Command.SystemError msg) ->
                Error ("Failed to extract toolchain: " ^ msg)
            | Ok output when output.Command.status != 0 ->
                Error "Failed to extract toolchain tarball"
            | Ok _ -> (
                match ensure_manifest_present toolchain_path target (Version version) with
                | Error msg -> Error msg
                | Ok () ->
                    (* Clean up tarball *)
                    let _ = Fs.remove_file tar_path in
                    println ("✓ OCaml " ^ version ^ " (" ^ target ^ ") installed successfully");
                    Ok ()
              )
      )

let init = fun ~config ->
  let version = config.Tusk_model.Toolchain_config.version in
  let local_compiler = local_compiler_path () in
  let source =
    match config.source with
    | Tusk_model.Toolchain_config.Version v -> Version v
    | Tusk_model.Toolchain_config.Path p -> Path p
    | Tusk_model.Toolchain_config.Url u -> Url u
  in
  let source =
    match Fs.is_dir local_compiler with
    | Ok true -> Path local_compiler
    | _ -> source
  in
  let host = get_host_triple () in
  let target = host in
  (* init always uses host target for backward compatibility *)
  let toolchain = make_toolchain version source ~target in
  let toolchain_path = get_toolchain_path version in
  let bin_dir = Path.(toolchain_path / Path.v "bin") in
  (* Check if toolchain is already installed *)
  match Fs.is_dir bin_dir with
  | Ok true -> (
      match check_binaries_exist toolchain with
      | Ok () -> Ok toolchain
      | Error _ -> Error ("Toolchain at " ^ Path.to_string toolchain_path ^ " is incomplete")
    )
  | _ -> (
      (* Try to use ./vendor/ocaml/compiler if it exists *)
      match Fs.is_dir local_compiler with
      | Ok true -> (
          (* Create symlink from ~/.tusk/toolchains/{version}/{host_triple} to ./vendor/ocaml/compiler *)
          (
            match Path.parent toolchain_path with
            | Some parent ->
                let _ = Fs.create_dir_all parent in
                ()
            | None -> ()
          );
          (* Check if symlink already exists *)
          match Fs.exists toolchain_path with
          | Ok true -> Ok (make_toolchain version source ~target)
          | _ -> (
              (* Get absolute path for local_compiler *)
              let cwd = Env.current_dir () |> Result.expect ~msg:"Failed to get cwd" in
              let abs_local =
                if Path.is_absolute local_compiler then
                  local_compiler
                else
                  Path.(cwd / local_compiler)
              in
              match Fs.symlink ~src:abs_local ~dst:toolchain_path with
              | Ok () -> Ok (make_toolchain version source ~target)
              | Error err -> Error ("Failed to create toolchain symlink from "
              ^ Path.to_string toolchain_path
              ^ " to "
              ^ Path.to_string abs_local
              ^ ": "
              ^ IO.error_message err)
            )
        )
      | _ ->
          (* Try to download and install prebuilt binary *)
          println "Toolchain not found locally, attempting to download...";
          (
            match download_and_install_toolchain version ~host ~target with
            | Ok () -> (* Verify installation *)
              (
                match check_binaries_exist toolchain with
                | Ok () -> Ok toolchain
                | Error msg -> Error ("Toolchain installed but incomplete: " ^ msg)
              )
            | Error msg ->
                let host_triple = get_host_triple () in
                Error (
                  "Toolchain not found!\n\n\
                  Looking for: OCaml " ^ version ^ " for " ^ host_triple ^ "\n\
                  Expected location: " ^ Path.to_string toolchain_path ^ "\n\n\
                  Download failed: " ^ msg
                )
          )
    )

let ensure_default_toolchain = fun () ->
  let default_config = Tusk_model.Toolchain_config.default in
  match init ~config:default_config with
  | Ok _ -> Ok ()
  | Error msg -> Error msg

let check_health = fun toolchain ->
  match check_binaries_exist toolchain with
  | Error msg -> Error msg
  | Ok () -> (
      (* Try to execute ocamlc -version to verify it works *)
      let ocamlc_path = Ocamlc.path toolchain.ocamlc in
      let cmd = Command.make ~args:[ "-version" ] (Path.to_string ocamlc_path) in
      match Command.output cmd with
      | Ok output when output.Command.status = 0 ->
          Log.debug ("Toolchain healthy: ocamlc version = " ^ String.trim output.Command.stdout);
          Ok ()
      | Ok output ->
          Error ("ocamlc exists but failed: exit code " ^ Int.to_string output.Command.status)
      | Error (Command.SystemError msg) ->
          Error ("ocamlc health check failed: " ^ msg)
    )

let hash = fun t ->
  let hasher = Crypto.Sha256.create () in
  let toolchain_path = get_toolchain_path_for_target t.version t.target in
  let write_legacy_path_fingerprint paths = List.iter (write_path_fingerprint hasher) paths in
  Crypto.Sha256.write_string hasher t.version;
  Crypto.Sha256.write_string hasher t.target;
  let () =
    match t.source with
    | Path _ ->
        write_legacy_path_fingerprint
          [
            Ocamlc.path t.ocamlc;
            t.ocamlopt;
            Ocamldep.path t.ocamldep;
            Ocamlformat.path t.ocamlformat;
            Path.(toolchain_path / Path.v "lib" / Path.v "ocaml" / Path.v "stdlib.cmxa");
            Path.(toolchain_path / Path.v "lib" / Path.v "ocaml" / Path.v "unix" / Path.v "unix.cmi");
            Path.(toolchain_path / Path.v "lib" / Path.v "ocaml" / Path.v "unix" / Path.v "unix.cmxa");
          ];
        if not (String.equal t.target (get_host_triple ())) then
          (
            match first_existing (sysroot_candidates ~toolchain_path ~target:t.target) with
            | Some sysroot -> write_legacy_path_fingerprint
              [
                Path.(sysroot / Path.v "usr" / Path.v "include" / Path.v "uuid" / Path.v "uuid.h");
                Path.(sysroot / Path.v "usr" / Path.v "include" / Path.v "openssl" / Path.v "ssl.h");
                Path.(sysroot / Path.v "usr" / Path.v "include" / Path.v "zlib.h");
                Path.(sysroot / Path.v "usr" / Path.v "lib" / Path.v "libuuid.a");
                Path.(sysroot / Path.v "usr" / Path.v "lib" / Path.v "libssl.a");
                Path.(sysroot / Path.v "usr" / Path.v "lib" / Path.v "libcrypto.a");
              ]
            | None -> ()
          )
    | Version _
    | Url _ -> (
        match read_manifest_fingerprint toolchain_path with
        | Some fingerprint when not (String.equal (String.trim fingerprint) "") -> Crypto.Sha256.write_string
          hasher
          fingerprint
        | _ -> panic
          ("Toolchain manifest fingerprint is required for non-local toolchains. " ^ "Install from a republished archive that includes manifest.json.")
      )
  in
  Crypto.Sha256.finish hasher
(** Initialize toolchain for a specific target architecture *)
let init_for_target = fun ~config ~target ->
  let version = config.Tusk_model.Toolchain_config.version in
  let source =
    match config.source with
    | Tusk_model.Toolchain_config.Version v -> Version v
    | Tusk_model.Toolchain_config.Path p -> Path p
    | Tusk_model.Toolchain_config.Url u -> Url u
  in
  let host = get_host_triple () in
  let local_compiler = local_compiler_path () in
  let source =
    if String.equal target host then
      match Fs.is_dir local_compiler with
      | Ok true -> Path local_compiler
      | _ -> source
    else
      source
  in
  let toolchain_path = get_toolchain_path_for_target version target in
  let bin_dir = Path.(toolchain_path / Path.v "bin") in
  let validate () = validate_toolchain_install ~version ~target ~source in
  let refresh () =
    match download_and_install_toolchain version ~host ~target with
    | Ok () -> (
        match validate () with
        | Ok toolchain -> Ok toolchain
        | Error missing -> Error ("Downloaded toolchain for "
        ^ target
        ^ " but it is still incomplete: "
        ^ String.concat ", " missing)
      )
    | Error msg -> Error ("Failed to download toolchain for " ^ target ^ ": " ^ msg)
  in
  (* Check if already installed *)
  match Fs.is_dir bin_dir with
  | Ok true -> (
      match validate () with
      | Ok toolchain ->
          Ok toolchain
      | Error missing when not (String.equal target host) ->
          Log.info
            ("Refreshing cross toolchain for "
            ^ target
            ^ " because it is missing: "
            ^ String.concat ", " missing);
          refresh ()
      | Error _ ->
          Error ("Toolchain incomplete: " ^ Path.to_string toolchain_path)
    )
  | _ ->
      (* Try local compiler if native build *)
      if host = target then
        (
          match Fs.is_dir local_compiler with
          | Ok true ->
              (* Create symlink *)
              (
                match Path.parent toolchain_path with
                | Some parent ->
                    let _ = Fs.create_dir_all parent in
                    ()
                | None -> ()
              );
              (
                match Fs.exists toolchain_path with
                | Ok true ->
                    let toolchain = make_toolchain version source ~target in
                    Ok toolchain
                | _ ->
                    let cwd = Env.current_dir () |> Result.expect ~msg:"Failed to get cwd" in
                    let abs_local =
                      if Path.is_absolute local_compiler then
                        local_compiler
                      else
                        Path.(cwd / local_compiler)
                    in
                    (
                      match Fs.symlink ~src:abs_local ~dst:toolchain_path with
                      | Ok () ->
                          let toolchain = make_toolchain version source ~target in
                          Ok toolchain
                      | Error err -> Error ("Failed to create symlink: " ^ IO.error_message err)
                    )
              )
          | _ -> (* Download native toolchain *)
            (
              match download_and_install_toolchain version ~host ~target with
              | Ok () ->
                  let toolchain = make_toolchain version source ~target in
                  (
                    match check_binaries_exist toolchain with
                    | Ok () -> Ok toolchain
                    | Error msg -> Error ("Downloaded but incomplete: " ^ msg)
                  )
              | Error msg -> Error ("Failed to download toolchain for " ^ target ^ ": " ^ msg)
            )
        )
      else
        (* Cross-compilation - download cross-toolchain *)
        refresh ()
(** Get toolchain for specific target (lazy initialization) *)
let get_for_target = fun ~config ~target -> init_for_target ~config ~target

(** Toolchain management types and functions *)
type toolchain_status =
  | Installed of {
      path: Path.t;
    }
  | NotInstalled of {
      expected_path: Path.t;
    }
  | Incomplete of {
      path: Path.t;
      missing: string list;
    }

type toolchain_info = {
  version: string;
  target: string;
  is_host: bool;
  status: toolchain_status;
}

let check_toolchain_status = fun ~version ~target ->
  let toolchain_path = get_toolchain_path_for_target version target in
  let source =
    if String.equal target (get_host_triple ()) then
      match Fs.is_dir (local_compiler_path ()) with
      | Ok true -> Path (local_compiler_path ())
      | _ -> Version version
    else
      Version version
  in
  match Fs.is_dir toolchain_path with
  | Ok false
  | Error _ -> NotInstalled {expected_path = toolchain_path;}
  | Ok true -> (
      match validate_toolchain_install ~version ~target ~source with
      | Ok _ -> Installed {path = toolchain_path;}
      | Error missing -> Incomplete {path = toolchain_path;missing;}
    )

let list_toolchains = fun ~config ->
  let version = config.Tusk_model.Toolchain_config.version in
  let host = get_host_triple () in
  let targets =
    match config.targets with
    | [] -> [ host ]
    | ts -> ts
  in
  List.map
    (fun target ->
      let is_host = target = host in
      let status = check_toolchain_status ~version ~target in
      {version;target;is_host;status;})
    targets

let install_all_toolchains = fun ~config ->
  let version = config.Tusk_model.Toolchain_config.version in
  let toolchains = list_toolchains ~config in
  let host = get_host_triple () in
  let results =
    List.map
      (fun info ->
        match info.status with
        | Installed _ ->
            println
              (
                "  ✓ " ^ info.target ^ (
                  if info.is_host then
                    " (host)"
                  else
                    ""
                ) ^ " - already installed"
              );
            Ok `Skipped
        | NotInstalled _
        | Incomplete _ ->
            println
              (
                "  📥 " ^ info.target ^ (
                  if info.is_host then
                    " (host)"
                  else
                    ""
                ) ^ " - downloading..."
              );
            (
              match download_and_install_toolchain version ~host ~target:info.target with
              | Ok () -> Ok `Installed
              | Error msg ->
                  println ("     ✗ Failed: " ^ msg);
                  Error (info.target, msg)
            ))
      toolchains
  in
  let (successes, failures) =
    List.partition
      (
        function
        | Ok _ -> true
        | Error _ -> false
      )
      results
  in
  if List.length failures > 0 then
    let errors =
      List.filter_map
        (
          function
          | Error (target, msg) -> Some (target ^ ": " ^ msg)
          | _ -> None
        )
        results
    in
    Error ("Failed to install toolchains:\n  " ^ String.concat "\n  " errors)
  else
    let installed =
      List.filter
        (
          function
          | Ok `Installed -> true
          | _ -> false
        )
        successes
      |> List.length
    in
    let skipped =
      List.filter
        (
          function
          | Ok `Skipped -> true
          | _ -> false
        )
        successes
      |> List.length
    in
    Ok (installed, skipped)
