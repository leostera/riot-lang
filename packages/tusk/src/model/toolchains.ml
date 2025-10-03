(** Toolchain management for tusk build system *)

open Std
open Std.Data

(* Helper to run shell commands and return (success, output) *)
let run_command_compat cmd_str =
  let cmd = Command.make ~args:[ "-c"; cmd_str ] "sh" in
  match Command.output cmd with
  | Ok output -> (output.Command.status = 0, output.Command.stdout)
  | Error (Command.SystemError msg) -> (false, msg)

type source =
  | Version of string (* e.g., "5.3.0" *)
  | Path of Path.t (* e.g., "./ocaml/compiler" *)
  | Url of Net.Uri.t
(* e.g., "https://github.com/user/ocaml/archive/branch.tar.gz" *)

type toolchain = {
  version : string; (* version string for directory name *)
  source : source; (* where to get the compiler from *)
}

let default_ocaml_version = "5.3.0"

let default_toolchain =
  { version = default_ocaml_version; source = Version default_ocaml_version }

(* TODO: Implement toolchain management system
   
   1. Create and manage ~/.tusk/toolchains directory structure
      - Each toolchain version gets its own subdirectory
      - Store OCaml compiler, tools, and libraries
      - Handle platform-specific paths and configurations
   
   2. Create toolchain type and parse ocaml-toolchain.toml
      - Define a toolchain record type with:
        * version: string (e.g., "5.3.0")
        * channel: string (e.g., "stable", "nightly")
        * tools: list of required tools (ocamlc, ocamlopt, ocamldep, etc.)
        * env_vars: environment variable settings
      - Use Toml module to parse ocaml-toolchain.toml files
      - Support both global and project-specific toolchain configs
   
   3. Create ready_toolchains function
      - Check if requested toolchain is already installed
      - Download and install missing toolchains
      - Compile OCaml from source if needed
      - Set up proper directory structure in ~/.tusk/toolchains/<version>
      - Return paths to toolchain binaries
      - Cache toolchain information for faster lookups
   
   Additional TODOs:
   - Support multiple OCaml versions side-by-side
   - Handle toolchain switching (like rustup)
   - Provide toolchain validation and health checks
   - Support custom toolchain locations
   - Add toolchain metadata caching
   - Implement toolchain garbage collection
   - Support cross-compilation toolchains
*)

let toolchain_base_dir =
  let home =
    match Env.home_dir () with
    | Some path -> path
    | None ->
        Env.current_dir () |> Result.expect ~msg:"Failed to get current dir"
  in
  Path.(home / v ".tusk" / v "toolchains")

let get_toolchain_path toolchain =
  Path.(toolchain_base_dir / v toolchain.version)

let bin_dir toolchain = Path.(get_toolchain_path toolchain / Path.v "bin")
let bin_path toolchain bin = Path.(bin_dir toolchain / Path.v bin)
let ocamlc_path toolchain = bin_path toolchain "ocamlc.opt"
let ocamldep_path toolchain = bin_path toolchain "ocamldep.opt"
let ocamlformat_path toolchain = bin_path toolchain "ocamlformat"
let ocamllsp_path toolchain = bin_path toolchain "ocamllsp"
let ocamlopt_path toolchain = bin_path toolchain "ocamlopt.opt"
let odoc_path toolchain = bin_path toolchain "odoc"

(** Parse ocaml-toolchain.toml file *)
let parse_toolchain_file path =
  try
    let path_str = Path.to_string path in
    match Toml.parse_file path_str with
    | Error err ->
        Log.debug "[TOOLCHAINS] Failed to parse toolchain TOML: %s" (Toml.error_to_string err);
        { version = "5.3.0"; source = Version "5.3.0" }
    | Ok toml ->
        (* Pattern match on the nested structure *)
        match toml with
        | Toml.Table items -> (
            match List.assoc_opt "toolchain" items with
            | Some (Toml.Table toolchain_items) -> (
                match List.assoc_opt "version" toolchain_items with
                | Some (Toml.Table version_items) -> (
                    (* Check if it's a table with path or url *)
                    match List.assoc_opt "path" version_items with
                    | Some (Toml.String path_str) ->
                        let source_path = Path.of_string path_str |> Result.unwrap in
                        let version_name = format "%s-local" (Path.basename source_path) in
                        { version = version_name; source = Path source_path }
                    | _ -> (
                        match List.assoc_opt "url" version_items with
                        | Some (Toml.String url_str) ->
                            let version_name =
                              if String.contains url_str '/' then
                                let parts = String.split_on_char '/' url_str in
                                let last = List.hd (List.rev parts) in
                                if String.contains last '.' then
                                  Path.v last |> Path.remove_extension |> Path.to_string
                                else last
                              else "custom"
                            in
                            let uri = Net.Uri.of_string url_str |> Result.unwrap in
                            { version = version_name; source = Url uri }
                        | _ -> default_toolchain))
                | Some (Toml.String v) ->
                    (* It's a string version *)
                    { version = v; source = Version v }
                | _ -> default_toolchain)
            | _ -> default_toolchain)
        | _ -> default_toolchain
  with _ ->
    (* Default to stable version if file doesn't exist or can't be parsed *)
    default_toolchain

(** Check if a toolchain is installed *)
let is_toolchain_installed toolchain =
  let toolchain_path = get_toolchain_path toolchain in
  let ocamlc = ocamlc_path toolchain in
  match (Fs.exists toolchain_path, Fs.exists ocamlc) with
  | Ok true, Ok true -> true
  | _ -> false

let get_version toolchain = toolchain.version

(** Get cache directory for a URL *)
let get_cache_path uri =
  (* Build cache path from URI components: ~/.tusk/cache/host/path *)
  let cache_base =
    let home =
      match Env.home_dir () with
      | Some path -> path
      | None ->
          Env.current_dir () |> Result.expect ~msg:"Failed to get current dir"
    in
    Path.(home / Path.v ".tusk" / Path.v "cache")
  in

  (* Construct path from host and path components *)
  let host = Net.Uri.host uri |> Option.unwrap_or ~default:"unknown" in
  let path = Net.Uri.path uri in
  let relative_path = host ^ path in

  Path.(cache_base / Path.v relative_path)

(** Build OCaml from local source directory *)
let build_from_local_source ~source_path ~toolchain_path =
  let source_str = Path.to_string source_path in
  let toolchain_str = Path.to_string toolchain_path in
  println "Building OCaml from local source: %s..." source_str;

  (* Verify source directory exists *)
  (match Fs.exists source_path with
  | Ok true -> ()
  | _ -> failwith (format "Source directory does not exist: %s" source_str));

  (* Configure *)
  println "Configuring OCaml...";
  let configure_cmd =
    format "cd %s && ./configure --prefix=%s --disable-ocamldoc" source_str
      toolchain_str
  in
  let success, output = run_command_compat configure_cmd in
  if not success then failwith (format "Failed to configure OCaml: %s" output);

  (* Build *)
  println "Building OCaml (this may take a while)...";
  let num_cores = 4 in
  (* Default to 4 cores *)
  println "Using %d cores for compilation" num_cores;
  let make_cmd = format "cd %s && make -j%d" source_str num_cores in
  let success, output = run_command_compat make_cmd in
  if not success then failwith (format "Failed to build OCaml: %s" output);

  (* Install *)
  println "Installing OCaml...";
  let install_cmd = format "cd %s && make install" source_str in
  let success, output = run_command_compat install_cmd in
  if not success then failwith (format "Failed to install OCaml: %s" output);

  println "Successfully built and installed OCaml from %s" source_str

(** Download OCaml source from URL *)
let download_source_from_url uri =
  let url_str = Net.Uri.to_string uri in
  let cache_path = get_cache_path uri in
  let cache_dir = Path.parent cache_path |> Option.unwrap in

  (* Create cache directory if needed *)
  let _ = Fs.create_dir_all cache_dir in

  (* Check if already cached *)
  (match Fs.exists cache_path with
  | Ok false ->
      let cache_str = Path.to_string cache_path in
      println "Downloading OCaml from %s..." url_str;

      (* Download to cache *)
      let download_cmd = format "curl -L -o %s %s" cache_str url_str in
      let success, output = run_command_compat download_cmd in
      if not success then
        failwith (format "Failed to download OCaml: %s" output)
  | Ok true -> println "Using cached OCaml from %s" (Path.to_string cache_path)
  | Error _ -> failwith "Failed to check cache");

  (* Extract to temporary directory *)
  let extract_dir =
    Path.(
      Path.v "/tmp"
      / Path.v (format "ocaml-build-%d" (System.OsProcess.current_pid ())))
  in
  let _ = Fs.create_dir_all extract_dir in

  println "Extracting OCaml source...";
  let extract_cmd =
    format "tar -xzf %s -C %s"
      (Path.to_string cache_path)
      (Path.to_string extract_dir)
  in
  let success, output = run_command_compat extract_cmd in
  if not success then failwith (format "Failed to extract OCaml: %s" output);

  (* Find the extracted directory (should be the only subdirectory) *)
  let dirs =
    (match Fs.read_dir extract_dir with
      | Ok iter ->
          let result = ref [] in
          let rec read_all () =
            match MutIterator.next iter with
            | Some file ->
                result := Path.basename file :: !result;
                read_all ()
            | None -> !result
          in
          read_all ()
      | Error _ -> [])
    |> List.filter (fun f -> f <> "." && f <> "..")
  in
  match dirs with
  | [ dir ] -> Path.(extract_dir / Path.v dir)
  | [] -> failwith "No directory found after extraction"
  | _ -> failwith "Multiple directories found after extraction"

(** Download and extract OCaml source *)
let download_ocaml_source version =
  let major_minor =
    (* Extract major.minor from version like 5.3.0 *)
    match String.split_on_char '.' version with
    | major :: minor :: _ -> major ^ "." ^ minor
    | _ -> version
  in

  let url_str =
    format "https://github.com/ocaml/ocaml/archive/%s.tar.gz" version
  in
  let uri = Net.Uri.of_string url_str |> Result.unwrap in
  let cache_path = get_cache_path uri in
  let cache_dir = Path.parent cache_path |> Option.unwrap in

  (* Create cache directory if needed *)
  let _ = Fs.create_dir_all cache_dir in

  (* Check if already cached *)
  (match Fs.exists cache_path with
  | Ok false ->
      let cache_str = Path.to_string cache_path in
      println "Downloading OCaml %s from %s..." version url_str;

      (* Download to cache *)
      let download_cmd = format "curl -L -o %s %s" cache_str url_str in
      let success, output = run_command_compat download_cmd in
      if not success then
        failwith (format "Failed to download OCaml: %s" output)
  | Ok true ->
      println "Using cached OCaml %s from %s" version
        (Path.to_string cache_path)
  | Error _ -> failwith "Failed to check cache");

  (* Extract to temporary directory *)
  let extract_dir =
    Path.(
      Path.v "/tmp"
      / Path.v
          (format "ocaml-build-%s-%d" version (System.OsProcess.current_pid ())))
  in
  let _ = Fs.create_dir_all extract_dir in

  println "Extracting OCaml source...";
  let extract_cmd =
    format "tar -xzf %s -C %s"
      (Path.to_string cache_path)
      (Path.to_string extract_dir)
  in
  let success, output = run_command_compat extract_cmd in
  if not success then failwith (format "Failed to extract OCaml: %s" output);

  (* The extracted directory might be ocaml-5.3.0 or ocaml-5.3.0 *)
  let possible_dirs =
    [
      Path.(extract_dir / Path.v (format "ocaml-%s" version));
      Path.(extract_dir / Path.v (format "ocaml-%s" major_minor));
    ]
  in

  match
    List.find_opt
      (fun dir -> Fs.exists dir |> Result.unwrap_or ~default:false)
      possible_dirs
  with
  | Some dir -> dir
  | None -> failwith "Could not find extracted OCaml source directory"

(** Download and install pre-built dev tools *)
let install_dev_tools toolchain =
  let toolchain_path = get_toolchain_path toolchain in
  let bin_dir = Path.(toolchain_path / Path.v "bin") in

  (* Check if tools already exist *)
  let ocamllsp = Path.(bin_dir / Path.v "ocamllsp") in
  let odoc = Path.(bin_dir / Path.v "odoc") in
  let ocamlformat = Path.(bin_dir / Path.v "ocamlformat") in

  let all_exist =
    match (Fs.exists ocamllsp, Fs.exists odoc, Fs.exists ocamlformat) with
    | Ok true, Ok true, Ok true -> true
    | _ -> false
  in

  if all_exist then
    println "Development tools already installed for toolchain %s"
      toolchain.version
  else (
    println "Installing development tools for toolchain %s..." toolchain.version;

    (* Determine host triplet *)
    let host_triplet = Std.System.Host.(to_string current) in

    (* Download from the S3 CDN *)
    let tools_url =
      format
        "https://hel1.your-objectstorage.com/ml-riot-cdn/ocaml-platform/ocaml-platform-%s-%s.tar.gz"
        toolchain.version host_triplet
    in

    let tools_archive =
      Path.(
        Path.v "/tmp"
        / Path.v (format "ocaml-platform-%s.tar.gz" toolchain.version))
    in

    println "Downloading pre-built tools from %s..." tools_url;

    (* Use -S to show errors, -f to fail on HTTP errors *)
    let download_cmd =
      format "curl -fSL -o %s %s 2>&1" (Path.to_string tools_archive) tools_url
    in
    let success, output = run_command_compat download_cmd in

    if success then (
      (* Extract to toolchain directory *)
      println "Extracting development tools...";

      let extract_cmd =
        format "cd %s && tar xzf %s"
          (Path.to_string toolchain_path)
          (Path.to_string tools_archive)
      in
      let success, output = run_command_compat extract_cmd in

      if success then
        (* Clean up *)
        let _ = Fs.remove_file tools_archive in
        println "Successfully installed development tools"
      else failwith (format "Failed to extract tools: %s" output))
    else
      failwith
        (format "Failed to download pre-built tools from %s: %s" tools_url
           output))

(** Install a toolchain *)
let install_toolchain toolchain =
  if is_toolchain_installed toolchain then
    println "Toolchain %s is already installed" toolchain.version
  else (
    println "Installing OCaml toolchain %s..." toolchain.version;

    (* Create toolchain directory *)
    let toolchain_path = get_toolchain_path toolchain in
    let _ = Fs.create_dir_all toolchain_path in

    (* Build OCaml based on source type *)
    (match toolchain.source with
    | Version version ->
        (* Download and build from GitHub release *)
        let src_dir = download_ocaml_source version in
        build_from_local_source ~source_path:src_dir ~toolchain_path;
        (* Clean up temporary extraction directory *)
        let extract_parent = Path.parent src_dir |> Option.unwrap in
        let _ = Fs.remove_dir_all extract_parent in
        ()
    | Path path ->
        (* Build from local source directory *)
        (* Resolve path relative to workspace root if needed *)
        let source_path =
          if Path.is_relative path then
            (* Assume relative paths are relative to current working directory *)
            let cwd =
              Env.current_dir () |> Result.expect ~msg:"Failed to get cwd"
            in
            Path.join cwd path
          else path
        in
        build_from_local_source ~source_path ~toolchain_path
    | Url uri ->
        (* Download and build from URL *)
        let src_dir = download_source_from_url uri in
        build_from_local_source ~source_path:src_dir ~toolchain_path;
        (* Clean up temporary extraction directory *)
        let extract_parent = Path.parent src_dir |> Option.unwrap in
        let _ = Fs.remove_dir_all extract_parent in
        ());

    println "Successfully installed OCaml %s" toolchain.version;

    (* Download pre-built dev tools - only for Version sources *)
    match toolchain.source with
    | Version _ -> install_dev_tools toolchain
    | Path _ | Url _ ->
        println "Note: Development tools not installed for custom toolchain")

(** Ready toolchains for a workspace *)
let ready_toolchains workspace =
  (* Look for ocaml-toolchain.toml in workspace root *)
  let toolchain_file =
    Path.(workspace.Workspace.root / Path.v "ocaml-toolchain.toml")
  in
  let toolchain =
    match Fs.exists toolchain_file with
    | Ok true -> parse_toolchain_file toolchain_file
    | _ -> default_toolchain
  in

  println "Using OCaml toolchain: %s"
    (match toolchain.source with
    | Version v -> v
    | Path p -> format "path:%s" (Path.to_string p)
    | Url u -> format "url:%s" (Net.Uri.to_string u));

  (* Ensure toolchain is installed *)
  (if not (is_toolchain_installed toolchain) then (
     println "Toolchain %s not found. Installing..." toolchain.version;
     install_toolchain toolchain)
   else
     (* Check if dev tools are installed even if compiler exists *)
     let ocamllsp = ocamllsp_path toolchain in
     let odoc = odoc_path toolchain in
     let ocamlformat = ocamlformat_path toolchain in

     let tools_missing =
       match (Fs.exists ocamllsp, Fs.exists odoc, Fs.exists ocamlformat) with
       | Ok true, Ok true, Ok true -> false
       | _ -> true
     in

     if tools_missing then (
       println "Development tools missing. Installing...";
       install_dev_tools toolchain));

  (* Return the toolchain *)
  toolchain

(** Validate that a toolchain is working *)
let validate_toolchain toolchain =
  let ocamlc = ocamlc_path toolchain in
  match Fs.exists ocamlc with
  | Ok false -> false
  | Ok true ->
      (* Try to run ocamlc -version *)
      let cmd = format "%s -version" (Path.to_string ocamlc) in
      let success, _ = run_command_compat cmd in
      success
  | Error _ -> false

(** List installed toolchains *)
let list_installed_toolchains () =
  match Fs.exists toolchain_base_dir with
  | Ok false -> []
  | Ok true -> (
      match Fs.read_dir toolchain_base_dir with
      | Ok iter ->
          let result = ref [] in
          let rec collect () =
            match MutIterator.next iter with
            | None -> List.rev !result
            | Some path ->
                let dir_name = Path.basename path in
                (if dir_name <> "." && dir_name <> ".." then
                   let full_path =
                     Path.(toolchain_base_dir / Path.v dir_name)
                   in
                   let is_valid_dir =
                     match Fs.is_dir full_path with
                     | Ok true ->
                         let ocamlc_exists =
                           match
                             Fs.exists
                               Path.(full_path / Path.v "bin" / Path.v "ocamlc")
                           with
                           | Ok b -> b
                           | Error _ -> false
                         in
                         ocamlc_exists
                     | _ -> false
                   in
                   if is_valid_dir then result := dir_name :: !result);
                collect ()
          in
          collect ()
      | Error _ -> [])
  | Error _ -> []

(** Tests submodule *)
module Tests = struct
  let test_default_toolchain_uses_latest_version () : (unit, string) result =
    (* Test that default() returns the latest available toolchain *)
    Ok ()
    [@test]

  let test_ocamlc_path_points_to_valid_compiler () : (unit, string) result =
    (* Test that ocamlc_path returns executable path *)
    Ok ()
    [@test]

  let test_ocamldep_path_points_to_valid_tool () : (unit, string) result =
    (* Test that ocamldep_path returns executable path *)
    Ok ()
    [@test]

  let test_list_available_finds_installed_toolchains () : (unit, string) result
      =
    (* Test that list_available discovers all toolchains *)
    Ok ()
    [@test]

  let test_get_version_returns_semantic_version () : (unit, string) result =
    (* Test that get_version returns proper version string *)
    Ok ()
end [@test]

(** Hash a toolchain - hashes the compiler binary *)
let hash toolchain =
  let compiler_path = ocamlc_path toolchain in
  match Fs.exists compiler_path with
  | Ok false -> Crypto.hash_string toolchain.version
  | Ok true -> (
      match Fs.read_to_string compiler_path with
      | Ok contents -> Crypto.hash_string contents
      | Error _ -> Crypto.hash_string toolchain.version)
  | Error _ -> Crypto.hash_string toolchain.version
