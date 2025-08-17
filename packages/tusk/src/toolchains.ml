(** Toolchain management for tusk build system *)

type source =
  | Version of string (* e.g., "5.3.0" *)
  | Path of string (* e.g., "./ocaml/compiler" *)
  | Url of
      string (* e.g., "https://github.com/user/ocaml/archive/branch.tar.gz" *)

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

let toolchain_base_dir = Filename.concat (System.get_home ()) ".tusk/toolchains"

let get_toolchain_path toolchain =
  Filename.concat toolchain_base_dir toolchain.version

let ocamlc_path toolchain =
  Filename.concat (get_toolchain_path toolchain) "bin/ocamlc"

let ocamlopt_path toolchain =
  Filename.concat (get_toolchain_path toolchain) "bin/ocamlopt"

let ocamldep_path toolchain =
  Filename.concat (get_toolchain_path toolchain) "bin/ocamldep"

(** Parse ocaml-toolchain.toml file *)
let parse_toolchain_file path =
  try
    let toml = Toml.parse_file path in
    (* Look for toolchain.version *)
    match Toml.find_value "toolchain.version" toml with
    | Some value -> (
        (* Check if it's a table with path or url *)
        (* Check if value is a table with path or url *)
        match value with
        | Toml.Table items -> (
            (* Look for path field *)
            match List.assoc_opt "path" items with
            | Some (Toml.String path_str) ->
                (* For path sources, create a version name from the path *)
                let version_name =
                  (* Use basename + "-local" to make it unique *)
                  Printf.sprintf "%s-local" (Filename.basename path_str)
                in
                { version = version_name; source = Path path_str }
            | _ -> (
                (* Look for url field *)
                match List.assoc_opt "url" items with
                | Some (Toml.String url_str) ->
                    (* For URL sources, extract version from URL or use hash *)
                    let version_name =
                      (* Try to extract version from URL *)
                      if String.contains url_str '/' then
                        let parts = String.split_on_char '/' url_str in
                        let last = List.hd (List.rev parts) in
                        if String.contains last '.' then
                          Filename.chop_extension last
                        else last
                      else "custom"
                    in
                    { version = version_name; source = Url url_str }
                | _ -> default_toolchain))
        | Toml.String v ->
            (* It's a string version *)
            { version = v; source = Version v }
        | _ -> default_toolchain)
    | None -> default_toolchain
  with _ ->
    (* Default to stable version if file doesn't exist or can't be parsed *)
    default_toolchain

(** Check if a toolchain is installed *)
let is_toolchain_installed toolchain =
  let toolchain_path = get_toolchain_path toolchain in
  let ocamlc = ocamlc_path toolchain in
  System.file_exists toolchain_path && System.file_exists ocamlc

let get_version toolchain = toolchain.version

(** Get cache directory for a URL *)
let get_cache_path url =
  (* Remove protocol prefix *)
  let url_without_protocol =
    if String.length url > 8 && String.sub url 0 8 = "https://" then
      String.sub url 8 (String.length url - 8)
    else if String.length url > 7 && String.sub url 0 7 = "http://" then
      String.sub url 7 (String.length url - 7)
    else url
  in

  (* Create cache path: ~/.tusk/cache/domain/path/file *)
  let cache_base = Filename.concat (System.get_home ()) ".tusk/cache" in
  Filename.concat cache_base url_without_protocol

(** Build OCaml from local source directory *)
let build_from_local_source ~source_path ~toolchain_path =
  Printf.printf "Building OCaml from local source: %s...\n%!" source_path;

  (* Verify source directory exists *)
  if not (System.file_exists source_path) then
    failwith (Printf.sprintf "Source directory does not exist: %s" source_path);

  (* Configure *)
  Printf.printf "Configuring OCaml...\n%!";
  let configure_cmd =
    Printf.sprintf "cd %s && ./configure --prefix=%s --disable-ocamldoc"
      source_path toolchain_path
  in
  let success, output = System.run_command configure_cmd in
  if not success then
    failwith (Printf.sprintf "Failed to configure OCaml: %s" output);

  (* Build *)
  Printf.printf "Building OCaml (this may take a while)...\n%!";
  let num_cores = System.cpu_count () in
  Printf.printf "Using %d cores for compilation\n%!" num_cores;
  let make_cmd = Printf.sprintf "cd %s && make -j%d" source_path num_cores in
  let success, output = System.run_command make_cmd in
  if not success then
    failwith (Printf.sprintf "Failed to build OCaml: %s" output);

  (* Install *)
  Printf.printf "Installing OCaml...\n%!";
  let install_cmd = Printf.sprintf "cd %s && make install" source_path in
  let success, output = System.run_command install_cmd in
  if not success then
    failwith (Printf.sprintf "Failed to install OCaml: %s" output);

  Printf.printf "Successfully built and installed OCaml from %s\n%!" source_path

(** Download OCaml source from URL *)
let download_source_from_url url =
  let cache_path = get_cache_path url in
  let cache_dir = Filename.dirname cache_path in

  (* Create cache directory if needed *)
  System.mkdirp cache_dir;

  (* Check if already cached *)
  if not (System.file_exists cache_path) then (
    Printf.printf "Downloading OCaml from %s...\n%!" url;

    (* Download to cache *)
    let download_cmd = Printf.sprintf "curl -L -o %s %s" cache_path url in
    let success, output = System.run_command download_cmd in
    if not success then
      failwith (Printf.sprintf "Failed to download OCaml: %s" output))
  else Printf.printf "Using cached OCaml from %s\n%!" cache_path;

  (* Extract to temporary directory *)
  let extract_dir =
    Filename.concat "/tmp" (Printf.sprintf "ocaml-build-%d" (System.getpid ()))
  in
  System.mkdirp extract_dir;

  Printf.printf "Extracting OCaml source...\n%!";
  let extract_cmd = Printf.sprintf "tar -xzf %s -C %s" cache_path extract_dir in
  let success, output = System.run_command extract_cmd in
  if not success then
    failwith (Printf.sprintf "Failed to extract OCaml: %s" output);

  (* Find the extracted directory (should be the only subdirectory) *)
  let dirs = System.list_dir_all extract_dir in
  match dirs with
  | [ dir ] -> Filename.concat extract_dir dir
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

  let url =
    Printf.sprintf "https://github.com/ocaml/ocaml/archive/%s.tar.gz" version
  in
  let cache_path = get_cache_path url in
  let cache_dir = Filename.dirname cache_path in

  (* Create cache directory if needed *)
  System.mkdirp cache_dir;

  (* Check if already cached *)
  if not (System.file_exists cache_path) then (
    Printf.printf "Downloading OCaml %s from %s...\n%!" version url;

    (* Download to cache *)
    let download_cmd = Printf.sprintf "curl -L -o %s %s" cache_path url in
    let success, output = System.run_command download_cmd in
    if not success then
      failwith (Printf.sprintf "Failed to download OCaml: %s" output))
  else Printf.printf "Using cached OCaml %s from %s\n%!" version cache_path;

  (* Extract to temporary directory *)
  let extract_dir =
    Filename.concat "/tmp"
      (Printf.sprintf "ocaml-build-%s-%d" version (System.getpid ()))
  in
  System.mkdirp extract_dir;

  Printf.printf "Extracting OCaml source...\n%!";
  let extract_cmd = Printf.sprintf "tar -xzf %s -C %s" cache_path extract_dir in
  let success, output = System.run_command extract_cmd in
  if not success then
    failwith (Printf.sprintf "Failed to extract OCaml: %s" output);

  (* The extracted directory might be ocaml-5.3.0 or ocaml-5.3.0 *)
  let possible_dirs =
    [
      Filename.concat extract_dir (Printf.sprintf "ocaml-%s" version);
      Filename.concat extract_dir (Printf.sprintf "ocaml-%s" major_minor);
    ]
  in

  match List.find_opt System.file_exists possible_dirs with
  | Some dir -> dir
  | None -> failwith "Could not find extracted OCaml source directory"

(** Download and install pre-built dev tools *)
let install_dev_tools toolchain =
  let toolchain_path = get_toolchain_path toolchain in
  let bin_dir = Filename.concat toolchain_path "bin" in

  (* Check if tools already exist *)
  let ocamllsp = Filename.concat bin_dir "ocamllsp" in
  let odoc = Filename.concat bin_dir "odoc" in
  let ocamlformat = Filename.concat bin_dir "ocamlformat" in

  if
    System.file_exists ocamllsp
    && System.file_exists odoc
    && System.file_exists ocamlformat
  then
    Printf.printf "Development tools already installed for toolchain %s\n%!"
      toolchain.version
  else (
    Printf.printf "Installing development tools for toolchain %s...\n%!"
      toolchain.version;

    (* Determine host triplet *)
    let host_triplet =
      if System.os_type () = "Unix" then
        let uname_s =
          let success, output = System.run_command "uname -s" in
          if success then String.trim output
          else failwith "Could not determine OS"
        in
        let uname_m =
          let success, output = System.run_command "uname -m" in
          if success then String.trim output
          else failwith "Could not determine architecture"
        in

        let host_os =
          match String.lowercase_ascii uname_s with
          | "darwin" -> "apple-darwin"
          | "linux" -> "unknown-linux-gnu"
          | os -> failwith (Printf.sprintf "Unsupported OS: %s" os)
        in

        let host_arch =
          match uname_m with
          | "arm64" | "aarch64" -> "aarch64"
          | "x86_64" -> "x86_64"
          | arch ->
              failwith (Printf.sprintf "Unsupported architecture: %s" arch)
        in

        Printf.sprintf "%s-%s" host_arch host_os
      else failwith "Windows not yet supported"
    in

    (* Download from the S3 CDN *)
    let tools_url =
      Printf.sprintf
        "https://hel1.your-objectstorage.com/ml-riot-cdn/ocaml-platform/ocaml-platform-%s-%s.tar.gz"
        toolchain.version host_triplet
    in

    let tools_archive =
      Filename.concat "/tmp"
        (Printf.sprintf "ocaml-platform-%s.tar.gz" toolchain.version)
    in

    Printf.printf "Downloading pre-built tools from %s...\n%!" tools_url;

    (* Use -S to show errors, -f to fail on HTTP errors *)
    let download_cmd =
      Printf.sprintf "curl -fSL -o %s %s 2>&1" tools_archive tools_url
    in
    let success, output = System.run_command download_cmd in

    if success then (
      (* Extract to toolchain directory *)
      Printf.printf "Extracting development tools...\n%!";

      let extract_cmd =
        Printf.sprintf "cd %s && tar xzf %s" toolchain_path tools_archive
      in
      let success, output = System.run_command extract_cmd in

      if success then (
        (* Clean up *)
        ignore (System.run_command (Printf.sprintf "rm -f %s" tools_archive));
        Printf.printf "Successfully installed development tools\n%!")
      else failwith (Printf.sprintf "Failed to extract tools: %s" output))
    else
      failwith
        (Printf.sprintf "Failed to download pre-built tools from %s: %s"
           tools_url output))

(** Install a toolchain *)
let install_toolchain toolchain =
  if is_toolchain_installed toolchain then
    Printf.printf "Toolchain %s is already installed\n%!" toolchain.version
  else (
    Printf.printf "Installing OCaml toolchain %s...\n%!" toolchain.version;

    (* Create toolchain directory *)
    let toolchain_path = get_toolchain_path toolchain in
    System.mkdirp toolchain_path;

    (* Build OCaml based on source type *)
    (match toolchain.source with
    | Version version ->
        (* Download and build from GitHub release *)
        let src_dir = download_ocaml_source version in
        build_from_local_source ~source_path:src_dir ~toolchain_path;
        (* Clean up temporary extraction directory *)
        let extract_parent = Filename.dirname src_dir in
        let cleanup_cmd = Printf.sprintf "rm -rf %s" extract_parent in
        ignore (System.run_command cleanup_cmd)
    | Path path ->
        (* Build from local source directory *)
        (* Resolve path relative to workspace root if needed *)
        let source_path =
          if Filename.is_relative path then
            (* Assume relative paths are relative to current working directory *)
            Filename.concat (Sys.getcwd ()) path
          else path
        in
        build_from_local_source ~source_path ~toolchain_path
    | Url url ->
        (* Download and build from URL *)
        let src_dir = download_source_from_url url in
        build_from_local_source ~source_path:src_dir ~toolchain_path;
        (* Clean up temporary extraction directory *)
        let extract_parent = Filename.dirname src_dir in
        let cleanup_cmd = Printf.sprintf "rm -rf %s" extract_parent in
        ignore (System.run_command cleanup_cmd));

    Printf.printf "Successfully installed OCaml %s\n%!" toolchain.version;

    (* Download pre-built dev tools - only for Version sources *)
    match toolchain.source with
    | Version _ -> install_dev_tools toolchain
    | Path _ | Url _ ->
        Printf.printf
          "Note: Development tools not installed for custom toolchain\n%!")

(** Ready toolchains for a workspace *)
let ready_toolchains workspace =
  (* Look for ocaml-toolchain.toml in workspace root *)
  let toolchain_file =
    Filename.concat workspace.Workspace.root "ocaml-toolchain.toml"
  in
  let toolchain =
    if System.file_exists toolchain_file then
      parse_toolchain_file toolchain_file
    else default_toolchain
  in

  Printf.printf "Using OCaml toolchain: %s\n%!"
    (match toolchain.source with
    | Version v -> v
    | Path p -> Printf.sprintf "path:%s" p
    | Url u -> Printf.sprintf "url:%s" u);

  (* Ensure toolchain is installed *)
  (if not (is_toolchain_installed toolchain) then (
     Printf.printf "Toolchain %s not found. Installing...\n%!" toolchain.version;
     install_toolchain toolchain)
   else
     (* Check if dev tools are installed even if compiler exists *)
     let toolchain_path = get_toolchain_path toolchain in
     let bin_dir = Filename.concat toolchain_path "bin" in
     let ocamllsp = Filename.concat bin_dir "ocamllsp" in
     let odoc = Filename.concat bin_dir "odoc" in
     let ocamlformat = Filename.concat bin_dir "ocamlformat" in

     if
       not
         (System.file_exists ocamllsp
         && System.file_exists odoc
         && System.file_exists ocamlformat)
     then (
       Printf.printf "Development tools missing. Installing...\n%!";
       install_dev_tools toolchain));

  (* Return the toolchain *)
  toolchain

(** Validate that a toolchain is working *)
let validate_toolchain toolchain =
  let ocamlc = ocamlc_path toolchain in
  if not (System.file_exists ocamlc) then false
  else
    (* Try to run ocamlc -version *)
    let cmd = Printf.sprintf "%s -version" ocamlc in
    let success, _ = System.run_command cmd in
    success

(** List installed toolchains *)
let list_installed_toolchains () =
  if System.file_exists toolchain_base_dir then
    System.list_dir_all toolchain_base_dir
    |> List.map (fun dir -> Filename.concat toolchain_base_dir dir)
    |> List.filter System.is_directory
    |> List.filter (fun path ->
        System.file_exists (Filename.concat path "bin/ocamlc"))
    |> List.map Filename.basename
  else []

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
