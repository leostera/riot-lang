(** Toolchain management for tusk build system *)

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

type toolchain = { version : string (* TODO: Add more fields as needed *) }

let toolchain_base_dir = Filename.concat (System.get_home ()) ".tusk/toolchains"
let get_toolchain_path version = Filename.concat toolchain_base_dir version

let ocamlc_path version =
  Filename.concat (get_toolchain_path version) "bin/ocamlc"

let ocamlopt_path version =
  Filename.concat (get_toolchain_path version) "bin/ocamlopt"

let ocamldep_path version =
  Filename.concat (get_toolchain_path version) "bin/ocamldep"

(** Parse ocaml-toolchain.toml file *)
let parse_toolchain_file path =
  try
    let toml = Toml.parse_file path in
    (* Look for toolchain.version *)
    let version =
      match Toml.find_value "toolchain.version" toml with
      | Some value -> (
          match Toml.get_string value with
          | Some v -> v
          | None -> "5.3.0" (* Default version *))
      | None -> "5.3.0" (* Default version *)
    in
    { version }
  with _ ->
    (* Default to stable version if file doesn't exist or can't be parsed *)
    { version = "5.3.0" }

(** Check if a toolchain is installed *)
let is_toolchain_installed version =
  let toolchain_path = get_toolchain_path version in
  let ocamlc = ocamlc_path version in
  System.file_exists toolchain_path && System.file_exists ocamlc

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
let install_dev_tools version =
  let toolchain_path = get_toolchain_path version in
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
      version
  else (
    Printf.printf "Installing development tools for toolchain %s...\n%!" version;

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
        version host_triplet
    in

    let tools_archive =
      Filename.concat "/tmp" (Printf.sprintf "ocaml-platform-%s.tar.gz" version)
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

(** Install a toolchain version *)
let install_toolchain version =
  if is_toolchain_installed version then
    Printf.printf "Toolchain %s is already installed\n%!" version
  else (
    Printf.printf "Installing OCaml toolchain %s...\n%!" version;

    (* Create toolchain directory *)
    let toolchain_path = get_toolchain_path version in
    System.mkdirp toolchain_path;

    (* Download and build OCaml *)
    let src_dir = download_ocaml_source version in

    (* Configure *)
    Printf.printf "Configuring OCaml %s...\n%!" version;
    let configure_cmd =
      Printf.sprintf "cd %s && ./configure --prefix=%s --disable-ocamldoc"
        src_dir toolchain_path
    in
    let success, output = System.run_command configure_cmd in
    if not success then
      failwith (Printf.sprintf "Failed to configure OCaml: %s" output);

    (* Build *)
    Printf.printf "Building OCaml %s (this may take a while)...\n%!" version;
    let num_cores = System.cpu_count () in
    Printf.printf "Using %d cores for compilation\n%!" num_cores;
    let make_cmd = Printf.sprintf "cd %s && make -j%d" src_dir num_cores in
    let success, output = System.run_command make_cmd in
    if not success then
      failwith (Printf.sprintf "Failed to build OCaml: %s" output);

    (* Install *)
    Printf.printf "Installing OCaml %s...\n%!" version;
    let install_cmd = Printf.sprintf "cd %s && make install" src_dir in
    let success, output = System.run_command install_cmd in
    if not success then
      failwith (Printf.sprintf "Failed to install OCaml: %s" output);

    (* Clean up temporary extraction directory *)
    let extract_parent = Filename.dirname src_dir in
    let cleanup_cmd = Printf.sprintf "rm -rf %s" extract_parent in
    ignore (System.run_command cleanup_cmd);

    Printf.printf "Successfully installed OCaml %s\n%!" version;

    (* Download pre-built dev tools *)
    install_dev_tools version)

(** Ready toolchains for a workspace *)
let ready_toolchains workspace_root =
  (* Look for ocaml-toolchain.toml in workspace root *)
  let toolchain_file = Filename.concat workspace_root "ocaml-toolchain.toml" in
  let toolchain =
    if System.file_exists toolchain_file then
      parse_toolchain_file toolchain_file
    else { version = "5.3.0" }
    (* Default version *)
  in

  Printf.printf "Using OCaml toolchain: %s\n%!" toolchain.version;

  (* Ensure toolchain is installed *)
  (if not (is_toolchain_installed toolchain.version) then (
     Printf.printf "Toolchain %s not found. Installing...\n%!" toolchain.version;
     install_toolchain toolchain.version)
   else
     (* Check if dev tools are installed even if compiler exists *)
     let toolchain_path = get_toolchain_path toolchain.version in
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
       install_dev_tools toolchain.version));

  (* Return the toolchain *)
  toolchain

(** Validate that a toolchain is working *)
let validate_toolchain toolchain =
  let ocamlc = ocamlc_path toolchain.version in
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

(** Switch to a different toolchain version *)
let switch_toolchain version =
  if is_toolchain_installed version then
    Printf.printf "Switched to toolchain %s\n" version
  else
    Printf.printf
      "Toolchain %s is not installed. Run 'tusk toolchain install %s' first.\n"
      version version
