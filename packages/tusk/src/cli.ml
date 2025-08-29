(** CLI module - handles command-line interface *)

open Miniriot
open Std.Data

(** Format an event for cargo-style output *)
let format_cargo_event (event : Event.t) =
  match event.kind with
  | PackageStarted { package = _ } ->
      "" (* Don't show on start - wait for cache status *)
  | PackageComplete { package; success; errors; _ } ->
      if success then "" (* Already shown as "Compiling" *)
      else if errors = [] then ""
        (* Skipped due to dependency failure, don't show *)
      else Printf.sprintf "   \027[1;31mFailed\027[0m %s" package
  | PackageSkipped _ -> "" (* Don't show skipped packages *)
  | CacheHit { package; _ } ->
      Printf.sprintf
        "   \027[1;32mCompiling\027[0m %s \027[1;90m(cached)\027[0m" package
  | CacheMiss { package; _ } ->
      Printf.sprintf "   \027[1;32mCompiling\027[0m %s" package
  | CompileError { package = _; error } ->
      (* Just display the raw compiler output for best fidelity *)
      error.raw
  | BuildComplete { duration_ms; succeeded; failed; _ } ->
      if List.length failed = 0 then
        Printf.sprintf "   \027[1;32mFinished\027[0m in %.2fs"
          (float_of_int duration_ms /. 1000.0)
      else
        Printf.sprintf "   \027[1;31mFailed\027[0m with %d errors"
          (List.length failed)
  | _ -> ""

(** Helper to create a tusk client connected to the local server *)
let create_local_client () =
  let cwd =
    Std.Env.current_dir ()
    |> Std.Result.expect ~msg:"Failed to get current directory"
  in
  let workspace =
    Workspace_manager.scan cwd
    |> Std.Result.expect ~msg:"Failed to scan workspace"
  in
  Server_manager.ensure_running ~workspace
  |> Std.Result.expect ~msg:"Failed to connect to server"

let usage_msg =
  "tusk - OCaml build system\n\n\
   Usage: tusk [COMMAND] [OPTIONS]\n\n\
   Commands:\n\
  \  build    Build packages\n\
  \  clean    Clean build artifacts\n\
  \  doc      Generate documentation\n\
  \  fmt      Format OCaml code\n\
  \  help     Show this help message\n\n\
  \  install  Install a package binary to ~/.tusk/bin/\n\
  \  lsp      Start OCaml LSP server\n\
  \  mcp      Start Model Context Protocol server\n\
  \  new      Create a new package\n\
  \  rpc      Send RPC command to server\n\
  \  run      Run a binary\n\
  \  server   Start the tusk server (for debugging)\n\
  \  version  Show tusk version\n\
   Options:\n\
  \  -p <package>    Build only the specified package\n\
  \  -b <binary>     Run the specified binary"

(** Parse command line arguments for build command *)
let parse_build_args args start_idx =
  let rec parse idx package =
    if idx >= Array.length args then package
    else
      match args.(idx) with
      | "-p" when idx + 1 < Array.length args ->
          parse (idx + 2) (Some args.(idx + 1))
      | _ ->
          Printf.eprintf "Warning: Unknown argument '%s'\n" args.(idx);
          parse (idx + 1) package
  in
  parse start_idx None

(** Parse command line arguments for run command *)
let parse_run_args args start_idx =
  let rec parse idx binary =
    if idx >= Array.length args then binary
    else
      match args.(idx) with
      | "-b" when idx + 1 < Array.length args ->
          parse (idx + 2) (Some args.(idx + 1))
      | _ ->
          Printf.eprintf "Warning: Unknown argument '%s'\n" args.(idx);
          parse (idx + 1) binary
  in
  parse start_idx None

(** Find all available binaries in the workspace *)
let find_binaries () =
  let root =
    Fs.getcwd () |> Result.expect ~msg:"Failed to get cwd" |> Path.to_string
  in
  let workspace = Workspace_manager.get_workspace ~root in

  (* Look for packages that have main.ml files (indicating they produce binaries) *)
  List.filter_map
    (fun package ->
      let main_ml_path =
        Filename.concat
          (Std.Path.to_string package.Workspace.path)
          "src/main.ml"
      in
      if File_utils.exists ~path:main_ml_path then Some package.Workspace.name
      else None)
    workspace.packages

(** Get the path to a binary *)
let get_binary_path binary_name =
  let root =
    Fs.getcwd () |> Result.expect ~msg:"Failed to get cwd" |> Path.to_string
  in
  let target_path = Filename.concat root "target/debug/out" in

  (* Try different possible locations *)
  let possible_paths =
    [
      Filename.concat target_path binary_name;
      Filename.concat
        (Filename.concat target_path ("packages/" ^ binary_name))
        binary_name;
      Filename.concat (Filename.concat root "target/bootstrap") binary_name;
      Filename.concat (Filename.concat root "target/debug") binary_name;
    ]
  in

  List.find_opt
    (fun path ->
      try
        match
          Fs.stat (Path.of_string path |> Result.expect ~msg:"Invalid path")
        with
        | Ok stat -> (
            match Std.File.kind_of_unix stat.st_kind with
            | Std.File.Regular -> true
            | _ -> false)
        | Error _ -> false
      with _ -> false)
    possible_paths

(** Execute the build command *)
let build_command package_opt =
  (* Make sure we have a valid workspace *)
  let cwd =
    Std.Env.current_dir ()
    |> Std.Result.expect ~msg:"Failed to get current directory"
  in
  let workspace =
    Workspace_manager.scan cwd
    |> Std.Result.expect
         ~msg:"Failed to scan workspace. Is this a valid tusk project?"
  in

  (* Ensure server is running *)
  let client =
    Server_manager.ensure_running ~workspace
    |> Std.Result.expect ~msg:"Failed to start or connect to tusk server"
  in

  let open Tusk_jsonrpc in
  let request =
    match package_opt with
    | Some pkg -> Client.BuildPackage pkg
    | None -> Client.BuildAll
  in
  (* Track packages we've already displayed to avoid duplicates *)
  let displayed_packages = Hashtbl.create 32 in
  let result =
    Client.build_streaming client request (fun event ->
        match event with
        | Client.BuildStarted session_id -> ()
        | Client.BuildEvent event ->
            (* Only display package events once *)
            let should_display =
              match event.kind with
              | CacheHit { package; _ } | CacheMiss { package; _ } ->
                  if Hashtbl.mem displayed_packages package then false
                  else (
                    Hashtbl.add displayed_packages package ();
                    true)
              | PackageComplete { package; _ } -> (
                  (* Always show failures, but not successes (already shown as Compiling) *)
                  match event.kind with
                  | PackageComplete { success = false; _ } -> true
                  | _ -> false)
              | _ -> true
            in
            if should_display then
              let formatted = format_cargo_event event in
              if formatted <> "" then Printf.printf "%s\n%!" formatted
        | Client.BuildFinished _ -> ())
    |> Std.Result.expect ~msg:"Build failed"
  in
  Client.close client;

  (* Print final result *)
  match result with
  | Client.BuildFinished (Ok ()) -> Ok ()
  | Client.BuildFinished (Error msg) ->
      Printf.eprintf "error: build failed: %s\n" msg;
      Error (Failure "Build failed")

(** Execute a binary and handle its exit status *)
let execute_binary binary_name binary_path =
  Printf.printf "🚀 Running %s...\n" binary_name;
  let result = Command.system binary_path in
  match Std.Command.of_unix_status result with
  | Std.Command.Exited code ->
      if code = 0 then Ok ()
      else Error (Failure (Printf.sprintf "Binary exited with code %d" code))
  | Std.Command.Signaled signal ->
      Error (Failure (Printf.sprintf "Binary killed by signal %d" signal))
  | Std.Command.Stopped signal ->
      Error (Failure (Printf.sprintf "Binary stopped by signal %d" signal))

(** Execute the clean command *)
let clean_command () =
  Printf.printf "🧹 Cleaning build artifacts...\n";
  let result = Command.system "rm -rf ./target" in
  match Std.Command.of_unix_status result with
  | Std.Command.Exited 0 ->
      Printf.printf "Build artifacts cleaned!\n";
      Ok ()
  | _ -> Error (Failure "Failed to clean build artifacts")

(** Build a package and wait for completion *)
let build_package package_name =
  (* Get workspace *)
  let cwd =
    Std.Env.current_dir () |> Std.Result.expect ~msg:"Operation failed"
  in
  let workspace =
    Workspace_manager.scan cwd |> Std.Result.expect ~msg:"Operation failed"
  in
  (* Ensure server is running *)
  let client_result = Server_manager.ensure_running ~workspace in
  if Result.is_error client_result then false
  else
    (* Use JSON-RPC client to send build request *)
    let client = client_result |> Std.Result.expect ~msg:"Operation failed" in
    (* Track packages we've already displayed to avoid duplicates *)
    let displayed_packages = Hashtbl.create 32 in
    let result =
      Tusk_jsonrpc.Client.build_streaming client
        (Tusk_jsonrpc.Client.BuildPackage package_name) (function
        | Tusk_jsonrpc.Client.BuildStarted session_id ->
            (* Don't print session ID in Cargo style *)
            ()
        | Tusk_jsonrpc.Client.BuildEvent event ->
            (* Only display package events once *)
            let should_display =
              match event.kind with
              | CacheHit { package; _ } | CacheMiss { package; _ } ->
                  if Hashtbl.mem displayed_packages package then false
                  else (
                    Hashtbl.add displayed_packages package ();
                    true)
              | PackageComplete { package; success; errors; _ } ->
                  (* Always show failures with errors, but not successes or skips *)
                  success = false && errors <> []
              | _ -> true
            in
            if should_display then
              let formatted = format_cargo_event event in
              if formatted <> "" then (
                Printf.printf "%s\n" formatted;
                flush stdout)
        | Tusk_jsonrpc.Client.BuildFinished _ ->
            (* This is handled below in the result match *)
            ())
    in
    Tusk_jsonrpc.Client.close client;
    match result with
    | Ok (Tusk_jsonrpc.Client.BuildFinished (Ok ())) -> true
    | Ok (Tusk_jsonrpc.Client.BuildFinished (Error _)) -> false
    | Error _ -> false

(** Execute the run command *)
let run_command binary_opt =
  let available_binaries = find_binaries () in

  match binary_opt with
  | Some binary_name ->
      (* User specified a binary with -b flag *)
      if List.mem binary_name available_binaries then (
        (* Check if binary exists, if not build it first *)
        match get_binary_path binary_name with
        | Some binary_path ->
            (* Binary exists, run it directly *)
            execute_binary binary_name binary_path
        | None ->
            (* Binary doesn't exist, build it first *)
            Printf.printf "📦 Binary '%s' not found, building first...\n"
              binary_name;
            if build_package binary_name then (
              (* Build succeeded, try to run again *)
              match get_binary_path binary_name with
              | Some binary_path -> execute_binary binary_name binary_path
              | None ->
                  Printf.eprintf
                    "Error: Binary '%s' still not found after building.\n"
                    binary_name;
                  Error
                    (Failure
                       (Printf.sprintf "Binary '%s' not found after build"
                          binary_name)))
            else (
              Printf.eprintf "Error: Failed to build package '%s'.\n"
                binary_name;
              Error (Failure (Printf.sprintf "Build failed for %s" binary_name))))
      else (
        Printf.eprintf "Error: Binary '%s' not found in workspace.\n"
          binary_name;
        Printf.eprintf "Available binaries: %s\n"
          (String.concat ", " available_binaries);
        Error (Failure (Printf.sprintf "Unknown binary: %s" binary_name)))
  | None -> (
      (* No binary specified *)
      match available_binaries with
      | [] ->
          Printf.eprintf "Error: No binaries found in workspace.\n";
          Error (Failure "No binaries found")
      | [ single_binary ] -> (
          (* Only one binary available, run it *)
          match get_binary_path single_binary with
          | Some binary_path -> execute_binary single_binary binary_path
          | None ->
              (* Binary doesn't exist, build it first *)
              Printf.printf "📦 Binary '%s' not found, building first...\n"
                single_binary;
              if build_package single_binary then (
                (* Build succeeded, try to run again *)
                match get_binary_path single_binary with
                | Some binary_path -> (
                    Printf.printf "🚀 Running %s...\n" single_binary;
                    let result = Command.system binary_path in
                    match Std.Command.of_unix_status result with
                    | Std.Command.Exited code ->
                        if code = 0 then Ok ()
                        else
                          Error
                            (Failure
                               (Printf.sprintf "Binary exited with code %d" code))
                    | Std.Command.Signaled signal ->
                        Error
                          (Failure
                             (Printf.sprintf "Binary killed by signal %d" signal))
                    | Std.Command.Stopped signal ->
                        Error
                          (Failure
                             (Printf.sprintf "Binary stopped by signal %d"
                                signal)))
                | None ->
                    Printf.eprintf
                      "Error: Binary '%s' still not found after building.\n"
                      single_binary;
                    Error
                      (Failure
                         (Printf.sprintf "Binary '%s' not found after build"
                            single_binary)))
              else (
                Printf.eprintf "Error: Failed to build package '%s'.\n"
                  single_binary;
                Error
                  (Failure (Printf.sprintf "Build failed for %s" single_binary)))
          )
      | _ ->
          (* Multiple binaries available, user must specify *)
          Printf.eprintf
            "Error: Multiple binaries found. Please specify which one to run \
             with -b flag.\n";
          Printf.eprintf "Available binaries: %s\n"
            (String.concat ", " available_binaries);
          Error (Failure "Multiple binaries found"))

(** Read OCaml toolchain version from ocaml-toolchain.toml *)
let read_toolchain_version () =
  let toml_path = "ocaml-toolchain.toml" in
  if not (File_utils.exists ~path:toml_path) then
    failwith "ocaml-toolchain.toml not found in current directory"
  else
    let ic = open_in toml_path in
    let rec find_version () =
      try
        let line = input_line ic in
        if String.contains line '=' then
          let trimmed_line = String.trim line in
          if
            String.length trimmed_line >= 7
            && String.sub trimmed_line 0 7 = "version"
          then
            let parts = String.split_on_char '=' line in
            if List.length parts >= 2 then (
              let version_part = List.nth parts 1 in
              let trimmed = String.trim version_part in
              (* Remove quotes *)
              let unquoted =
                if
                  String.length trimmed >= 2
                  && trimmed.[0] = '"'
                  && trimmed.[String.length trimmed - 1] = '"'
                then String.sub trimmed 1 (String.length trimmed - 2)
                else trimmed
              in
              close_in ic;
              unquoted)
            else find_version ()
          else find_version ()
        else find_version ()
      with End_of_file ->
        close_in ic;
        failwith "version not found in ocaml-toolchain.toml"
    in
    find_version ()

(** Get path to LSP binary in toolchain *)
let get_lsp_binary_path () =
  let version = read_toolchain_version () in
  let home =
    match Env.home_dir () with
    | Some h -> Path.to_string h
    | None -> failwith "Failed to get home"
  in
  let lsp_path =
    Printf.sprintf "%s/.tusk/toolchains/%s/bin/ocamllsp" home version
  in
  if File_utils.exists ~path:lsp_path then lsp_path
  else
    failwith
      (Printf.sprintf
         "OCaml LSP server not found at %s\n\
          Run 'tusk toolchain install' to install development tools"
         lsp_path)

(** Execute the LSP command *)
let rec lsp_command args =
  (* Parse subcommand if provided *)
  let subcommand = if Array.length args > 2 then args.(2) else "" in
  match subcommand with
  | "ocaml-merlin" ->
      (* Start the merlin bridge for ocaml-lsp-server integration *)
      let cwd =
        Std.Env.current_dir () |> Std.Result.expect ~msg:"Operation failed"
      in
      let workspace =
        Workspace_manager.scan cwd |> Std.Result.expect ~msg:"Operation failed"
      in
      Merlin_bridge.start ~workspace;
      Ok ()
  | "ocamlformat-rpc" ->
      (* Bridge to ocamlformat-rpc from toolchain *)
      let toolchain_dir =
        Filename.concat
          (match Env.home_dir () with
          | Some h -> Path.to_string h
          | None -> failwith "Failed to get home")
          ".tusk/toolchains/5.3.0/bin"
      in
      let ocamlformat_rpc = Filename.concat toolchain_dir "ocamlformat-rpc" in
      if File_utils.exists ~path:ocamlformat_rpc then
        (* Pass through to ocamlformat-rpc with all remaining args *)
        let argv = Array.sub args 3 (Array.length args - 3) in
        Command.exec ocamlformat_rpc (Array.append [| "ocamlformat-rpc" |] argv)
      else (
        Printf.eprintf "Error: ocamlformat-rpc not found at %s\n"
          ocamlformat_rpc;
        Printf.eprintf "Please run: cd ocaml && ./local-install.sh\n";
        Error (Failure "ocamlformat-rpc not found"))
  | "" ->
      (* Default: Start OCaml LSP server *)
      lsp_start_server ()
  | _ ->
      Printf.eprintf "Unknown lsp subcommand: %s\n" subcommand;
      Printf.eprintf "Available subcommands:\n";
      Printf.eprintf "  tusk lsp                 - Start OCaml LSP server\n";
      Printf.eprintf "  tusk lsp ocaml-merlin    - Run merlin protocol bridge\n";
      Printf.eprintf "  tusk lsp ocamlformat-rpc - Run ocamlformat RPC server\n";
      Error (Failure "Invalid lsp subcommand")

(** Start the LSP server *)
and lsp_start_server () =
  try
    (* Try to ensure the tusk server is running, but don't fail if there's an issue *)
    let _ =
      try
        let cwd =
          Std.Env.current_dir () |> Std.Result.expect ~msg:"Operation failed"
        in
        let workspace =
          Workspace_manager.scan cwd
          |> Std.Result.expect ~msg:"Operation failed"
        in
        ignore (Server_manager.ensure_running ~workspace);
        ()
      with _ ->
        (* Server might already be running or there might be an issue - continue anyway *)
        ()
    in

    let lsp_path = get_lsp_binary_path () in
    let version = read_toolchain_version () in
    let home =
      match Env.home_dir () with
      | Some h -> Path.to_string h
      | None -> failwith "Failed to get home"
    in
    let toolchain_path = Printf.sprintf "%s/.tusk/toolchains/%s" home version in
    let stdlib_path = Printf.sprintf "%s/lib/ocaml" toolchain_path in

    (* Check if .merlin file exists *)
    let merlin_exists = File_utils.exists ~path:".merlin" in
    if not merlin_exists then
      Printf.printf
        "⚠️  No .merlin file found. Run 'tusk build' to generate it.\n%!";

    Printf.printf "🔍 Starting OCaml LSP server...\n%!";
    Printf.printf "   Toolchain: %s\n%!" version;
    Printf.printf "   Stdlib: %s\n%!" stdlib_path;
    Printf.printf "   LSP binary: %s\n%!" lsp_path;
    Printf.printf "   Tusk server: running in background\n%!";
    if merlin_exists then
      Printf.printf "   Using .merlin file for configuration\n%!"
    else
      Printf.printf
        "   Warning: No .merlin file, LSP may have limited functionality\n%!";
    Printf.printf
      "\n💡 Connect your editor to the LSP server (usually automatic)\n%!";
    Printf.printf "   For VSCode: Install OCaml Platform extension\n%!";
    Printf.printf "   For Neovim: Configure nvim-lspconfig with ocamllsp\n%!";
    Printf.printf "   For Emacs: Use lsp-mode or eglot\n\n%!";

    (* Set up environment *)
    let _ = Env.putenv "OCAMLPATH" stdlib_path in
    let _ = Env.putenv "OCAMLLIB" stdlib_path in

    (* Add target/debug to PATH so ocamllsp can find tusk *)
    let current_path =
      match Env.getenv "PATH" with Ok path -> path | Error _ -> ""
    in
    let target_debug_path =
      Filename.concat
        (Fs.getcwd ()
        |> Result.expect ~msg:"Failed to get cwd"
        |> Path.to_string)
        "target/debug"
    in
    let new_path = Printf.sprintf "%s:%s" target_debug_path current_path in
    let _ = Env.putenv "PATH" new_path in

    (* Execute the LSP server with stdio by default *)
    let args =
      if Array.length (Command.argv ()) > 2 then
        (* Pass through any additional arguments after "lsp" *)
        Array.sub (Command.argv ()) 2 (Array.length (Command.argv ()) - 2)
      else
        (* Default to stdio mode *)
        [| "--stdio" |]
    in

    (* Build the full command *)
    let full_args = Array.append [| lsp_path |] args in

    (* Execute the LSP server *)
    let _ = Command.exec lsp_path full_args in
    (* This should never be reached if execv succeeds *)
    Error (Failure "Failed to execute LSP server")
  with
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      Error (Failure msg)
  | exn ->
      let error_msg =
        Printf.sprintf "LSP command failed: %s" (Printexc.to_string exn)
      in
      Printf.eprintf "Error: %s\n" error_msg;
      Error (Failure error_msg)

(** Get ocamlformat binary path *)
let get_ocamlformat_binary_path () =
  let version = read_toolchain_version () in
  let home =
    match Env.home_dir () with
    | Some h -> Path.to_string h
    | None -> failwith "Failed to get home"
  in
  let fmt_path =
    Printf.sprintf "%s/.tusk/toolchains/%s/bin/ocamlformat" home version
  in
  if File_utils.exists ~path:fmt_path then fmt_path
  else
    failwith
      (Printf.sprintf
         "ocamlformat not found at %s\n\
          Run 'tusk build' to install development tools"
         fmt_path)

(** Get odoc binary path *)
let get_odoc_binary_path () =
  let version = read_toolchain_version () in
  let home =
    match Env.home_dir () with
    | Some h -> Path.to_string h
    | None -> failwith "Failed to get home"
  in
  let odoc_path =
    Printf.sprintf "%s/.tusk/toolchains/%s/bin/odoc" home version
  in
  if File_utils.exists ~path:odoc_path then odoc_path
  else
    failwith
      (Printf.sprintf
         "odoc not found at %s\nRun 'tusk build' to install development tools"
         odoc_path)

(** Execute the fmt command *)
let fmt_command () =
  try
    let fmt_path = get_ocamlformat_binary_path () in

    (* Find all OCaml source files *)
    let root =
      Fs.getcwd () |> Result.expect ~msg:"Failed to get cwd" |> Path.to_string
    in
    let packages_dir = Filename.concat root "packages" in

    (* Use find to get all .ml and .mli files *)
    let find_cmd =
      Printf.sprintf "find %s -name '*.ml' -o -name '*.mli' 2>/dev/null"
        packages_dir
    in
    let ic = Command.open_process_in find_cmd in
    let files = ref [] in
    (try
       while true do
         files := input_line ic :: !files
       done
     with End_of_file -> ());
    ignore (Command.close_process_in ic);

    let file_list = List.rev !files in
    let total = List.length file_list in

    if total = 0 then (
      Printf.printf "No OCaml files found to format\n%!";
      Ok ())
    else (
      Printf.printf "🎨 Formatting %d OCaml files...\n%!" total;

      (* Format each file in place *)
      let formatted = ref 0 in
      List.iter
        (fun file ->
          let cmd = Printf.sprintf "%s -i %s 2>/dev/null" fmt_path file in
          let result = Command.system cmd in
          match Std.Command.of_unix_status result with
          | Std.Command.Exited 0 ->
              incr formatted;
              Printf.printf "   ✓ %s\n%!" file
          | _ -> Printf.printf "   ✗ %s (skipped)\n%!" file)
        file_list;

      Printf.printf "\n✨ Formatted %d/%d files successfully\n%!" !formatted
        total;
      Ok ())
  with
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      Error (Failure msg)
  | exn ->
      let error_msg =
        Printf.sprintf "Format command failed: %s" (Printexc.to_string exn)
      in
      Printf.eprintf "Error: %s\n" error_msg;
      Error (Failure error_msg)

(** Execute the doc command *)
let doc_command () =
  try
    let odoc_path = get_odoc_binary_path () in
    let root =
      Fs.getcwd () |> Result.expect ~msg:"Failed to get cwd" |> Path.to_string
    in
    let doc_dir = Filename.concat root "_doc" in

    Printf.printf "📚 Generating documentation...\n%!";
    Printf.printf "   Output directory: %s\n%!" doc_dir;

    (* Create doc directory *)
    let mkdir_cmd = Printf.sprintf "mkdir -p %s" doc_dir in
    ignore (Command.system mkdir_cmd);

    (* First, build all packages to ensure .cmi files exist *)
    Printf.printf "\n🔨 Building packages to generate interface files...\n%!";
    Printf.printf
      "   Run 'tusk build' first to ensure all packages are built\n%!";

    (* Find all .cmi files in target directory *)
    let target_dir = Filename.concat root "target/debug/out/packages" in
    let find_cmi_cmd =
      Printf.sprintf "find %s -name '*.cmi' 2>/dev/null" target_dir
    in
    let ic = Command.open_process_in find_cmi_cmd in
    let cmi_files = ref [] in
    (try
       while true do
         cmi_files := input_line ic :: !cmi_files
       done
     with End_of_file -> ());
    ignore (Command.close_process_in ic);

    let cmi_list = List.rev !cmi_files in

    if List.length cmi_list = 0 then (
      Printf.printf
        "\n⚠️  No compiled interface files found. Build the project first.\n%!";
      Ok ())
    else (
      Printf.printf "\n📝 Processing %d interface files...\n%!"
        (List.length cmi_list);

      (* Generate .odoc files from .cmi files *)
      List.iter
        (fun cmi_file ->
          let basename = Filename.basename cmi_file in
          let modname = Filename.chop_extension basename in
          let odoc_file = Filename.concat doc_dir (modname ^ ".odoc") in

          let cmd =
            Printf.sprintf "%s compile %s -o %s 2>/dev/null" odoc_path cmi_file
              odoc_file
          in
          let result = Command.system cmd in
          match Std.Command.of_unix_status result with
          | Std.Command.Exited 0 -> Printf.printf "   ✓ %s\n%!" modname
          | _ -> Printf.printf "   ✗ %s (failed)\n%!" modname)
        cmi_list;

      (* Generate HTML from .odoc files *)
      Printf.printf "\n🌐 Generating HTML documentation...\n%!";
      let html_dir = Filename.concat doc_dir "html" in
      let cmd =
        Printf.sprintf "%s html-generate %s/*.odoc -o %s 2>/dev/null" odoc_path
          doc_dir html_dir
      in
      ignore (Command.system cmd);

      Printf.printf "\n✨ Documentation generated at: %s/html\n%!" doc_dir;
      Printf.printf "   Open %s/html/index.html in your browser\n%!" doc_dir;
      Ok ())
  with
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      Error (Failure msg)
  | exn ->
      let error_msg =
        Printf.sprintf "Doc command failed: %s" (Printexc.to_string exn)
      in
      Printf.eprintf "Error: %s\n" error_msg;
      Error (Failure error_msg)

(** Show help message *)
let help_command () =
  Printf.printf "%s\n" usage_msg;
  Ok ()

(** Show version *)
let version_command () =
  Printf.printf "%s\n" Version.version;
  Ok ()

(** Execute the server command *)
let server_command args =
  (* Parse subcommand if provided *)
  let subcommand = if Array.length args > 2 then args.(2) else "" in
  match subcommand with
  | "start" ->
      (* Start server in background *)
      Printf.printf "Server start not implemented yet\n";
      Ok ()
  | "stop" ->
      (* Stop background server *)
      Printf.printf "Server stop not implemented yet\n";
      Ok ()
  | "kill" ->
      (* Kill background server forcefully *)
      Printf.printf "Server kill not implemented yet\n";
      Ok ()
  | "status" ->
      (* Check server status *)
      Printf.printf "Server status not implemented yet\n";
      Ok ()
  | "" | "foreground" ->
      (* Default: Run server in foreground *)
      Printf.printf "🚀 Starting tusk server...\n";
      Printf.printf "   Press Ctrl+C to stop\n\n";
      Tusk_server.start_with_listener ()
  | _ ->
      Printf.eprintf "Unknown server subcommand: %s\n" subcommand;
      Printf.eprintf "Available subcommands:\n";
      Printf.eprintf "  tusk server            - Start server in foreground\n";
      Printf.eprintf "  tusk server start      - Start server in background\n";
      Printf.eprintf "  tusk server stop       - Stop background server\n";
      Printf.eprintf
        "  tusk server kill       - Kill background server (force)\n";
      Printf.eprintf "  tusk server status     - Check server status\n";
      Error (Failure "Invalid server subcommand")

(** Execute the rpc command *)
let rpc_command args =
  let cmd = if Array.length args > 2 then args.(2) else "" in
  let rest =
    if Array.length args > 3 then
      Array.to_list (Array.sub args 3 (Array.length args - 3))
    else []
  in

  (* Show help if no subcommand provided *)
  if cmd = "" then (
    Printf.printf "Available RPC commands:\n";
    Printf.printf "  tusk rpc ping              - Test server connectivity\n";
    Printf.printf "  tusk rpc workspace         - Get workspace information\n";
    Printf.printf
      "  tusk rpc package <name>    - Get package details including sources\n";
    Printf.printf "  tusk rpc graph             - Get build graph\n";
    Printf.printf
      "  tusk rpc build [package]   - Build all or specific package\n";
    Printf.printf "  tusk rpc restart           - Restart the server\n";
    Printf.printf "  tusk rpc shutdown          - Shutdown the server\n";
    Ok ())
  else if cmd = "ping" then (
    let client = create_local_client () in
    let result = Tusk_jsonrpc.Client.ping client in
    Tusk_jsonrpc.Client.close client;
    match result with
    | Ok () ->
        let json = Json.Object [ ("type", Json.String "pong") ] in
        Printf.printf "%s\n" (Json.to_string json);
        Ok ()
    | Error e ->
        Printf.eprintf "Error: %s\n" e;
        Error (Failure e))
  else if cmd = "workspace" then (
    let client = create_local_client () in
    let result = Tusk_jsonrpc.Client.get_workspace_config client in
    Tusk_jsonrpc.Client.close client;
    match result with
    | Ok config ->
        let json =
          Json.Object
            [
              ("type", Json.String "workspace_config");
              ( "workspace_root",
                Json.String config.Tusk_jsonrpc.TuskProtocol.workspace_root );
              ( "target_dir",
                Json.String config.Tusk_jsonrpc.TuskProtocol.target_dir );
              ( "toolchain",
                Json.String config.Tusk_jsonrpc.TuskProtocol.toolchain );
              ( "toolchain_path",
                Json.String config.Tusk_jsonrpc.TuskProtocol.toolchain_path );
              ( "packages",
                Json.Array
                  (List.map
                     (fun (pkg : Tusk_jsonrpc.TuskProtocol.package_info) ->
                       Json.Object
                         [
                           ("name", Json.String pkg.name);
                           ("path", Json.String pkg.path);
                           ( "dependencies",
                             Json.Array
                               (List.map
                                  (fun d -> Json.String d)
                                  pkg.dependencies) );
                         ])
                     config.Tusk_jsonrpc.TuskProtocol.packages) );
              ( "total_packages",
                Json.Int config.Tusk_jsonrpc.TuskProtocol.total_packages );
            ]
        in
        Printf.printf "%s\n" (Json.to_string json);
        Ok ()
    | Error e ->
        Printf.eprintf "Error: %s\n" e;
        Error (Failure e))
  else if cmd = "package" then
    match rest with
    | [] ->
        Printf.eprintf "Error: package name required\n";
        Printf.eprintf "Usage: tusk rpc package <package-name>\n";
        Error (Failure "Missing package name")
    | package_name :: _ -> (
        let client = create_local_client () in
        let result = Tusk_jsonrpc.Client.get_package_info client package_name in
        Tusk_jsonrpc.Client.close client;
        match result with
        | Ok detail ->
            let json =
              Json.Object
                [
                  ("type", Json.String "package_info");
                  ( "package",
                    Json.Object
                      [
                        ( "name",
                          Json.String
                            detail.Tusk_jsonrpc.TuskProtocol.package.name );
                        ( "path",
                          Json.String
                            detail.Tusk_jsonrpc.TuskProtocol.package.path );
                        ( "dependencies",
                          Json.Array
                            (List.map
                               (fun d -> Json.String d)
                               detail.Tusk_jsonrpc.TuskProtocol.package
                                 .dependencies) );
                      ] );
                  ( "sources",
                    Json.Array
                      (List.map
                         (fun s -> Json.String s)
                         detail.Tusk_jsonrpc.TuskProtocol.sources) );
                  ( "dependency_names",
                    Json.Array
                      (List.map
                         (fun d -> Json.String d)
                         detail.Tusk_jsonrpc.TuskProtocol.dependency_names) );
                ]
            in
            Printf.printf "%s\n" (Json.to_string json);
            Ok ()
        | Error e ->
            Printf.eprintf "Error: %s\n" e;
            Error (Failure e))
  else if cmd = "graph" then (
    let client = create_local_client () in
    let result = Tusk_jsonrpc.Client.get_build_graph client in
    Tusk_jsonrpc.Client.close client;
    match result with
    | Ok response ->
        let nodes_json =
          List.map
            (fun node ->
              Json.Object
                [
                  ( "name",
                    Json.String node.Tusk_jsonrpc.TuskProtocol.package_name );
                  ("status", Json.String node.Tusk_jsonrpc.TuskProtocol.status);
                  ( "dependencies",
                    Json.Array
                      (List.map
                         (fun d -> Json.String d)
                         node.Tusk_jsonrpc.TuskProtocol.deps) );
                ])
            response.Tusk_jsonrpc.TuskProtocol.nodes
        in
        let json =
          Json.Object
            [
              ("type", Json.String "build_graph");
              ("nodes", Json.Array nodes_json);
            ]
        in
        Printf.printf "%s\n" (Json.to_string json);
        Ok ()
    | Error e ->
        Printf.eprintf "Error: %s\n" e;
        Error (Failure e))
  else if cmd = "build" then (
    (* Parse optional package name *)
    let package = if Array.length args > 3 then Some args.(3) else None in
    let request =
      match package with
      | Some pkg -> Tusk_jsonrpc.Client.BuildPackage pkg
      | None -> Tusk_jsonrpc.Client.BuildAll
    in
    let client = create_local_client () in
    let session_id = ref None in
    let callback = function
      | Tusk_jsonrpc.Client.BuildStarted sid ->
          session_id := Some sid;
          let dt = Std.Datetime.now () in
          let timestamp = Std.Datetime.to_iso8601 dt in
          let json =
            Json.Object
              [
                ("type", Json.String "build_started");
                ("timestamp", Json.String timestamp);
                ("session_id", Json.String (Session_id.to_string sid));
              ]
          in
          Printf.printf "%s\n" (Json.to_string json);
          flush stdout
      | Tusk_jsonrpc.Client.BuildEvent event ->
          (* Use Event.to_json for all events *)
          let json = Event.to_json event in
          Printf.printf "%s\n" (Json.to_string json);
          flush stdout
      | Tusk_jsonrpc.Client.BuildFinished result ->
          let json =
            match result with
            | Ok () -> Json.Object [ ("type", Json.String "success") ]
            | Error msg ->
                Json.Object
                  [
                    ("type", Json.String "error"); ("message", Json.String msg);
                  ]
          in
          Printf.printf "%s\n" (Json.to_string json);
          flush stdout
    in
    let result = Tusk_jsonrpc.Client.build_streaming client request callback in
    Tusk_jsonrpc.Client.close client;
    match result with
    | Ok _ -> Ok ()
    | Error e ->
        let response =
          Json.Object
            [ ("type", Json.String "Error"); ("message", Json.String e) ]
        in
        Printf.printf "%s\n" (Json.to_string response);
        Error (Failure e))
  else if cmd = "restart" then (
    let client = create_local_client () in
    let result = Tusk_jsonrpc.Client.restart client in
    Tusk_jsonrpc.Client.close client;
    match result with
    | Ok () ->
        let json = Json.Object [ ("type", Json.String "restarted") ] in
        Printf.printf "%s\n" (Json.to_string json);
        Ok ()
    | Error e ->
        Printf.eprintf "Error: %s\n" e;
        Error (Failure e))
  else if cmd = "shutdown" then (
    let client = create_local_client () in
    let result = Tusk_jsonrpc.Client.shutdown client in
    Tusk_jsonrpc.Client.close client;
    match result with
    | Ok () ->
        let json = Json.Object [ ("type", Json.String "shutdown") ] in
        Printf.printf "%s\n" (Json.to_string json);
        Ok ()
    | Error e ->
        Printf.eprintf "Error: %s\n" e;
        Error (Failure e))
  else if cmd = "format" then
    (* Format a file *)
    match rest with
    | [] ->
        Printf.eprintf "Error: file path required\n";
        Printf.eprintf "Usage: tusk rpc format <file-path>\n";
        Error (Failure "Missing file path")
    | file_path :: _ -> (
        let client = create_local_client () in
        let result =
          Tusk_jsonrpc.Client.format_file client ~file_path ~check_only:false
        in
        Tusk_jsonrpc.Client.close client;
        match result with
        | Ok (formatted_code, changed) ->
            let json =
              Json.Object
                [
                  ("type", Json.String "format_result");
                  ("formatted_code", Json.String formatted_code);
                  ("changed", Json.Bool changed);
                ]
            in
            Printf.printf "%s\n" (Json.to_string json);
            Ok ()
        | Error e ->
            Printf.eprintf "Error: %s\n" e;
            Error (Failure e))
  else if cmd = "format-check" then
    (* Check if a file needs formatting *)
    match rest with
    | [] ->
        Printf.eprintf "Error: file path required\n";
        Printf.eprintf "Usage: tusk rpc format-check <file-path>\n";
        Error (Failure "Missing file path")
    | file_path :: _ -> (
        let client = create_local_client () in
        let result =
          Tusk_jsonrpc.Client.format_file client ~file_path ~check_only:true
        in
        Tusk_jsonrpc.Client.close client;
        match result with
        | Ok (_formatted_code, changed) ->
            let json =
              Json.Object
                [
                  ("type", Json.String "format_check");
                  ("needs_formatting", Json.Bool changed);
                ]
            in
            Printf.printf "%s\n" (Json.to_string json);
            Ok ()
        | Error e ->
            Printf.eprintf "Error: %s\n" e;
            Error (Failure e))
  else if cmd = "format-code" then
    (* Format code string *)
    match rest with
    | [] ->
        Printf.eprintf "Error: code string required\n";
        Printf.eprintf "Usage: tusk rpc format-code <code-string> [file-hint]\n";
        Error (Failure "Missing code string")
    | code :: file_hint -> (
        let file_path = match file_hint with [] -> None | h :: _ -> Some h in
        let client = create_local_client () in
        let result = Tusk_jsonrpc.Client.format_code client ~code ~file_path in
        Tusk_jsonrpc.Client.close client;
        match result with
        | Ok (formatted_code, changed) ->
            let json =
              Json.Object
                [
                  ("type", Json.String "format_result");
                  ("formatted_code", Json.String formatted_code);
                  ("changed", Json.Bool changed);
                ]
            in
            Printf.printf "%s\n" (Json.to_string json);
            Ok ()
        | Error e ->
            Printf.eprintf "Error: %s\n" e;
            Error (Failure e))
  else (
    Printf.eprintf "Error: Unknown RPC command '%s'\n" cmd;
    Printf.eprintf
      "Available commands: ping, workspace, graph, build [package], format \
       <file>, format-check <file>, format-code <code>, restart, shutdown\n";
    Error (Failure (Printf.sprintf "Unknown RPC command: %s" cmd)))

(** Execute the new command *)
let new_command args =
  if Array.length args < 3 then (
    Printf.eprintf "Error: Package path required\n";
    Printf.eprintf "Usage: tusk new <path> [--lib|--bin]\n";
    Error (Failure "Missing package path"))
  else
    let path = args.(2) in
    let is_library =
      if Array.length args > 3 then
        match args.(3) with
        | "--bin" -> false
        | "--lib" -> true
        | _ -> true (* default to library *)
      else true
    in

    (* Extract package name from path *)
    let name = Filename.basename path in

    (* Use server to create the package *)
    let cwd =
      Std.Env.current_dir ()
      |> Std.Result.expect ~msg:"Failed to get current directory"
    in
    let workspace =
      Workspace_manager.scan cwd
      |> Std.Result.expect ~msg:"Failed to scan workspace"
    in

    (* Ensure server is running and create package via client *)
    match Server_manager.ensure_running ~workspace with
    | Ok client -> (
        match
          Tusk_jsonrpc.Client.new_package client ~path ~name ~is_library
        with
        | Ok (created_path, created_name) ->
            Printf.printf "Package '%s' created at '%s'\n" created_name
              created_path;
            Ok ()
        | Error e -> Error (Failure ("Package creation failed: " ^ e)))
    | Error _e -> Error (Failure "Failed to connect to server")

(** Execute the install command *)
let install_command args =
  if Array.length args < 3 then (
    Printf.eprintf "Error: Package name required\n";
    Printf.eprintf "Usage: tusk install <package>\n";
    Error (Failure "Package name required"))
  else
    let package_name = args.(2) in
    Printf.printf "📦 Installing %s...\n" package_name;

    (* First, build the package *)
    Printf.printf "Building %s...\n" package_name;
    if not (build_package package_name) then (
      Printf.eprintf "\n❌ Failed to build %s, nothing was installed\n"
        package_name;
      Error (Failure (Printf.sprintf "Failed to build %s" package_name)))
    else
      (* Look for the binary in various locations *)
      let root =
        Fs.getcwd () |> Result.expect ~msg:"Failed to get cwd" |> Path.to_string
      in
      let possible_binary_paths =
        [
          (* Bootstrap location *)
          Filename.concat (Filename.concat root "target/bootstrap") package_name;
          Filename.concat
            (Filename.concat root "target/bootstrap/out")
            (package_name ^ "/" ^ package_name);
          (* Debug location *)
          Filename.concat (Filename.concat root "target/debug") package_name;
          Filename.concat
            (Filename.concat root "target/debug/out")
            (package_name ^ "/" ^ package_name);
          Filename.concat
            (Filename.concat root "target/debug/out")
            ("packages/" ^ package_name ^ "/" ^ package_name);
        ]
      in

      match
        List.find_opt
          (fun path -> File_utils.exists ~path)
          possible_binary_paths
      with
      | None ->
          Printf.eprintf "❌ Binary for %s not found after build\n" package_name;
          Printf.eprintf
            "Note: Only packages with main.ml produce installable binaries\n";
          Error
            (Failure (Printf.sprintf "Binary not found for %s" package_name))
      | Some binary_path -> (
          (* Create ~/.tusk/bin if it doesn't exist *)
          let home =
            match Env.home_dir () with
            | Some h -> Path.to_string h
            | None -> failwith "HOME not set"
          in
          let tusk_bin_dir = Filename.concat home ".tusk/bin" in
          let mkdir_cmd = Printf.sprintf "mkdir -p %s" tusk_bin_dir in
          ignore (Command.system mkdir_cmd);

          (* Copy the binary to ~/.tusk/bin *)
          let dest_path = Filename.concat tusk_bin_dir package_name in
          let cp_cmd = Printf.sprintf "cp %s %s" binary_path dest_path in
          match Std.Command.of_unix_status (Command.system cp_cmd) with
          | Std.Command.Exited 0 ->
              (* Make it executable *)
              let chmod_cmd = Printf.sprintf "chmod +x %s" dest_path in
              ignore (Command.system chmod_cmd);

              Printf.printf "✅ Installed %s to %s\n" package_name dest_path;
              Printf.printf "\n";
              Printf.printf
                "To use %s from anywhere, add ~/.tusk/bin to your PATH:\n"
                package_name;
              Printf.printf "  export PATH='$HOME/.tusk/bin:$PATH'\n";
              Ok ()
          | _ ->
              Printf.eprintf "❌ Failed to install %s\n" package_name;
              Error
                (Failure (Printf.sprintf "Failed to install %s" package_name)))

(** Main entry point - runs as a Miniriot process *)
let main () =
  let args = Command.argv () in
  let argc = Array.length args in
  (* Initialize logger process first *)
  let _logger_pid = Log.init () in

  if argc < 2 then (
    Printf.eprintf "Error: No command specified\n\n%s\n" usage_msg;
    Error (Failure "No command specified"))
  else
    let command = args.(1) in
    match command with
    | "build" ->
        let package_opt = parse_build_args args 2 in
        build_command package_opt
    | "run" ->
        let binary_opt = parse_run_args args 2 in
        run_command binary_opt
    | "new" -> new_command args
    | "install" -> install_command args
    | "server" -> server_command args
    | "rpc" -> rpc_command args
    | "lsp" -> lsp_command args
    | "mcp" ->
        (* Start MCP server *)
        let cwd =
          Std.Env.current_dir () |> Std.Result.expect ~msg:"Operation failed"
        in
        let workspace =
          Workspace_manager.scan cwd
          |> Std.Result.expect ~msg:"Operation failed"
        in
        Mcp_server.start ();
        Ok ()
    | "fmt" | "format" -> fmt_command ()
    | "doc" -> doc_command ()
    | "clean" -> clean_command ()
    | "version" | "--version" | "-v" -> version_command ()
    | "help" | "--help" | "-h" -> help_command ()
    | _ ->
        Printf.eprintf "Error: Unknown command '%s'\n\n%s\n" command usage_msg;
        Error (Failure (Printf.sprintf "Unknown command: %s" command))
