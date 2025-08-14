(** CLI module - handles command-line interface *)

open Miniriot
open Build_messages
open Rpc_messages

let usage_msg =
  "tusk - OCaml build system\n\n\
   Usage: tusk [COMMAND] [OPTIONS]\n\n\
   Commands:\n\
  \  build    Build packages\n\
  \  run      Run a binary\n\
  \  install  Install a package binary to ~/.tusk/bin/\n\
  \  server   Start the tusk server (for debugging)\n\
  \  rpc      Send RPC command to server\n\
  \  lsp      Start OCaml LSP server\n\
  \  fmt      Format OCaml code\n\
  \  doc      Generate documentation\n\
  \  clean    Clean build artifacts\n\
  \  version  Show tusk version\n\
  \  help     Show this help message\n\n\
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
  let root = System.getcwd () in
  let workspace = Workspace.scan ~root in

  (* Look for packages that have main.ml files (indicating they produce binaries) *)
  List.filter_map
    (fun package ->
      let main_ml_path = Filename.concat package.Workspace.path "src/main.ml" in
      if System.file_exists main_ml_path then Some package.Workspace.name
      else None)
    workspace.packages

(** Get the path to a binary *)
let get_binary_path binary_name =
  let root = System.getcwd () in
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

  List.find_opt (fun path -> System.is_regular_file path) possible_paths

(** Execute the build command *)
let build_command package_opt =
  (match package_opt with
  | Some pkg -> Printf.printf "🔨 Building package %s...\n" pkg
  | None -> Printf.printf "🔨 Starting build...\n");

  (* Start the build server *)
  let server_pid = Server.start () in

  (* Send ScanWorkspace message with target package *)
  send server_pid (ScanWorkspace package_opt);

  (* Give server time to scan and print *)
  sleep 0.5;

  (* Send RPC-style build request to server *)
  let request =
    match package_opt with
    | Some package -> Rpc.BuildPackage { package; watch = false }
    | None -> Rpc.BuildAll { watch = false }
  in
  send server_pid (ClientRequest (self (), request));

  (* Wait for RPC response *)
  let rec wait_for_completion () =
    let selector = function
      | ServerResponse (Rpc.BuildComplete { successful; failed }) ->
          `select (`build_complete (successful, failed))
      | ServerResponse (Rpc.Error { message }) -> `select (`error message)
      | _ -> `skip
    in
    match receive ~selector () with
    | `build_complete (successful, failed) ->
        if failed > 0 then (
          Printf.printf "❌ Build completed with %d failures!\n" failed;
          (* Exit with status 1 to indicate build failure *)
          Miniriot.shutdown ~status:1;
          Process.Normal)
        else (
          Printf.printf "✅ Build completed! %d packages built successfully.\n"
            successful;
          Process.Normal)
    | `error message ->
        Printf.printf "❌ Build error: %s\n" message;
        Miniriot.shutdown ~status:1;
        Process.Normal
  in
  wait_for_completion ()

(** Execute a binary and handle its exit status *)
let execute_binary binary_name binary_path =
  Printf.printf "🚀 Running %s...\n" binary_name;
  let result = System.system binary_path in
  match result with
  | Unix.WEXITED code ->
      if code = 0 then Process.Normal
      else
        Process.Exception
          (Failure (Printf.sprintf "Binary exited with code %d" code))
  | Unix.WSIGNALED signal ->
      Process.Exception
        (Failure (Printf.sprintf "Binary killed by signal %d" signal))
  | Unix.WSTOPPED signal ->
      Process.Exception
        (Failure (Printf.sprintf "Binary stopped by signal %d" signal))

(** Execute the clean command *)
let clean_command () =
  Printf.printf "🧹 Cleaning build artifacts...\n";
  let result = System.system "rm -rf ./target" in
  match result with
  | Unix.WEXITED 0 ->
      Printf.printf "Build artifacts cleaned!\n";
      Process.Normal
  | _ -> Process.Exception (Failure "Failed to clean build artifacts")

(** Build a package and wait for completion *)
let build_package package_name =
  Printf.printf "🔨 Building %s...\n" package_name;

  (* Start the build server *)
  let server_pid = Server.start () in

  (* Send ScanWorkspace message *)
  send server_pid (ScanWorkspace (Some package_name));

  (* Give server time to scan *)
  sleep 0.5;

  (* Send RPC-style build request *)
  let request = Rpc.BuildPackage { package = package_name; watch = false } in
  send server_pid (ClientRequest (self (), request));

  (* Wait for RPC response *)
  let rec wait_for_completion () =
    let selector = function
      | ServerResponse (Rpc.BuildComplete { successful = _; failed }) ->
          `select (`build_complete failed)
      | ServerResponse (Rpc.Error _) -> `select `error
      | _ -> `skip
    in
    match receive ~selector () with
    | `build_complete failed -> failed = 0
    | `error -> false
  in
  wait_for_completion ()

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
                  Process.Exception
                    (Failure
                       (Printf.sprintf "Binary '%s' not found after build"
                          binary_name)))
            else (
              Printf.eprintf "Error: Failed to build package '%s'.\n"
                binary_name;
              Process.Exception
                (Failure (Printf.sprintf "Build failed for %s" binary_name))))
      else (
        Printf.eprintf "Error: Binary '%s' not found in workspace.\n"
          binary_name;
        Printf.eprintf "Available binaries: %s\n"
          (String.concat ", " available_binaries);
        Process.Exception
          (Failure (Printf.sprintf "Unknown binary: %s" binary_name)))
  | None -> (
      (* No binary specified *)
      match available_binaries with
      | [] ->
          Printf.eprintf "Error: No binaries found in workspace.\n";
          Process.Exception (Failure "No binaries found")
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
                    let result = System.system binary_path in
                    match result with
                    | Unix.WEXITED code ->
                        if code = 0 then Process.Normal
                        else
                          Process.Exception
                            (Failure
                               (Printf.sprintf "Binary exited with code %d" code))
                    | Unix.WSIGNALED signal ->
                        Process.Exception
                          (Failure
                             (Printf.sprintf "Binary killed by signal %d" signal))
                    | Unix.WSTOPPED signal ->
                        Process.Exception
                          (Failure
                             (Printf.sprintf "Binary stopped by signal %d"
                                signal)))
                | None ->
                    Printf.eprintf
                      "Error: Binary '%s' still not found after building.\n"
                      single_binary;
                    Process.Exception
                      (Failure
                         (Printf.sprintf "Binary '%s' not found after build"
                            single_binary)))
              else (
                Printf.eprintf "Error: Failed to build package '%s'.\n"
                  single_binary;
                Process.Exception
                  (Failure (Printf.sprintf "Build failed for %s" single_binary)))
          )
      | _ ->
          (* Multiple binaries available, user must specify *)
          Printf.eprintf
            "Error: Multiple binaries found. Please specify which one to run \
             with -b flag.\n";
          Printf.eprintf "Available binaries: %s\n"
            (String.concat ", " available_binaries);
          Process.Exception (Failure "Multiple binaries found"))

(** Read OCaml toolchain version from ocaml-toolchain.toml *)
let read_toolchain_version () =
  let toml_path = "ocaml-toolchain.toml" in
  if not (System.file_exists toml_path) then
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
  let home = System.get_home () in
  let lsp_path =
    Printf.sprintf "%s/.tusk/toolchains/%s/bin/ocamllsp" home version
  in
  if System.file_exists lsp_path then lsp_path
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
      Merlin_bridge.start ();
      Process.Normal
  | "ocamlformat-rpc" ->
      (* Bridge to ocamlformat-rpc from toolchain *)
      let toolchain_dir =
        Filename.concat (Sys.getenv "HOME") ".tusk/toolchains/5.3.0/bin"
      in
      let ocamlformat_rpc = Filename.concat toolchain_dir "ocamlformat-rpc" in
      if Sys.file_exists ocamlformat_rpc then
        (* Pass through to ocamlformat-rpc with all remaining args *)
        let argv = Array.sub args 3 (Array.length args - 3) in
        Unix.execv ocamlformat_rpc (Array.append [| "ocamlformat-rpc" |] argv)
      else (
        Printf.eprintf "Error: ocamlformat-rpc not found at %s\n"
          ocamlformat_rpc;
        Printf.eprintf "Please run: cd ocaml && ./local-install.sh\n";
        Process.Exception (Failure "ocamlformat-rpc not found"))
  | "" | "start" ->
      (* Default: Start OCaml LSP server *)
      lsp_start_server ()
  | _ ->
      Printf.eprintf "Unknown lsp subcommand: %s\n" subcommand;
      Printf.eprintf "Available subcommands:\n";
      Printf.eprintf "  tusk lsp                 - Start OCaml LSP server\n";
      Printf.eprintf "  tusk lsp ocaml-merlin    - Run merlin protocol bridge\n";
      Printf.eprintf "  tusk lsp ocamlformat-rpc - Run ocamlformat RPC server\n";
      Process.Exception (Failure "Invalid lsp subcommand")

(** Start the LSP server *)
and lsp_start_server () =
  try
    (* Try to ensure the tusk server is running, but don't fail if there's an issue *)
    let _ = 
      try 
        ignore (Server_manager.ensure_running ());
        ()
      with _ -> 
        (* Server might already be running or there might be an issue - continue anyway *)
        ()
    in
    
    let lsp_path = get_lsp_binary_path () in
    let version = read_toolchain_version () in
    let home = System.get_home () in
    let toolchain_path =
      Printf.sprintf "%s/.tusk/toolchains/%s" home version
    in
    let stdlib_path = Printf.sprintf "%s/lib/ocaml" toolchain_path in

      (* Check if .merlin file exists *)
      let merlin_exists = System.file_exists ".merlin" in
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
      System.putenv "OCAMLPATH" stdlib_path;
      System.putenv "OCAMLLIB" stdlib_path;

      (* Execute the LSP server with stdio by default *)
      let args =
        if Array.length (System.argv ()) > 2 then
          (* Pass through any additional arguments *)
          Array.sub (System.argv ()) 2 (Array.length (System.argv ()) - 2)
        else
          (* Default to stdio mode *)
          [| "--stdio" |]
      in

      (* Build the full command *)
      let full_args = Array.append [| lsp_path |] args in

      (* Execute the LSP server *)
      let _ = System.exec lsp_path full_args in
      (* This should never be reached if execv succeeds *)
      Process.Exception (Failure "Failed to execute LSP server")
  with
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      Process.Exception (Failure msg)
  | exn ->
      let error_msg =
        Printf.sprintf "LSP command failed: %s" (Printexc.to_string exn)
      in
      Printf.eprintf "Error: %s\n" error_msg;
      Process.Exception (Failure error_msg)

(** Get ocamlformat binary path *)
let get_ocamlformat_binary_path () =
  let version = read_toolchain_version () in
  let home = System.get_home () in
  let fmt_path =
    Printf.sprintf "%s/.tusk/toolchains/%s/bin/ocamlformat" home version
  in
  if Sys.file_exists fmt_path then fmt_path
  else
    failwith
      (Printf.sprintf
         "ocamlformat not found at %s\n\
          Run 'tusk build' to install development tools"
         fmt_path)

(** Get odoc binary path *)
let get_odoc_binary_path () =
  let version = read_toolchain_version () in
  let home = System.get_home () in
  let odoc_path =
    Printf.sprintf "%s/.tusk/toolchains/%s/bin/odoc" home version
  in
  if Sys.file_exists odoc_path then odoc_path
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
    let root = System.getcwd () in
    let packages_dir = Filename.concat root "packages" in

    (* Use find to get all .ml and .mli files *)
    let find_cmd =
      Printf.sprintf "find %s -name '*.ml' -o -name '*.mli' 2>/dev/null"
        packages_dir
    in
    let ic = System.open_process_in find_cmd in
    let files = ref [] in
    (try
       while true do
         files := input_line ic :: !files
       done
     with End_of_file -> ());
    ignore (System.close_process_in ic);

    let file_list = List.rev !files in
    let total = List.length file_list in

    if total = 0 then (
      Printf.printf "No OCaml files found to format\n%!";
      Process.Normal)
    else (
      Printf.printf "🎨 Formatting %d OCaml files...\n%!" total;

      (* Format each file in place *)
      let formatted = ref 0 in
      List.iter
        (fun file ->
          let cmd = Printf.sprintf "%s -i %s 2>/dev/null" fmt_path file in
          let result = System.system cmd in
          match result with
          | Unix.WEXITED 0 ->
              incr formatted;
              Printf.printf "   ✓ %s\n%!" file
          | _ -> Printf.printf "   ✗ %s (skipped)\n%!" file)
        file_list;

      Printf.printf "\n✨ Formatted %d/%d files successfully\n%!" !formatted
        total;
      Process.Normal)
  with
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      Process.Exception (Failure msg)
  | exn ->
      let error_msg =
        Printf.sprintf "Format command failed: %s" (Printexc.to_string exn)
      in
      Printf.eprintf "Error: %s\n" error_msg;
      Process.Exception (Failure error_msg)

(** Execute the doc command *)
let doc_command () =
  try
    let odoc_path = get_odoc_binary_path () in
    let root = System.getcwd () in
    let doc_dir = Filename.concat root "_doc" in

    Printf.printf "📚 Generating documentation...\n%!";
    Printf.printf "   Output directory: %s\n%!" doc_dir;

    (* Create doc directory *)
    let mkdir_cmd = Printf.sprintf "mkdir -p %s" doc_dir in
    ignore (System.system mkdir_cmd);

    (* First, build all packages to ensure .cmi files exist *)
    Printf.printf "\n🔨 Building packages to generate interface files...\n%!";
    Printf.printf
      "   Run 'tusk build' first to ensure all packages are built\n%!";

    (* Find all .cmi files in target directory *)
    let target_dir = Filename.concat root "target/debug/out/packages" in
    let find_cmi_cmd =
      Printf.sprintf "find %s -name '*.cmi' 2>/dev/null" target_dir
    in
    let ic = System.open_process_in find_cmi_cmd in
    let cmi_files = ref [] in
    (try
       while true do
         cmi_files := input_line ic :: !cmi_files
       done
     with End_of_file -> ());
    ignore (System.close_process_in ic);

    let cmi_list = List.rev !cmi_files in

    if List.length cmi_list = 0 then (
      Printf.printf
        "\n⚠️  No compiled interface files found. Build the project first.\n%!";
      Process.Normal)
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
          let result = System.system cmd in
          match result with
          | Unix.WEXITED 0 -> Printf.printf "   ✓ %s\n%!" modname
          | _ -> Printf.printf "   ✗ %s (failed)\n%!" modname)
        cmi_list;

      (* Generate HTML from .odoc files *)
      Printf.printf "\n🌐 Generating HTML documentation...\n%!";
      let html_dir = Filename.concat doc_dir "html" in
      let cmd =
        Printf.sprintf "%s html-generate %s/*.odoc -o %s 2>/dev/null" odoc_path
          doc_dir html_dir
      in
      ignore (System.system cmd);

      Printf.printf "\n✨ Documentation generated at: %s/html\n%!" doc_dir;
      Printf.printf "   Open %s/html/index.html in your browser\n%!" doc_dir;
      Process.Normal)
  with
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      Process.Exception (Failure msg)
  | exn ->
      let error_msg =
        Printf.sprintf "Doc command failed: %s" (Printexc.to_string exn)
      in
      Printf.eprintf "Error: %s\n" error_msg;
      Process.Exception (Failure error_msg)

(** Show help message *)
let help_command () =
  Printf.printf "%s\n" usage_msg;
  Process.Normal

(** Show version *)
let version_command () =
  Printf.printf "%s\n" Version.version;
  Process.Normal

(** Execute the server command *)
let server_command args =
  (* Parse subcommand if provided *)
  let subcommand = if Array.length args > 2 then args.(2) else "" in
  match subcommand with
  | "start" ->
      (* Start server in background *)
      if Server_manager.start_background () then Process.Normal
      else Process.Exception (Failure "Failed to start server")
  | "stop" ->
      (* Stop background server *)
      if Server_manager.stop_background () then Process.Normal
      else Process.Exception (Failure "Failed to stop server")
  | "kill" ->
      (* Kill background server forcefully *)
      if Server_manager.kill_background () then Process.Normal
      else Process.Exception (Failure "Failed to kill server")
  | "status" ->
      (* Check server status *)
      Server_manager.status ();
      Process.Normal
  | "" | "foreground" ->
      (* Default: Run server in foreground *)
      Printf.printf "🚀 Starting tusk server...\n";
      Printf.printf "   Press Ctrl+C to stop\n\n";
      Server.start_with_listener ()
  | _ ->
      Printf.eprintf "Unknown server subcommand: %s\n" subcommand;
      Printf.eprintf "Available subcommands:\n";
      Printf.eprintf "  tusk server            - Start server in foreground\n";
      Printf.eprintf "  tusk server start      - Start server in background\n";
      Printf.eprintf "  tusk server stop       - Stop background server\n";
      Printf.eprintf
        "  tusk server kill       - Kill background server (force)\n";
      Printf.eprintf "  tusk server status     - Check server status\n";
      Process.Exception (Failure "Invalid server subcommand")

(** Execute the rpc command *)
let rpc_command args =
  (* Parse subcommand *)
  let cmd = if Array.length args > 2 then args.(2) else "" in

  (* Show help if no subcommand provided *)
  if cmd = "" then (
    Printf.printf "Available RPC commands:\n";
    Printf.printf "  tusk rpc ping              - Test server connectivity\n";
    Printf.printf "  tusk rpc workspace         - Get workspace information\n";
    Printf.printf "  tusk rpc graph             - Get build graph\n";
    Printf.printf "  tusk rpc scan-workspace    - Scan workspace for packages\n";
    Printf.printf
      "  tusk rpc build [package]   - Build all or specific package\n";
    Printf.printf "  tusk rpc restart           - Restart the server\n";
    Printf.printf "  tusk rpc shutdown          - Shutdown the server\n";
    Process.Normal)
  else
    try
      (* Map command to request *)
      let request =
        match cmd with
        | "ping" -> Rpc.Ping
        | "workspace" -> Rpc.GetWorkspace
        | "graph" -> Rpc.GetBuildGraph
        | "scan-workspace" ->
            (* Simply request a workspace scan - it will return current workspace info *)
            Rpc.GetWorkspace
        | "build" -> (
            (* Parse optional package name *)
            let package =
              if Array.length args > 3 then Some args.(3) else None
            in
            match package with
            | Some pkg -> Rpc.BuildPackage { package = pkg; watch = false }
            | None -> Rpc.BuildAll { watch = false })
        | "restart" -> Rpc.Restart
        | "shutdown" -> Rpc.Shutdown
        | _ ->
            Printf.eprintf "Error: Unknown RPC command '%s'\n" cmd;
            Printf.eprintf
              "Available commands: ping, workspace, graph, scan-workspace, \
               build [package], restart, shutdown\n";
            failwith (Printf.sprintf "Unknown RPC command: %s" cmd)
      in

      (* Make RPC call *)
      match Rpc_client.call request with
      | Ok response -> (
          match response with
          | Rpc.Pong ->
              Printf.printf "pong\n";
              Process.Normal
          | Rpc.Ok ->
              Printf.printf "ok\n";
              Process.Normal
          | Rpc.WorkspaceInfo { packages; root } ->
              Printf.printf "Workspace at %s:\n" root;
              List.iter (fun pkg -> Printf.printf "  - %s\n" pkg) packages;
              Process.Normal
          | Rpc.BuildGraphInfo { packages } ->
              Printf.printf "Build graph:\n";
              List.iter
                (fun (pkg, deps) ->
                  Printf.printf "  %s -> %s\n" pkg
                    (if deps = [] then "[]"
                     else "[" ^ String.concat ", " deps ^ "]"))
                packages;
              Process.Normal
          | Rpc.BuildComplete { successful; failed } ->
              if failed > 0 then
                Printf.printf
                  "Build completed with %d successes and %d failures\n"
                  successful failed
              else
                Printf.printf
                  "Build completed successfully! %d packages built\n" successful;
              Process.Normal
          | Rpc.Error { message } ->
              Printf.eprintf "Error: %s\n" message;
              Process.Exception (Failure message)
          | _ ->
              Printf.printf "%s\n" (Rpc.response_to_string response);
              Process.Normal)
      | Error msg ->
          if msg = "Server is not running" then
            Printf.eprintf
              "Server is not running, start server with `tusk server`\n"
          else Printf.eprintf "Error: %s\n" msg;
          Process.Exception (Failure msg)
    with
    | Failure msg ->
        if String.contains msg 's' && String.contains msg 'e' then
          (* Check for "server" *)
          Printf.eprintf
            "Server is not running, start server with `tusk server`\n"
        else Printf.eprintf "Error: %s\n" msg;
        Process.Exception (Failure msg)
    | exn ->
        let error_msg =
          Printf.sprintf "RPC command failed: %s" (Printexc.to_string exn)
        in
        Printf.eprintf "Error: %s\n" error_msg;
        Process.Exception (Failure error_msg)

(** Execute the install command *)
let install_command args =
  if Array.length args < 3 then (
    Printf.eprintf "Error: Package name required\n";
    Printf.eprintf "Usage: tusk install <package>\n";
    Process.Exception (Failure "Package name required"))
  else
    let package_name = args.(2) in
    Printf.printf "📦 Installing %s...\n" package_name;

    (* First, build the package *)
    Printf.printf "Building %s...\n" package_name;
    if not (build_package package_name) then (
      Printf.eprintf "❌ Failed to build %s\n" package_name;
      Process.Exception
        (Failure (Printf.sprintf "Failed to build %s" package_name)))
    else
      (* Look for the binary in various locations *)
      let root = System.getcwd () in
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

      match List.find_opt System.file_exists possible_binary_paths with
      | None ->
          Printf.eprintf "❌ Binary for %s not found after build\n" package_name;
          Printf.eprintf
            "Note: Only packages with main.ml produce installable binaries\n";
          Process.Exception
            (Failure (Printf.sprintf "Binary not found for %s" package_name))
      | Some binary_path -> (
          (* Create ~/.tusk/bin if it doesn't exist *)
          let home = System.get_home () in
          let tusk_bin_dir = Filename.concat home ".tusk/bin" in
          let mkdir_cmd = Printf.sprintf "mkdir -p %s" tusk_bin_dir in
          ignore (System.system mkdir_cmd);

          (* Copy the binary to ~/.tusk/bin *)
          let dest_path = Filename.concat tusk_bin_dir package_name in
          let cp_cmd = Printf.sprintf "cp %s %s" binary_path dest_path in
          match System.system cp_cmd with
          | Unix.WEXITED 0 ->
              (* Make it executable *)
              let chmod_cmd = Printf.sprintf "chmod +x %s" dest_path in
              ignore (System.system chmod_cmd);

              Printf.printf "✅ Installed %s to %s\n" package_name dest_path;
              Printf.printf "\n";
              Printf.printf
                "To use %s from anywhere, add ~/.tusk/bin to your PATH:\n"
                package_name;
              Printf.printf "  export PATH=\"$HOME/.tusk/bin:$PATH\"\n";
              Process.Normal
          | _ ->
              Printf.eprintf "❌ Failed to install %s\n" package_name;
              Process.Exception
                (Failure (Printf.sprintf "Failed to install %s" package_name)))

(** Main entry point - runs as a Miniriot process *)
let main () =
  let args = System.argv () in
  let argc = Array.length args in

  if argc < 2 then (
    Printf.eprintf "Error: No command specified\n\n%s\n" usage_msg;
    Process.Exception (Failure "No command specified"))
  else
    let command = args.(1) in
    match command with
    | "build" ->
        let package_opt = parse_build_args args 2 in
        build_command package_opt
    | "run" ->
        let binary_opt = parse_run_args args 2 in
        run_command binary_opt
    | "install" -> install_command args
    | "server" -> server_command args
    | "rpc" -> rpc_command args
    | "lsp" -> lsp_command args
    | "fmt" | "format" -> fmt_command ()
    | "doc" -> doc_command ()
    | "clean" -> clean_command ()
    | "version" | "--version" | "-v" -> version_command ()
    | "help" | "--help" | "-h" -> help_command ()
    | _ ->
        Printf.eprintf "Error: Unknown command '%s'\n\n%s\n" command usage_msg;
        Process.Exception
          (Failure (Printf.sprintf "Unknown command: %s" command))
