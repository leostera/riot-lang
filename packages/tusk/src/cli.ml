(** CLI module - handles command-line interface *)

open Miniriot
open Build_messages

let usage_msg = "tusk - OCaml build system

Usage: tusk [COMMAND] [OPTIONS]

Commands:
  build    Build packages
  run      Run a binary
  clean    Clean build artifacts
  help     Show this help message

Options:
  -p <package>    Build only the specified package
  -b <binary>     Run the specified binary"

(** Parse command line arguments for build command *)
let parse_build_args args start_idx =
  let rec parse idx package =
    if idx >= Array.length args then
      package
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
    if idx >= Array.length args then
      binary
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
  let root = Sys.getcwd () in
  let workspace = Workspace.scan ~root in
  
  (* Look for packages that have main.ml files (indicating they produce binaries) *)
  List.filter_map (fun package ->
    let main_ml_path = Filename.concat package.Workspace.path "src/main.ml" in
    if Sys.file_exists main_ml_path then
      Some package.Workspace.name
    else
      None
  ) workspace.packages

(** Get the path to a binary *)
let get_binary_path binary_name =
  let root = Sys.getcwd () in
  let target_path = Filename.concat root "target/debug/out" in
  
  (* Try different possible locations *)
  let possible_paths = [
    Filename.concat target_path binary_name;
    Filename.concat (Filename.concat target_path ("packages/" ^ binary_name)) binary_name;
    Filename.concat (Filename.concat root "target/bootstrap") binary_name;
    Filename.concat (Filename.concat root "target/debug") binary_name;
  ] in
  
  List.find_opt (fun path ->
    Sys.file_exists path && (Unix.stat path).st_kind = Unix.S_REG
  ) possible_paths

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
  
  (* Send appropriate build message *)
  (match package_opt with
  | Some package ->
      send server_pid (BuildPackage (package, self ()))
  | None ->
      send server_pid (BuildAll (self ())));
  
  (* Wait for BuildFinished message *)
  let rec wait_for_completion () =
    match receive () with
    | BuildFinished ->
        Printf.printf "✅ Build completed!\n";
        Process.Normal
    | _ ->
        (* Ignore other messages *)
        wait_for_completion ()
  in
  wait_for_completion ()

(** Execute the clean command *)
let clean_command () =
  Printf.printf "🧹 Cleaning build artifacts...\n";
  let result = Unix.system "rm -rf ./target" in
  match result with
  | Unix.WEXITED 0 -> 
      Printf.printf "Build artifacts cleaned!\n";
      Process.Normal
  | _ -> 
      Process.Exception (Failure "Failed to clean build artifacts")

(** Build a package and wait for completion *)
let build_package package_name =
  Printf.printf "🔨 Building %s...\n" package_name;
  
  (* Start the build server *)
  let server_pid = Server.start () in
  
  (* Send ScanWorkspace message *)
  send server_pid (ScanWorkspace (Some package_name));
  
  (* Give server time to scan *)
  sleep 0.5;
  
  (* Send build message *)
  send server_pid (BuildPackage (package_name, self ()));
  
  (* Wait for BuildFinished message *)
  let rec wait_for_completion () =
    match receive () with
    | BuildFinished -> true
    | _ -> wait_for_completion ()
  in
  wait_for_completion ()

(** Execute the run command *)
let run_command binary_opt =
  let available_binaries = find_binaries () in
  
  match binary_opt with
  | Some binary_name ->
      (* User specified a binary with -b flag *)
      if List.mem binary_name available_binaries then
        (* Check if binary exists, if not build it first *)
        (match get_binary_path binary_name with
        | Some binary_path ->
            (* Binary exists, run it directly *)
            Printf.printf "🚀 Running %s...\n" binary_name;
            let result = Unix.system binary_path in
            (match result with
            | Unix.WEXITED code -> 
                if code = 0 then Process.Normal
                else Process.Exception (Failure (Printf.sprintf "Binary exited with code %d" code))
            | Unix.WSIGNALED signal ->
                Process.Exception (Failure (Printf.sprintf "Binary killed by signal %d" signal))
            | Unix.WSTOPPED signal ->
                Process.Exception (Failure (Printf.sprintf "Binary stopped by signal %d" signal)))
        | None ->
            (* Binary doesn't exist, build it first *)
            Printf.printf "📦 Binary '%s' not found, building first...\n" binary_name;
            if build_package binary_name then
              (* Build succeeded, try to run again *)
              (match get_binary_path binary_name with
              | Some binary_path ->
                  Printf.printf "🚀 Running %s...\n" binary_name;
                  let result = Unix.system binary_path in
                  (match result with
                  | Unix.WEXITED code -> 
                      if code = 0 then Process.Normal
                      else Process.Exception (Failure (Printf.sprintf "Binary exited with code %d" code))
                  | Unix.WSIGNALED signal ->
                      Process.Exception (Failure (Printf.sprintf "Binary killed by signal %d" signal))
                  | Unix.WSTOPPED signal ->
                      Process.Exception (Failure (Printf.sprintf "Binary stopped by signal %d" signal)))
              | None ->
                  Printf.eprintf "Error: Binary '%s' still not found after building.\n" binary_name;
                  Process.Exception (Failure (Printf.sprintf "Binary '%s' not found after build" binary_name)))
            else
              (Printf.eprintf "Error: Failed to build package '%s'.\n" binary_name;
              Process.Exception (Failure (Printf.sprintf "Build failed for %s" binary_name))))
      else (
        Printf.eprintf "Error: Binary '%s' not found in workspace.\n" binary_name;
        Printf.eprintf "Available binaries: %s\n" (String.concat ", " available_binaries);
        Process.Exception (Failure (Printf.sprintf "Unknown binary: %s" binary_name)))
  | None ->
      (* No binary specified *)
      (match available_binaries with
      | [] ->
          Printf.eprintf "Error: No binaries found in workspace.\n";
          Process.Exception (Failure "No binaries found")
      | [single_binary] ->
          (* Only one binary available, run it *)
          (match get_binary_path single_binary with
          | Some binary_path ->
              Printf.printf "🚀 Running %s...\n" single_binary;
              let result = Unix.system binary_path in
              (match result with
              | Unix.WEXITED code -> 
                  if code = 0 then Process.Normal
                  else Process.Exception (Failure (Printf.sprintf "Binary exited with code %d" code))
              | Unix.WSIGNALED signal ->
                  Process.Exception (Failure (Printf.sprintf "Binary killed by signal %d" signal))
              | Unix.WSTOPPED signal ->
                  Process.Exception (Failure (Printf.sprintf "Binary stopped by signal %d" signal)))
          | None ->
              (* Binary doesn't exist, build it first *)
              Printf.printf "📦 Binary '%s' not found, building first...\n" single_binary;
              if build_package single_binary then
                (* Build succeeded, try to run again *)
                (match get_binary_path single_binary with
                | Some binary_path ->
                    Printf.printf "🚀 Running %s...\n" single_binary;
                    let result = Unix.system binary_path in
                    (match result with
                    | Unix.WEXITED code -> 
                        if code = 0 then Process.Normal
                        else Process.Exception (Failure (Printf.sprintf "Binary exited with code %d" code))
                    | Unix.WSIGNALED signal ->
                        Process.Exception (Failure (Printf.sprintf "Binary killed by signal %d" signal))
                    | Unix.WSTOPPED signal ->
                        Process.Exception (Failure (Printf.sprintf "Binary stopped by signal %d" signal)))
                | None ->
                    Printf.eprintf "Error: Binary '%s' still not found after building.\n" single_binary;
                    Process.Exception (Failure (Printf.sprintf "Binary '%s' not found after build" single_binary)))
              else
                (Printf.eprintf "Error: Failed to build package '%s'.\n" single_binary;
                Process.Exception (Failure (Printf.sprintf "Build failed for %s" single_binary))))
      | _ ->
          (* Multiple binaries available, user must specify *)
          Printf.eprintf "Error: Multiple binaries found. Please specify which one to run with -b flag.\n";
          Printf.eprintf "Available binaries: %s\n" (String.concat ", " available_binaries);
          Process.Exception (Failure "Multiple binaries found"))

(** Show help message *)
let help_command () =
  Printf.printf "%s\n" usage_msg;
  Process.Normal

(** Main entry point - runs as a Miniriot process *)
let main () =
  let args = Sys.argv in
  let argc = Array.length args in
  
  if argc < 2 then begin
    Printf.eprintf "Error: No command specified\n\n%s\n" usage_msg;
    Process.Exception (Failure "No command specified")
  end else
    let command = args.(1) in
    match command with
    | "build" -> 
        let package_opt = parse_build_args args 2 in
        build_command package_opt
    | "run" ->
        let binary_opt = parse_run_args args 2 in
        run_command binary_opt
    | "clean" -> clean_command ()
    | "help" | "--help" | "-h" -> help_command ()
    | _ ->
        Printf.eprintf "Error: Unknown command '%s'\n\n%s\n" command usage_msg;
        Process.Exception (Failure (Printf.sprintf "Unknown command: %s" command))