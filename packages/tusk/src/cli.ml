(** CLI module - handles command-line interface *)

open Miniriot
open Build_messages

let usage_msg = "tusk - OCaml build system

Usage: tusk [COMMAND] [OPTIONS]

Commands:
  build    Build packages
  clean    Clean build artifacts
  help     Show this help message

Options:
  -p <package>    Build only the specified package"

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

(** Execute the build command *)
let build_command package_opt =
  (match package_opt with
  | Some pkg -> Printf.printf "🔨 Building package %s...\n" pkg
  | None -> Printf.printf "🔨 Starting build...\n");
  
  (* Start the build server *)
  let server_pid = Server.start () in
  
  (* Send ScanWorkspace message *)
  send server_pid ScanWorkspace;
  
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
    | "clean" -> clean_command ()
    | "help" | "--help" | "-h" -> help_command ()
    | _ ->
        Printf.eprintf "Error: Unknown command '%s'\n\n%s\n" command usage_msg;
        Process.Exception (Failure (Printf.sprintf "Unknown command: %s" command))