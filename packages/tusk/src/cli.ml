(** CLI module - handles command-line interface *)

open Miniriot

let usage_msg = "tusk - OCaml build system

Usage: tusk [COMMAND]

Commands:
  build    Build all packages
  clean    Clean build artifacts
  help     Show this help message"

(** Execute the build command *)
let build_command () =
  Printf.printf "🔨 Starting build...\n";
  
  (* Start the build server *)
  let server_pid = Server.start () in
  
  (* Send ScanWorkspace message *)
  send server_pid Server.ScanWorkspace;
  
  (* Give server time to scan and print *)
  sleep 100.0;
  
  (* Send BuildAll message *)
  send server_pid Server.BuildAll;
  
  (* Give server time to process *)
  sleep 100.0;
  
  (* Shutdown server *)
  send server_pid Server.Shutdown;
  
  (* Wait a bit for shutdown *)
  sleep 50.0;
  
  Process.Normal

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
    | "build" -> build_command ()
    | "clean" -> clean_command ()
    | "help" | "--help" | "-h" -> help_command ()
    | _ ->
        Printf.eprintf "Error: Unknown command '%s'\n\n%s\n" command usage_msg;
        Process.Exception (Failure (Printf.sprintf "Unknown command: %s" command))