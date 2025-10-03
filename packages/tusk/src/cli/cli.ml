open Std
(** CLI module - handles command-line interface *)

let usage_msg =
  {|
   tusk - OCaml build system\n\n\
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
  \  -b <binary>     Run the specified binary
|}

(** Show help message *)
let help_command () =
  println "%s" usage_msg;
  Ok ()

(** Show version *)
let version_command () =
  println "dev";
  Ok ()

(** Main entry point - runs as a Miniriot process *)
let main ~args =
  let argc = List.length args in
  (* Initialize logger process first *)
  let _logger_pid = Core.Tusk_log.init () in

  if argc < 1 then (
    println "Error: No command specified\n\n%s" usage_msg;
    Error (Failure "No command specified"))
  else
    let command = List.nth args 0 in
    let cmd_args = List.tl args in
    (* Remove the command itself *)
    match command with
    | "build" -> Build.run cmd_args
    | "new" -> New.run cmd_args
    | "install" -> Install.run cmd_args
    | "server" -> Server_cmd.run cmd_args
    | "rpc" -> Rpc.run cmd_args
    | "mcp" -> Mcp_cmd.run cmd_args
    | "clean" -> Clean.run cmd_args
    | "fmt" | "format" -> Fmt.run cmd_args
    | "version" | "--version" | "-v" -> version_command ()
    | "help" | "--help" | "-h" -> help_command ()
    | _ ->
        println "Error: Unknown command '%s'\n\n%s" command usage_msg;
        Error (Failure (format "Unknown command: %s" command))
