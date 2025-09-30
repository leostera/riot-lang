open Core
open Model
open Server

(** Execute the server command *)
let run args =
  (* Parse subcommand if provided *)
  let subcommand = if List.length args > 0 then List.nth args 0 else "" in
  match subcommand with
  | "start" ->
      (* Start server in background *)
      Printf.printf "Server start not implemented yet\n%!";
      Ok ()
  | "stop" ->
      (* Stop background server *)
      Printf.printf "Server stop not implemented yet\n%!";
      Ok ()
  | "kill" ->
      (* Kill background server forcefully *)
      Printf.printf "Server kill not implemented yet\n%!";
      Ok ()
  | "status" ->
      (* Check server status *)
      Printf.printf "Server status not implemented yet\n%!";
      Ok ()
  | "" | "foreground" ->
      (* Default: Run server in foreground *)
      Printf.printf "🚀 Starting tusk server...\n%!";
      Printf.printf "   Press Ctrl+C to stop\n\n%!";
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
