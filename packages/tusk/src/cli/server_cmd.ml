open Std
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
      println "Server start not implemented yet";
      Ok ()
  | "stop" ->
      (* Stop background server *)
      println "Server stop not implemented yet";
      Ok ()
  | "kill" ->
      (* Kill background server forcefully *)
      println "Server kill not implemented yet";
      Ok ()
  | "status" ->
      (* Check server status *)
      println "Server status not implemented yet";
      Ok ()
  | "" | "foreground" ->
      (* Default: Run server in foreground *)
      println "🚀 Starting tusk server...";
      println "   Press Ctrl+C to stop\n";
      Tusk_server.start_with_listener ()
  | _ ->
      println "Unknown server subcommand: %s" subcommand;
      println "Available subcommands:";
      println "  tusk server            - Start server in foreground";
      println "  tusk server start      - Start server in background";
      println "  tusk server stop       - Stop background server";
      println "  tusk server kill       - Kill background server (force)";
      println "  tusk server status     - Check server status";
      Error (Failure "Invalid server subcommand")
