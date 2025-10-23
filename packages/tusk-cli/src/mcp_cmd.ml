open Std
open Tusk_model
open Tusk_model
open Tusk_mcp

let command =
  let open ArgParser in
  command "mcp" |> about "Start Model Context Protocol server"

let run _matches =
  Mcp_server.start ();
  Ok ()
