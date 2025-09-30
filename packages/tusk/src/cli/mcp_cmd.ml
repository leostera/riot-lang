open Model
open Core
open Tusk_mcp

let run _args =
  (* Start MCP server *)
  Mcp_server.start ();
  Ok ()
