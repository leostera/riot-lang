(* # Graph Tool

   MCP tool for getting the build dependency graph.

   See the `description` field in graph.ml for complete documentation on
   when to use this tool and what it returns.

   This tool returns the complete dependency graph and should be used to
   understand package relationships. The description field contains detailed
   guidance for LLMs on when and how to use this tool.
*)

val tool : Mcp.tool

type request = unit
type response = GraphInfo of { json : string } | Error of string

val execute : Server.Tusk_jsonrpc.Client.t -> request -> response
val response_to_json : response -> Std.Data.Json.t
