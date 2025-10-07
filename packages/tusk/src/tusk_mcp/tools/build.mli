(* # Build Tool

   MCP tool for building OCaml packages in the tusk workspace.

   See the `description` field in build.ml for complete documentation on
   when to use this tool, what parameters it accepts, and what it returns.

   This tool compiles OCaml code and should be used instead of shell commands
   like 'dune build'. The description field contains detailed guidance for LLMs
   on when and how to use this tool.
*)

val tool : Mcp.tool

type request = { package : string option }
type response = BuildResult of { messages : string list } | Error of string

val execute : Server.Tusk_jsonrpc.Client.t -> request -> response
val response_to_json : response -> Std.Data.Json.t
