(* # Workspace Tool

   MCP tool for getting complete workspace information.

   See the `description` field in workspace.ml for complete documentation on
   when to use this tool and what it returns.

   This tool returns comprehensive workspace metadata and should be used
   FIRST when starting work on the codebase. The description field contains
   detailed guidance for LLMs on when and how to use this tool.
*)

val tool : Mcp.tool

type request = unit
type response = WorkspaceInfo of { json : string } | Error of string

val execute : Tusk_client.t -> request -> response
val response_to_json : response -> Std.Data.Json.t
