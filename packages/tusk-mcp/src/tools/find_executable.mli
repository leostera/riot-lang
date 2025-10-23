(* # Find_executable Tool

   MCP tool for finding which package owns a binary/executable.

   ## Purpose

   Discovers the owning package for a given binary name without requiring
   filesystem searches or grep operations.

   ## Request

   - `name`: Binary name to search for (required, case-sensitive)

   ## Response

   - `ExecutableInfo { json }`: JSON with package and binary name if found
   - `Error string`: Error message if lookup fails

   ## Usage

   Use this tool instead of 'find' or 'grep' when you need to discover
   where a binary is defined in the workspace.
*)

val tool : Mcp.tool

type request = { name : string }
type response = ExecutableInfo of { json : string } | Error of string

val execute : Tusk_client.t -> request -> response
val response_to_json : response -> Std.Data.Json.t
