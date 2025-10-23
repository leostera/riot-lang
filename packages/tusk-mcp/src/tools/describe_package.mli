(* # Package Tool

   MCP tool for getting detailed information about a specific package.

   ## Purpose

   Returns complete package metadata including all source files, dependencies,
   and configuration. Use this instead of 'find' or 'grep' when you need to
   see what files belong to a package.

   ## Request

   - `name`: Package name to query (required, case-sensitive)

   ## Response

   - `PackageInfo { json }`: JSON containing:
     - package: Package metadata (name, path, dependencies)
     - sources: Array of all source file paths (.ml and .mli files)
     - dependency_names: Flattened list of all dependencies
   - `Error string`: Error message if package not found or query fails

   ## Usage

   Use this when you need to understand the contents and structure of a
   specific package. DO NOT use 'find' or 'ls' to discover source files.
*)

val tool : Mcp.tool

type request = { name : string }
type response = PackageInfo of { json : string } | Error of string

val execute : Tusk_client.t -> request -> response
val response_to_json : response -> Std.Data.Json.t
