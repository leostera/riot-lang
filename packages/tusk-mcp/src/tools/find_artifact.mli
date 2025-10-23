(* # Find_artifact Tool

   MCP tool for finding the filesystem path to a built binary artifact.

   ## Purpose

   Locates the actual file path of a compiled executable. Use this to get
   the exact location of a built binary instead of manually searching
   target/ directories.

   ## Request

   - `package`: Package name that owns the binary (required)
   - `name`: Binary name to locate (required, case-sensitive)

   ## Response

   - `ArtifactInfo { json }`: JSON containing:
     - found: Boolean indicating if artifact exists
     - path: Absolute filesystem path to the binary (if found)
     - package: Package name (confirms input)
     - binary: Binary name (confirms input)
     - hint: Suggestion to build first if not found
   - `Error string`: Error message if lookup fails

   ## Usage

   Use this to get the actual file path of a compiled executable. The
   artifact must have been built before it can be found. DO NOT use 'find'
   or manually construct paths to target/debug or target/release.

   ## Typical Workflow

   1. Use tusk.findExecutable to discover which package owns a binary
   2. Use tusk.build to compile the package if needed
   3. Use tusk.findArtifact to get the path to the compiled binary
   4. Use the path to run or inspect the binary
*)

val tool : Mcp.tool

type request = { package : string; name : string }
type response = ArtifactInfo of { json : string } | Error of string

val execute : Tusk_client.t -> request -> response
val response_to_json : response -> Std.Data.Json.t
