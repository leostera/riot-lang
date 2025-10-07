open Std
open Std.Data

let description =
  {|Find the filesystem path to a built binary artifact.

WHEN TO USE THIS TOOL:
- When you need the actual file path of a compiled executable
- When preparing to run a binary and need its location
- When you want to inspect or copy a built artifact
- When verifying that a binary was successfully built

INSTEAD OF:
- DO NOT use 'find' to search target/ directories
- DO NOT use 'ls' to manually browse build output
- DO NOT guess at artifact paths
- DO NOT manually construct paths to target/debug or target/release

PARAMETERS:
- package (required): The package name that owns the binary
  - Must be an exact package name from the workspace
  - Use tusk.findExecutable first if you don't know which package owns a binary
- name (required): The binary name to locate
  - Must be an exact binary name (case-sensitive)

RETURNS:
- If found:
  - path: Absolute filesystem path to the built binary
  - This path can be used directly to execute the binary
- If not found:
  - Error message indicating the binary hasn't been built or doesn't exist
  - You may need to run tusk.build first to create the artifact

The artifact must have been built before it can be found. If the binary doesn't exist, build the package first.

EXAMPLES:
- Find the tusk binary: {package: "tusk", name: "tusk"}
- Find a custom binary: {package: "myapp", name: "myapp"}

TYPICAL WORKFLOW:
1. Use tusk.findExecutable to discover which package owns a binary
2. Use tusk.build to compile the package if needed
3. Use tusk.findArtifact to get the path to the compiled binary
4. Use the path to run or inspect the binary|}

let tool =
  let open Mcp in
  let input_schema =
    Json.Object
      [
        ("type", Json.String "object");
        ( "properties",
          Json.Object
            [
              ( "package",
                Json.Object
                  [
                    ("type", Json.String "string");
                    ( "description",
                      Json.String "Package name that owns the binary (required)"
                    );
                  ] );
              ( "name",
                Json.Object
                  [
                    ("type", Json.String "string");
                    ( "description",
                      Json.String "Binary name to locate (required)" );
                  ] );
            ] );
        ("required", Json.Array [ Json.String "package"; Json.String "name" ]);
      ]
  in
  { name = "findArtifact"; description = Some description; input_schema }

type request = { package : string; name : string }
type response = ArtifactInfo of { json : string } | Error of string

let execute (client : Server.Tusk_jsonrpc.Client.t) (req : request) : response =
  match
    Server.Tusk_jsonrpc.Client.find_artifact client ~package:req.package
      ~kind:"binary" ~name:req.name
  with
  | Ok path ->
      let json =
        Json.Object
          [
            ("found", Json.Bool true);
            ("path", Json.String path);
            ("package", Json.String req.package);
            ("binary", Json.String req.name);
          ]
        |> Json.to_string
      in
      ArtifactInfo { json }
  | Error msg ->
      let json =
        Json.Object
          [
            ("found", Json.Bool false);
            ( "message",
              Json.String
                (format "Binary '%s' in package '%s' not found: %s" req.name
                   req.package msg) );
            ( "hint",
              Json.String
                "The binary may not have been built yet. Try running \
                 tusk.build first." );
          ]
        |> Json.to_string
      in
      ArtifactInfo { json }

let response_to_json = function
  | ArtifactInfo { json } ->
      Json.Object
        [
          ( "content",
            Json.Array
              [
                Json.Object
                  [ ("type", Json.String "text"); ("text", Json.String json) ];
              ] );
          ("isError", Json.Bool false);
        ]
  | Error message ->
      Json.Object
        [
          ( "content",
            Json.Array
              [
                Json.Object
                  [
                    ("type", Json.String "text"); ("text", Json.String message);
                  ];
              ] );
          ("isError", Json.Bool true);
        ]
