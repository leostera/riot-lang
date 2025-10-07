open Std
open Std.Data

let description =
  {|Find which package owns a binary/executable by name.

WHEN TO USE THIS TOOL:
- When you need to discover where a binary is defined in the workspace
- When you want to know which package contains a specific executable
- When planning to run or modify a binary and need to locate its source
- When investigating build artifacts or executables

INSTEAD OF:
- DO NOT use 'find' to search for binaries
- DO NOT use 'grep' to search tusk.toml files for binary definitions
- DO NOT use 'ls' to list directories looking for executables
- DO NOT manually search through package directories

PARAMETERS:
- name (required): The binary name to find
  - Must be an exact binary name (case-sensitive)
  - Examples: 'tusk', 'myapp', 'test_runner'

RETURNS:
- If found:
  - package: Name of the package that owns the binary
  - binary: Name of the binary (confirms the match)
- If not found:
  - Indicates the binary was not found in any package

A binary is only discoverable if its package has a src/main.ml file that defines it.

EXAMPLES:
- Find the 'tusk' binary: {name: "tusk"}
- Find a custom binary: {name: "myapp"}

USE THIS when you need to locate the source package for a binary before building, running, or modifying it.|}

let tool =
  let open Mcp in
  let input_schema =
    Json.Object
      [
        ("type", Json.String "object");
        ( "properties",
          Json.Object
            [
              ( "name",
                Json.Object
                  [
                    ("type", Json.String "string");
                    ( "description",
                      Json.String
                        "Binary name to find (required, case-sensitive)" );
                  ] );
            ] );
        ("required", Json.Array [ Json.String "name" ]);
      ]
  in
  { name = "findExecutable"; description = Some description; input_schema }

type request = { name : string }
type response = ExecutableInfo of { json : string } | Error of string

let execute (client : Server.Tusk_jsonrpc.Client.t) (req : request) : response =
  match Server.Tusk_jsonrpc.Client.find_executable client req.name with
  | Ok (Some (package, binary)) ->
      let json =
        Json.Object
          [
            ("found", Json.Bool true);
            ("package", Json.String package);
            ("binary", Json.String binary);
          ]
        |> Json.to_string
      in
      ExecutableInfo { json }
  | Ok None ->
      let json =
        Json.Object
          [
            ("found", Json.Bool false);
            ( "message",
              Json.String
                (format "Binary '%s' not found in any package" req.name) );
          ]
        |> Json.to_string
      in
      ExecutableInfo { json }
  | Error msg -> Error msg

let response_to_json = function
  | ExecutableInfo { json } ->
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
