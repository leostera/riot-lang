open Std
open Std.Data

let description =
  {|Get detailed information about a specific package including all source files, dependencies, and configuration.

WHEN TO USE THIS TOOL:
- When you need to see what source files belong to a package
- When you want to understand a package's structure
- When finding where specific functionality is implemented
- When you need to know a package's dependencies
- When exploring code organization within a package

INSTEAD OF:
- DO NOT use 'find' to search for .ml/.mli files
- DO NOT use 'ls' to list package contents
- DO NOT use 'grep' to search for source files
- DO NOT manually look through directories

PARAMETERS:
- name (required): The package name to query
  - Must be an exact package name from the workspace
  - Case-sensitive

RETURNS:
- package: Package metadata:
  - name: Package name
  - path: Path to package directory
  - dependencies: List of package dependencies
- sources: Array of all source file paths in the package
  - Includes both .ml and .mli files
  - Paths are relative to package directory
- dependency_names: Flattened list of all dependencies

This gives you a complete view of what files and dependencies make up a package.

EXAMPLES:
- Get info about 'std' package: {name: "std"}
- Get info about 'tusk' package: {name: "tusk"}

USE THIS when you need to understand the contents and structure of a specific package.|}

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
                      Json.String "Package name to query (required)" );
                  ] );
            ] );
        ("required", Json.Array [ Json.String "name" ]);
      ]
  in
  { name = "describePackage"; description = Some description; input_schema }

type request = { name : string }
type response = PackageInfo of { json : string } | Error of string

let execute (client : Tusk_client.t) (req : request) : response =
  match Tusk_client.get_package_info client req.name with
  | Ok detail ->
      let json =
        Json.Object
          [
            ( "package",
              Json.Object
                [
                  ( "name",
                    Json.String detail.Tusk_protocol.WireProtocol.package.name
                  );
                  ( "path",
                    Json.String detail.Tusk_protocol.WireProtocol.package.path
                  );
                  ( "dependencies",
                    Json.Array
                      (List.map
                         (fun d -> Json.String d)
                         detail.Tusk_protocol.WireProtocol.package.dependencies)
                  );
                ] );
            ( "sources",
              Json.Array
                (List.map
                   (fun s -> Json.String s)
                   detail.Tusk_protocol.WireProtocol.sources) );
            ( "dependency_names",
              Json.Array
                (List.map
                   (fun d -> Json.String d)
                   detail.Tusk_protocol.WireProtocol.dependency_names) );
          ]
        |> Json.to_string
      in
      PackageInfo { json }
  | Error msg -> Error msg

let response_to_json = function
  | PackageInfo { json } ->
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
