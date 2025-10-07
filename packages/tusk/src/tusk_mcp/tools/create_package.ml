open Std
open Std.Data

let description =
  {|Create a new package in the workspace with basic structure and register it in tusk.toml.

WHEN TO USE THIS TOOL:
- When you need to create a new package/library in the project
- When adding a new module that should be its own package
- When organizing code into separate compilation units
- When creating a new library or executable package

Remember to always add the package to the top-level tusk.toml [member]s key after its created.

INSTEAD OF:
- DO NOT manually create directories and files
- DO NOT manually edit tusk.toml to add packages
- DO NOT use 'mkdir' or other shell commands
- DO NOT copy/paste from existing packages

PARAMETERS:
- name (required): Package name (e.g., "my-package")
  - Must be a valid OCaml module name (alphanumeric and hyphens)
  - Will be used as the directory name in ./packages/
- deps (optional): Array of package dependencies
  - List of other package names this package depends on
  - Examples: ["std"], ["std", "miniriot"], []
  - Defaults to empty array if not provided
- is_library (optional): Whether this is a library (true) or executable (false)
  - Defaults to true (creates a library)

RETURNS:
Returns CreatePackageResult with:
- path: Absolute path to the created package directory
- name: The package name
- files_created: List of files that were created

Or returns an Error if package creation fails.

EXAMPLES:
- Create library package: {name: "my-lib", deps: ["std"]}
- Create executable: {name: "my-app", deps: ["std", "my-lib"], is_library: false}
- Create standalone library: {name: "utils"}

This tool will:
1. Create ./packages/<name>/ directory
2. Create src/ subdirectory with basic module structure
3. Create tusk.toml with proper configuration
4. Set up dependencies as specified
|}

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
                    ("description", Json.String "Package name");
                  ] );
              ( "deps",
                Json.Object
                  [
                    ("type", Json.String "array");
                    ("items", Json.Object [ ("type", Json.String "string") ]);
                    ("description", Json.String "Array of package dependencies");
                  ] );
              ( "is_library",
                Json.Object
                  [
                    ("type", Json.String "boolean");
                    ( "description",
                      Json.String
                        "Whether this is a library (true) or executable (false)"
                    );
                  ] );
            ] );
        ("required", Json.Array [ Json.String "name" ]);
      ]
  in
  { name = "createPackage"; description = Some description; input_schema }

type request = { name : string; deps : string list; is_library : bool }

type response =
  | CreatePackageResult of {
      path : string;
      name : string;
      files_created : string list;
    }
  | Error of string

let execute (client : Server.Tusk_jsonrpc.Client.t) (req : request) : response =
  Log.debug "[CREATE_PACKAGE TOOL] execute() called with name: %s" req.name;

  let result =
    Server.Tusk_jsonrpc.Client.create_package client ~name:req.name
      ~deps:req.deps ~is_library:req.is_library
  in

  match result with
  | Ok (path, files) ->
      Log.debug "[CREATE_PACKAGE TOOL] Package created successfully";
      CreatePackageResult { path; name = req.name; files_created = files }
  | Error msg ->
      Log.error "[CREATE_PACKAGE TOOL] Error: %s" msg;
      Error msg

let response_to_json = function
  | CreatePackageResult { path; name; files_created } ->
      Json.Object
        [
          ( "content",
            Json.Array
              [
                Json.Object
                  [
                    ("type", Json.String "text");
                    ( "text",
                      Json.String
                        (format
                           "Created package '%s' at %s\n\nFiles created:\n%s"
                           name path
                           (String.concat "\n" files_created)) );
                  ];
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
