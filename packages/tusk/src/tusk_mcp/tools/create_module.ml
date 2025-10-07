open Std
open Std.Data

let description =
  {|Create a new OCaml module file within an existing package.

WHEN TO USE THIS TOOL:
- When adding a new module to an existing package
- When creating new .ml/.mli files with initial content
- When scaffolding new functionality within a package
- When organizing code into separate modules

INSTEAD OF:
- DO NOT manually create .ml/.mli files
- DO NOT use 'touch' or shell commands
- DO NOT copy/paste boilerplate from other files

PARAMETERS:
- package (required): Package name where the module should be created
  - Must be an existing package in the workspace
- module_name (required): Name of the module to create
  - Will create <module_name>.ml and optionally <module_name>.mli
  - Use capitalized names (e.g., "My_module" not "my_module")
- contents (optional): Initial contents for the .ml file
  - If not provided, creates a minimal module with open Std
  - Can include complete module implementation

RETURNS:
Returns CreateModuleResult with:
- package: The package name
- module_name: The module name
- files_created: List of files that were created (paths)

Or returns an Error if module creation fails.

EXAMPLES:
- Create empty module: {package: "my-lib", module_name: "Utils"}
- Create with contents: {package: "my-lib", module_name: "Helper", contents: "open Std\n\nlet greet name = format \"Hello, %s!\" name"}

This tool will:
1. Validate that the package exists
2. Create src/<module_name>.ml with the provided contents
3. Create src/<module_name>.mli if appropriate
4. Use proper OCaml formatting and conventions
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
              ( "package",
                Json.Object
                  [
                    ("type", Json.String "string");
                    ("description", Json.String "Package name");
                  ] );
              ( "module_name",
                Json.Object
                  [
                    ("type", Json.String "string");
                    ("description", Json.String "Module name (capitalized)");
                  ] );
              ( "contents",
                Json.Object
                  [
                    ("type", Json.String "string");
                    ( "description",
                      Json.String "Initial contents for the .ml file" );
                  ] );
            ] );
        ( "required",
          Json.Array [ Json.String "package"; Json.String "module_name" ] );
      ]
  in
  { name = "createModule"; description = Some description; input_schema }

type request = { package : string; module_name : string; contents : string }

type response =
  | CreateModuleResult of {
      package : string;
      module_name : string;
      files_created : string list;
    }
  | Error of string

let execute (client : Server.Tusk_jsonrpc.Client.t) (req : request) : response =
  Log.debug "[CREATE_MODULE TOOL] execute() called for package: %s, module: %s"
    req.package req.module_name;

  let result =
    Server.Tusk_jsonrpc.Client.create_module client ~package:req.package
      ~module_name:req.module_name ~contents:req.contents
  in

  match result with
  | Ok files ->
      Log.debug "[CREATE_MODULE TOOL] Module created successfully";
      CreateModuleResult
        {
          package = req.package;
          module_name = req.module_name;
          files_created = files;
        }
  | Error msg ->
      Log.error "[CREATE_MODULE TOOL] Error: %s" msg;
      Error msg

let response_to_json = function
  | CreateModuleResult { package; module_name; files_created } ->
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
                           "Created module '%s' in package '%s'\n\n\
                            Files created:\n\
                            %s"
                           module_name package
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
