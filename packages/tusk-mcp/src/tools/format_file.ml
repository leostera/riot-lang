open Std
open Std.Data

let description =
  {|Format an OCaml source file using ocamlformat.

WHEN TO USE THIS TOOL:
- After creating or modifying OCaml source files
- When you want to ensure code follows project formatting standards
- Before committing code changes
- To check if a file needs formatting (use check_only: true)

INSTEAD OF:
- DO NOT manually format code
- DO NOT use shell commands to run ocamlformat
- DO NOT guess at formatting rules

PARAMETERS:
- file_path (required): Path to the .ml or .mli file to format
  - Must be a valid path relative to workspace root or absolute
- check_only (optional): If true, only check if formatting is needed without modifying the file
  - Defaults to false (will format the file)

RETURNS:
Returns FormatResult with:
- formatted_code: The formatted code (empty string if check_only is true)
- changed: Boolean indicating whether the file needed formatting
- file_path: The path that was formatted

Or returns an Error if formatting fails.

EXAMPLES:
- Format a file: {file_path: "packages/my-lib/src/utils.ml"}
- Check formatting: {file_path: "packages/my-lib/src/utils.ml", check_only: true}

This tool uses the project's .ocamlformat configuration automatically.|}

let tool =
  let open Mcp in
  let input_schema =
    Json.Object
      [
        ("type", Json.String "object");
        ( "properties",
          Json.Object
            [
              ( "file_path",
                Json.Object
                  [
                    ("type", Json.String "string");
                    ("description", Json.String "Path to file to format");
                  ] );
              ( "check_only",
                Json.Object
                  [
                    ("type", Json.String "boolean");
                    ( "description",
                      Json.String
                        "Only check if formatting needed (don't modify file)" );
                  ] );
            ] );
        ("required", Json.Array [ Json.String "file_path" ]);
      ]
  in
  { name = "formatFile"; description = Some description; input_schema }

type request = { file_path : string; check_only : bool }

type response =
  | FormatResult of {
      formatted_code : string;
      changed : bool;
      file_path : string;
    }
  | Error of string

let execute (client : Tusk_client.t) (req : request) : response =
  Log.debug
    ("[FORMAT_FILE TOOL] execute() called with file_path: " ^ req.file_path
    ^ ", check_only: " ^ Bool.to_string req.check_only);

  let result =
    Tusk_client.format_file client ~file_path:req.file_path
      ~check_only:req.check_only
  in

  match result with
  | Ok (formatted_code, changed) ->
      Log.debug
        ("[FORMAT_FILE TOOL] Formatting completed, changed: "
        ^ Bool.to_string changed);
      FormatResult { formatted_code; changed; file_path = req.file_path }
  | Error msg ->
      Log.error ("[FORMAT_FILE TOOL] Error: " ^ msg);
      Error msg

let response_to_json = function
  | FormatResult { formatted_code; changed; file_path } ->
      let message =
        if changed then "File '" ^ file_path ^ "' was formatted"
        else "File '" ^ file_path ^ "' is already formatted correctly"
      in
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
