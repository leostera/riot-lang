open Std
open Std.Data

let description =
  {|Format a snippet of OCaml code using ocamlformat.

WHEN TO USE THIS TOOL:
- When you have generated OCaml code and want to format it before writing to a file
- When you want to format code snippets for display or comparison
- To ensure generated code follows project formatting standards

INSTEAD OF:
- DO NOT manually format code snippets
- DO NOT write unformatted code to files

PARAMETERS:
- code (required): The OCaml code snippet to format
  - Can be any valid OCaml code (expressions, declarations, modules, etc.)
- file_path (optional): Hint for the parser (e.g., "hint.ml" or "hint.mli")
  - Helps ocamlformat understand the code context
  - Use ".ml" for implementation code, ".mli" for interface code

RETURNS:
Returns FormatResult with:
- formatted_code: The formatted code
- changed: Boolean indicating whether formatting changed the code

Or returns an Error if formatting fails.

EXAMPLES:
- Format code: {code: "let x=1+2"}
- Format with hint: {code: "val foo : int -> string", file_path: "hint.mli"}

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
              ( "code",
                Json.Object
                  [
                    ("type", Json.String "string");
                    ("description", Json.String "OCaml code to format");
                  ] );
              ( "file_path",
                Json.Object
                  [
                    ("type", Json.String "string");
                    ( "description",
                      Json.String
                        "Hint for parser (e.g., 'hint.ml' or 'hint.mli')" );
                  ] );
            ] );
        ("required", Json.Array [ Json.String "code" ]);
      ]
  in
  { name = "formatCode"; description = Some description; input_schema }

type request = { code : string; file_path : string option }

type response =
  | FormatResult of { formatted_code : string; changed : bool }
  | Error of string

let execute (client : Tusk_client.t) (req : request) : response =
  Log.debug "[FORMAT_CODE TOOL] execute() called";

  let result =
    Tusk_client.format_code client ~code:req.code ~file_path:req.file_path
  in

  match result with
  | Ok (formatted_code, changed) ->
      Log.debug "[FORMAT_CODE TOOL] Formatting completed, changed: %b" changed;
      FormatResult { formatted_code; changed }
  | Error msg ->
      Log.error "[FORMAT_CODE TOOL] Error: %s" msg;
      Error msg

let response_to_json = function
  | FormatResult { formatted_code; changed } ->
      Json.Object
        [
          ( "content",
            Json.Array
              [
                Json.Object
                  [
                    ("type", Json.String "text");
                    ("text", Json.String formatted_code);
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
