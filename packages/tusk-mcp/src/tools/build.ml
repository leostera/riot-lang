open Std
open Std.Data
open Tusk_model

let description =
  {|Build OCaml packages in the tusk workspace. Use this instead of running 'dune build' or shell commands when you need to compile code.

WHEN TO USE THIS TOOL:
- When you need to compile OCaml code after making changes
- When verifying that code changes don't break the build
- When checking for compilation errors or type errors
- When preparing to run or test code
- Every time you need to compile code in the workspace

INSTEAD OF:
- DO NOT use 'dune build' or shell commands
- DO NOT use bash to run the compiler directly
- DO NOT manually invoke ocamlc or ocamlopt
- DO NOT use 'make' or other build tools

PARAMETERS:
- package (optional): Specific package name to build
  - If provided: builds only that package and its dependencies
  - If omitted: builds all packages in the workspace
  - Examples: "tusk", "std", "kernel"

RETURNS:
Returns a BuildResult with messages containing:
- Build status messages showing compilation progress
- Success/failure status for each package
- Compilation errors if any packages fail to build
- Clear indication of which packages were built from cache vs recompiled
- Overall build success or failure

Or returns an Error if the build system fails.

EXAMPLES:
- Build everything: {} or {package: null}
- Build specific package: {package: "tusk"}
- Build after code changes: {package: "std"}

USE THIS TOOL every time you need to compile OCaml code in the workspace.|}

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
                      Json.String
                        "Package name to build (optional, omit to build all \
                         packages)" );
                  ] );
            ] );
      ]
  in
  { name = "build"; description = Some description; input_schema }

type request = { package : string option }
type response = BuildResult of { messages : string list } | Error of string

let execute (client : Tusk_client.t) (req : request) : response =
  Log.debug "[BUILD TOOL] execute() called with package: %s"
    (Option.unwrap_or req.package ~default:"<all>");
  let build_request =
    match req.package with
    | Some pkg -> Tusk_client.BuildPackage pkg
    | None -> Tusk_client.BuildAll
  in
  Log.debug "[BUILD TOOL] Created build request, calling build_streaming";
  let messages = ref [] in
  let has_error = ref false in
  let result =
    Tusk_client.build_streaming client build_request (function
      | Tusk_client.BuildStarted _ ->
          Log.debug "[BUILD TOOL] BuildStarted callback";
          messages := !messages @ [ "Build started" ]
      | Tusk_client.BuildEvent event ->
          Log.debug "[BUILD TOOL] BuildEvent callback";
          let msg =
            match event.kind with
            | CacheHit { package; _ } -> format "Compiling %s (cached)" package
            | CacheMiss { package; _ } -> format "Compiling %s" package
            | PackageComplete { package; success; errors; _ } ->
                if success then format "✓ %s built successfully" package
                else
                  let error_msgs = List.map (fun e -> e.Event.raw) errors in
                  format "✗ %s failed: %s" package
                    (String.concat "; " error_msgs)
            | _ -> ""
          in
          if msg <> "" then messages := !messages @ [ msg ]
      | Tusk_client.BuildFinished (Ok ()) ->
          Log.debug "[BUILD TOOL] BuildFinished Ok callback";
          messages := !messages @ [ "Build completed successfully" ]
      | Tusk_client.BuildFinished (Error msg) ->
          Log.debug "[BUILD TOOL] BuildFinished Error callback";
          has_error := true;
          messages := !messages @ [ format "Build failed: %s" msg ])
  in
  Log.debug "[BUILD TOOL] build_streaming returned, processing result";
  match result with
  | Ok _ when !has_error ->
      Log.debug "[BUILD TOOL] returning Error from messages";
      Error (String.concat "\n" !messages)
  | Ok _ ->
      Log.debug "[BUILD TOOL] returning BuildResult";
      BuildResult { messages = !messages }
  | Error msg ->
      Log.debug "[BUILD TOOL] returning Error: %s" msg;
      Error msg

let response_to_json = function
  | BuildResult { messages } ->
      Json.Object
        [
          ( "content",
            Json.Array
              [
                Json.Object
                  [
                    ("type", Json.String "text");
                    ("text", Json.String (String.concat "\n" messages));
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
