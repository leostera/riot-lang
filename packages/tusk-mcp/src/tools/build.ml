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
  let result =
    Tusk_client.build_streaming client build_request (function
      | Tusk_client.BuildStarted _ ->
          Log.debug "[BUILD TOOL] BuildStarted callback";
          messages := !messages @ [ "Build started" ]
      | Tusk_client.BuildEvent _event ->
          Log.debug
            "[BUILD TOOL] BuildEvent callback (telemetry events not yet \
             processed)"
      | Tusk_client.BuildCompleted { stats; _ } ->
          Log.debug "[BUILD TOOL] BuildCompleted callback";
          messages :=
            !messages
            @ [
                format
                  "Build completed successfully (%d packages built, %d from \
                   cache)"
                  stats.packages_built stats.cache_hits;
              ]
      | Tusk_client.BuildFailed { errors; stats; _ } ->
          Log.debug "[BUILD TOOL] BuildFailed callback";
          let failed_packages =
            List.map
              (fun (r : Tusk_protocol.WireProtocol.build_result) ->
                r.package.name)
              errors
          in
          messages :=
            !messages
            @ [
                format "Build failed: %s (%d packages built, %d failed)"
                  (String.concat ", " failed_packages)
                  stats.packages_built stats.packages_failed;
              ])
  in
  Log.debug "[BUILD TOOL] build_streaming returned, processing result";
  match result with
  | Ok (Tusk_client.BuildCompleted _) ->
      Log.debug "[BUILD TOOL] returning BuildResult";
      BuildResult { messages = !messages }
  | Ok (Tusk_client.BuildFailed _) ->
      Log.debug "[BUILD TOOL] returning Error from BuildFailed";
      Error (String.concat "\n" !messages)
  | Error e ->
      Log.debug "[BUILD TOOL] returning Error from client error";
      let error_msg =
        match e with
        | Tusk_client.JsonrpcError je -> Tusk_client.jsonrpc_error_to_string je
        | Tusk_client.PackageNotFound { package_name; available_packages } ->
            format "Package not found: %s (available: %s)" package_name
              (String.concat ", " available_packages)
        | Tusk_client.UnexpectedEvent { reason; _ } -> reason
      in
      Error error_msg
  | Ok _ ->
      Log.debug "[BUILD TOOL] Unexpected final event, returning messages";
      BuildResult { messages = !messages }

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
