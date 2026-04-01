open Std
open Tusk_model
open Tusk_model
open Tusk_build
open ArgParser

let reconnect = fun ~workspace -> Client.connect_local ~workspace () |> Result.expect ~msg:"Failed to start local tusk session"

let command =
  let open ArgParser in
    let open Arg in
      command "run" |> about "Run a binary" |> ArgParser.allow_trailing_args |> args
        [ positional "name" |> help
            "Binary name to run. Use -p/--package to disambiguate, or the \
               legacy [package:]binary form"; option "package"
          |> short 'p'
          |> long "package"
          |> help "Run a binary from a specific package"; trailing "-- [args]..." |> help "Arguments to pass to the binary"; flag
            "verbose"
          |> short 'v'
          |> long "verbose"
          |> help "Enable verbose output for run"
          |> count; ]

let pick_name = fun matches -> get_one matches "name"

let trailing_args = fun matches ->
  let args = ArgParser.trailing_args matches in
  match args with
  | "--" :: rest -> rest
  | _ -> args

let build_scope_for_binary = fun (workspace: Workspace.t) ~package_name ~binary_name ->
  match
    List.find_opt
      (fun (pkg: Package.t) ->
        String.equal pkg.name package_name)
      workspace.packages
  with
  | None -> Build.Runtime
  | Some pkg -> (
      match Package.scope_of_binary_name pkg ~binary_name with
      | Some Package.Dev -> Build.Dev
      | Some Package.Normal
      | Some Package.Build
      | None -> Build.Runtime
    )

let run = fun matches ->
  match pick_name matches with
  | None ->
      println "error: missing binary name";
      Error (Failure "missing binary name")
  | Some name -> (
      let extra = trailing_args matches in
      let verbose = ArgParser.get_count matches "verbose" in
      let _ = verbose in
      let explicit_package = ArgParser.get_one matches "package" in
      let (legacy_package, bin_name) =
        match String.split_on_char ':' name with
        | [pkg;bin] -> (Some pkg, bin)
        | _ -> (None, name)
      in
      let pkg_filter =
        match (explicit_package, legacy_package) with
        | (Some explicit_package, Some legacy_package) when not
          (String.equal explicit_package legacy_package) -> None
        | (Some explicit_package, _) -> Some explicit_package
        | (None, legacy_package) -> legacy_package
      in
      let has_conflicting_package_filters =
        match (explicit_package, legacy_package) with
        | (Some explicit_package, Some legacy_package) -> not
          (String.equal explicit_package legacy_package)
        | _ -> false
      in
      if has_conflicting_package_filters then
        (
          println
            ("error: conflicting package filters '"
            ^ (explicit_package |> Option.unwrap_or ~default:"")
            ^ "' and '"
            ^ (legacy_package |> Option.unwrap_or ~default:"")
            ^ "'");
          Error (Failure "conflicting package filters")
        )
      else
        (
          let cwd = Env.current_dir () |> Result.expect ~msg:"Failed to get current directory" in
          let (workspace, _load_errors) = Workspace_manager.scan cwd |> Result.expect ~msg:"Failed to scan workspace" in
          let client = Client.connect_local ~workspace () |> Result.expect ~msg:"Failed to start local tusk session" in
          let _ = Client.scan_workspace client ~current_dir:cwd |> Result.expect ~msg:"Failed to scan workspace" in
          let result =
            match Client.find_executable client bin_name with
            | Ok (Some (pkg, _binary)) -> (
                match pkg_filter with
                | Some expected_pkg when expected_pkg != pkg ->
                    println
                      ("error: binary '" ^ bin_name ^ "' not found in package '" ^ expected_pkg ^ "'");
                    Error (Failure "binary not found in specified package")
                | _ -> (
                    let build_scope = build_scope_for_binary
                      workspace
                      ~package_name:pkg
                      ~binary_name:bin_name in
                    match Build.build_command ~scope:build_scope (Some pkg) None with
                    | Ok () ->
                        let refreshed_client = reconnect ~workspace in
                        let artifact_result = Client.find_artifact
                          refreshed_client
                          ~package:pkg
                          ~kind:"binary"
                          ~name:bin_name in
                        let result =
                          match artifact_result with
                          | Ok path ->
                              println ("     \027[1;32mRunning\027[0m " ^ pkg ^ ":" ^ bin_name);
                              let cmd = Command.make path ~args:extra in
                              (
                                match Command.status cmd with
                                | Ok 0 ->
                                    Ok ()
                                | Ok code ->
                                    println ("error: process exited with " ^ Int.to_string code);
                                    Error (Failure ("process exited with " ^ Int.to_string code))
                                | Error (Command.SystemError msg) ->
                                    println ("error: " ^ msg);
                                    Error (Failure msg)
                              )
                          | Error msg ->
                              println ("error: " ^ msg);
                              Error (Failure msg)
                        in
                        Client.close refreshed_client;
                        result
                    | Error _ ->
                        println ("error: build failed for package '" ^ pkg ^ "'");
                        Error (Failure "build failed")
                  )
              )
            | Ok None ->
                println ("error: binary '" ^ name ^ "' not found");
                Error (Failure "binary not found")
            | Error msg ->
                println ("error: " ^ msg);
                Error (Failure msg)
          in
          Client.close client;
          result
        )
    )
