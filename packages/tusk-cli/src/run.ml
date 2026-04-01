open Std
open Tusk_model
open Tusk_build
open ArgParser

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

let build_scope_for_binary = Tusk_build.build_scope_for_binary

let write_run_event = function
  | Tusk_build.Build _ ->
      ()
  | Tusk_build.RunningBinary { package; binary; _ } ->
      println ("     \027[1;32mRunning\027[0m " ^ package ^ ":" ^ binary)

let write_run_error = function
  | Tusk_build.BinaryNotFound { binary_name } ->
      println ("error: binary '" ^ binary_name ^ "' not found")
  | err ->
      println ("error: " ^ Tusk_build.run_error_message err)

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
          let (workspace, load_errors) = Workspace_manager.scan cwd |> Result.expect ~msg:"Failed to scan workspace" in
          match
            Tusk_build.run
              ~on_event:write_run_event
              {
                workspace;
                load_errors;
                current_dir = cwd;
                package_name = pkg_filter;
                binary_name = bin_name;
                args = extra;
              }
          with
          | Ok () ->
              Ok ()
          | Error err ->
              write_run_error err;
              Error (Failure (Tusk_build.run_error_message err))
        )
    )
