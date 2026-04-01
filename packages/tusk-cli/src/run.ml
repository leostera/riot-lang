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

let trailing_args = fun matches ->
  let args = ArgParser.trailing_args matches in
  match args with
  | "--" :: rest -> rest
  | _ -> args

let build_scope_for_binary = Tusk_build.build_scope_for_binary

let parse_binary_target = fun ?package_filter name ->
  match String.split_on_char ':' name with
  | [package_name;binary_name] -> (
      match package_filter with
      | Some expected_package when not (String.equal expected_package package_name) -> Error (Failure ("conflicting package filters: got --package "
      ^ expected_package
      ^ " and binary target "
      ^ name))
      | _ -> Ok (Some package_name, binary_name)
    )
  | _ -> Ok (package_filter, name)

let write_run_event = fun (event: Tusk_build.run_event) ->
  match event with
  | Tusk_build.Build _ -> ()
  | Tusk_build.RunningBinary { package; binary; _ } -> println
    ("    \027[1;32mBuilding\027[0m " ^ package ^ ":" ^ binary)

let write_run_error = fun (err: Tusk_build.run_error) ->
  match err with
  | Tusk_build.BinaryNotFound { binary_name } -> println
    ("error: binary '" ^ binary_name ^ "' not found")
  | err -> println ("error: " ^ Tusk_build.run_error_message err)

let run = fun ~workspace matches ->
  let seen_registry_updates = Collections.HashSet.create () in
  let displayed_packages = Collections.HashSet.create () in
  let progress = Build.{ built_count = 0; cached_count = 0; failed_count = 0; skipped_count = 0 } in
  match ArgParser.get_one matches "name" with
  | None ->
      println "error: missing binary name";
      Error (Failure "missing binary name")
  | Some name -> (
      let extra = trailing_args matches in
      let _verbose = ArgParser.get_count matches "verbose" in
      let pkg_filter = ArgParser.get_one matches "package" in
      match parse_binary_target ?package_filter:pkg_filter name with
      | Error _ as err -> err
      | Ok (package_name, binary_name) ->
          let on_event (event: Tusk_build.run_event) =
            match event with
            | Tusk_build.Build build_event -> (
                match build_event with
                | Tusk_build.Pm kind -> Build.write_pm_event
                  ~mode:Build.Human
                  ~seen_registry_updates
                  kind
                | Tusk_build.BuildingTarget { target; host } -> Build.write_building_target_event
                  ~mode:Build.Human
                  ~target
                  ~host
                | Tusk_build.Streaming streaming_event -> Build.write_streaming_event
                  ~mode:Build.Human
                  ~displayed_packages
                  ~progress
                  streaming_event
              )
            | _ -> write_run_event event
          in
          match Tusk_build.run ~on_event { workspace; package_name; binary_name; args = extra } with
          | Ok () -> Ok ()
          | Error err ->
              write_run_error err;
              Error (Failure (Tusk_build.run_error_message err))
    )
