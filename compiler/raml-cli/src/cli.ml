open Std

let ( let* ) = Result.and_then

let runtime_asset_names = [ "riot-runtime.js"; "Printf.js" ]

let is_ascii_digit = fun char -> char >= '0' && char <= '9'

type output_mode =
  | Human
  | Json

let command =
  let open ArgParser in
    let open Arg in command "raml"
    |> version "0.1.0"
    |> about "Compile OCaml source files with Raml"
    |> args
      [
        positional "source" |> required false |> help "OCaml source file to compile";
        option "target"
        |> short 'x'
        |> long "target"
        |> help "Target triple (currently only JS targets are supported here)";
        option "output"
        |> short 'o'
        |> long "output"
        |> help "Output file path for the emitted artifact";
        flag "json"
        |> long "json"
        |> help "Emit machine-readable JSONL compiler events";
      ]

let output_mode_of_matches = fun matches ->
  if ArgParser.get_flag matches "json" then
    Json
  else
    Human

let output_mode_of_args = fun args ->
  if List.exists (String.equal "--json") args then
    Json
  else
    Human

let write_json_line = fun json ->
  print (Std.Data.Json.to_string json);
  print "\n"

let write_cli_error = fun ~mode message ->
  match mode with
  | Human ->
      eprintln ("\027[1;31mError\027[0m: " ^ message)
  | Json ->
      write_json_line
        (Std.Data.Json.obj
           [
             ("kind", Std.Data.Json.string "cli_error");
             ("message", Std.Data.Json.string message);
           ])

let write_artifact_written = fun ~mode ~path ->
  match mode with
  | Human ->
      println ("Wrote " ^ Path.to_string path)
  | Json ->
      write_json_line
        (Std.Data.Json.obj
           [
             ("kind", Std.Data.Json.string "artifact_written");
             ("path", Std.Data.Json.string (Path.to_string path));
           ])

let path_error_message = function
  | Path.InvalidUtf8 { path } -> "invalid UTF-8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } ->
      "system call '" ^ syscall ^ "' returned invalid UTF-8 path: " ^ path
  | Path.SystemError error ->
      error

let path_of_string = fun ~kind value ->
  Path.of_string value
  |> Result.map_error (fun error ->
       "invalid " ^ kind ^ " path '" ^ value ^ "': " ^ path_error_message error)

let js_target_of_string = fun value ->
  let* target = Raml.Target.of_string value in
  match Raml.Target.backend target with
  | Raml.Target.Js ->
      Ok target
  | Raml.Target.Native
  | Raml.Target.Wasm ->
      Error
        ("raml CLI currently only emits JS artifacts; got target "
        ^ Raml.Target.to_string target)

let strip_ordering_prefix = fun name ->
  let len = String.length name in
  let rec consume_digits index =
    if index < len && is_ascii_digit name.[index] then
      consume_digits (index + 1)
    else
      index
  in
  let prefix_end = consume_digits 0 in
  if prefix_end > 0 && prefix_end < len && name.[prefix_end] = '_' then
    String.sub name (prefix_end + 1) (len - prefix_end - 1)
  else
    name

let logical_relpath = fun path ->
  let basename = Path.basename path |> strip_ordering_prefix |> Path.v in
  let dirname = Path.dirname path in
  if String.equal (Path.to_string dirname) "." then
    basename
  else
    Path.join dirname basename

let runtime_asset_source = fun ~repo_root name ->
  Path.(repo_root / Path.v "compiler/raml/src/js" / Path.v name)

let runtime_assets_available = fun repo_root ->
  List.for_all
    (fun name ->
      Fs.exists (runtime_asset_source ~repo_root name)
      |> Result.unwrap_or ~default:false)
    runtime_asset_names

let rec find_repo_root = fun dir ->
  if runtime_assets_available dir then
    Ok dir
  else
    match Path.parent dir with
    | Some parent when not (String.equal (Path.to_string parent) (Path.to_string dir)) ->
        find_repo_root parent
    | _ ->
        Error "could not locate Riot repo root for JS runtime assets"

let copy_runtime_assets = fun ~output_dir ->
  let* cwd =
    Env.current_dir ()
    |> Result.map_error (fun error ->
         "failed to determine current directory: " ^ path_error_message error)
  in
  let* repo_root = find_repo_root cwd in
  List.fold_left
    (fun result name ->
      let* () = result in
      Fs.copy
        ~src:(runtime_asset_source ~repo_root name)
        ~dst:Path.(output_dir / Path.v name)
      |> Result.map_error (fun error ->
           "failed to copy JS runtime asset '" ^ name ^ "': "
           ^ IO.error_message error))
    (Ok ())
    runtime_asset_names

let write_output = fun ~output_path output ->
  let output_dir = Path.dirname output_path in
  let* () =
    Fs.create_dir_all output_dir
    |> Result.map_error (fun error ->
         "failed to create output directory: " ^ IO.error_message error)
  in
  let* () =
    Fs.write output output_path
    |> Result.map_error (fun error ->
         "failed to write output artifact: " ^ IO.error_message error)
  in
  copy_runtime_assets ~output_dir

let run = fun matches ->
  let mode = output_mode_of_matches matches in
  let* source_raw =
    ArgParser.get_one matches "source"
    |> Option.to_result ~error:"missing required argument: source"
  in
  let* target_raw =
    ArgParser.get_one matches "target"
    |> Option.to_result ~error:"missing required option: --target"
  in
  let* output_raw =
    ArgParser.get_one matches "output"
    |> Option.to_result ~error:"missing required option: --output"
  in
  let* source_path = path_of_string ~kind:"source" source_raw in
  let* target = js_target_of_string target_raw in
  let* output_path = path_of_string ~kind:"output" output_raw in
  let config =
    (* NOTE: until the compiler has a real public prelude/runtime surface,
       the CLI uses the same prototype ambient typing surface as the current
       source-driven fixture corpus. *)
    let config = Raml.Config.make
      ~target
      ~typing_config:Raml.TestingHelpers.Test_fixture_typing.typing_config
      ()
    in
    match mode with
    | Human ->
        config
    | Json ->
        Raml.Config.with_on_event
          config
          ~on_event:(fun event -> write_json_line (Raml.Event.to_json event))
  in
  let* source =
    Fs.read source_path
    |> Result.map_error (fun error ->
         "failed to read source file: " ^ IO.error_message error)
  in
  let* compilation =
    Raml.TestingHelpers.compile_source
      ~config
      ~relpath:(logical_relpath source_path)
      source
    |> Result.map_error (fun error -> "compile failed: " ^ error)
  in
  let* output =
    Raml.Compilation.emitted_output compilation
    |> Result.map_error (fun error -> "codegen failed: " ^ error)
  in
  let* () = write_output ~output_path output in
  write_artifact_written ~mode ~path:output_path;
  Ok ()

let main = fun ~args ->
  match ArgParser.get_matches command args with
  | Error error ->
      let mode = output_mode_of_args args in
      let message = ArgParser.error_message error in
      (match mode with
      | Human ->
          ArgParser.print_error error;
          ArgParser.print_help command
      | Json ->
          write_cli_error ~mode message);
      Error (Failure (ArgParser.error_message error))
  | Ok matches -> (
      let mode = output_mode_of_matches matches in
      match run matches with
      | Ok () ->
          Ok ()
      | Error message ->
          write_cli_error ~mode message;
          Error (Failure message)
    )
