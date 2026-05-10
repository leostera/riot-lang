open Std
open Std.Result.Syntax
open Riot_e2e

module Test = Std.Test

let write_text = fun path content ->
  let* () =
    match Path.parent path with
    | None -> Ok ()
    | Some parent ->
        Fs.create_dir_all parent
        |> Result.map_err ~fn:IO.error_message
  in
  Fs.write content path
  |> Result.map_err ~fn:IO.error_message

let nonempty_lines = fun text ->
  String.split ~by:"\n" text
  |> List.map ~fn:String.trim
  |> List.filter ~fn:(fun line -> not (String.equal line ""))

let parse_json_lines = fun ~cmd (output: command_output) ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | line :: rest ->
        match Data.Json.from_string line with
        | Ok json -> loop (json :: acc) rest
        | Error error ->
            Error (cmd
            ^ " emitted invalid JSON: "
            ^ Data.Json.error_to_string error
            ^ "\nline: "
            ^ line)
  in
  loop [] (nonempty_lines output.stdout)

let json_type_is = fun expected json ->
  match Data.Json.get_field "type" json with
  | Some value ->
      (match Data.Json.get_string value with
      | Some actual -> String.equal actual expected
      | None -> false)
  | None -> false

let json_int_field = fun name json ->
  match Data.Json.get_field name json with
  | Some value -> Data.Json.get_int value
  | None -> None

let json_positive_field = fun name json ->
  match json_int_field name json with
  | Some value -> value > 0
  | None -> false

let clean_plan_removes_cache = fun json ->
  if json_type_is "CacheGcPlanComputed" json then
    json_positive_field "deleted_entries" json || json_positive_field "deleted_generations" json
  else
    false

let clean_completed = fun json -> json_type_is "CacheGcCompleted" json

let assert_clean_removed_entries = fun (output: command_output) ->
  let* jsons = parse_json_lines ~cmd:"riot clean --json" output in
  if List.any jsons ~fn:clean_plan_removes_cache && List.any jsons ~fn:clean_completed then
    Ok ()
  else
    Error ("expected riot clean --json to report cache deletion and completion, got: "
    ^ render_output output)

let assert_fully_cached_build = fun (output: command_output) ->
  if String.contains output.stdout {|"type":"CacheMiss"|} then
    Error ("expected build after clean to be fully cached, got cache miss: " ^ render_output output)
  else if String.contains output.stdout {|"status":"fresh"|} then
    Error ("expected build after clean to use cached artifacts, got fresh build: "
    ^ render_output output)
  else
    Ok ()

let write_cache_policy = fun workspace_root ~keep_generations ->
  write_text
    Path.(workspace_root / Path.v ".riot" / Path.v "config.toml")
    ("[riot.cache]\nkeep_generations = "
    ^ Int.to_string keep_generations
    ^ "\nmax_size = \"10 GiB\"\n")

let write_minimal_workspace = fun workspace_root ->
  let package_root = Path.(workspace_root / Path.v "solo") in
  let source_path = Path.(package_root / Path.v "src" / Path.v "solo.ml") in
  let* () =
    write_text Path.(workspace_root / Path.v "riot.toml") "[workspace]\nmembers = [\"solo\"]\n"
  in
  let* () =
    write_text
      Path.(package_root / Path.v "riot.toml")
      {|[package]
name = "solo"
version = "0.1.0"
description = "riot clean e2e cache fixture"
license = "Apache-2.0"
public = false
|}
  in
  let* () = write_text source_path "let value = 1\n" in
  Ok source_path

let test_riot_clean_keeps_latest_generation_cached =
  Test.case
    ~size:Test.Large
    "riot clean with one generation preserves latest cached workspace build"
    (fun ctx ->
      with_tempdir_result
        ~prefix:"riot_e2e_clean_"
        (fun workspace_root ->
          let* source_path = write_minimal_workspace workspace_root in
          let* () = write_cache_policy workspace_root ~keep_generations:1 in
          let* first_build = run_riot ctx ~cwd:workspace_root [ "build"; "--json" ] in
          let* _ = expect_success ~cmd:"riot build --json" first_build in
          let* () = write_text source_path "let value = 2\n" in
          let* second_build = run_riot ctx ~cwd:workspace_root [ "build"; "--json" ] in
          let* _ = expect_success ~cmd:"riot build --json after source change" second_build in
          let* clean_output = run_riot ctx ~cwd:workspace_root [ "clean"; "--json" ] in
          let* clean_output = expect_success ~cmd:"riot clean --json" clean_output in
          let* () = assert_clean_removed_entries clean_output in
          let* final_build = run_riot ctx ~cwd:workspace_root [ "build"; "--json" ] in
          let* final_build = expect_success ~cmd:"riot build --json after clean" final_build in
          assert_fully_cached_build final_build))

let tests = [ test_riot_clean_keeps_latest_generation_cached ]

let main ~args =
  Test.Cli.main ~execution_mode:Test.Cli.Linear ~name:"riot-e2e:riot-clean" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
