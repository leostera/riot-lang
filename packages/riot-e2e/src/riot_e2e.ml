open Std
open Std.Result.Syntax
module Test = Std.Test

type command_output = Command.output

let command_error_message = function
  | Command.SystemError message -> message

let render_output = fun (output: command_output) ->
  let stdout =
    if String.equal output.stdout "" then
      "<empty>"
    else
      output.stdout
  in
  let stderr =
    if String.equal output.stderr "" then
      "<empty>"
    else
      output.stderr
  in
  "status=" ^ Int.to_string output.status ^ ", stdout=" ^ stdout ^ ", stderr=" ^ stderr

let run_binary = fun ?cwd ?(env = []) binary_path args ->
  let cwd = Option.map cwd ~fn:Path.to_string in
  Command.make (Path.to_string binary_path) ?cwd ~env ~args |> Command.output |> Result.map_err ~fn:command_error_message

let run_riot = fun ctx ?cwd ?(env = []) args ->
  let* riot_binary_path = Test.Context.require_binary ctx "riot" in
  run_binary ?cwd ~env riot_binary_path args

let expect_success = fun ~cmd (output: command_output) ->
  if Int.equal output.status 0 then
    Ok output
  else
    Error (cmd ^ " failed: " ^ render_output output)

let expect_failure_contains = fun ~cmd ~needle (output: command_output) ->
  let text = output.stdout ^ output.stderr in
  if Int.equal output.status 0 then
    Error (cmd ^ " unexpectedly succeeded: " ^ render_output output)
  else if String.contains text needle then
    Ok output
  else
    Error (cmd ^ " failed without expected text `" ^ needle ^ "`: " ^ render_output output)

let assert_exists = fun path ->
  match Fs.exists path with
  | Ok true -> Ok ()
  | Ok false -> Error ("expected path to exist: " ^ Path.to_string path)
  | Error err -> Error ("failed to stat path " ^ Path.to_string path ^ ": " ^ IO.error_message err)

let assert_contains = fun path needle ->
  let* content = Fs.read_to_string path |> Result.map_err ~fn:IO.error_message in
  if String.contains content needle then
    Ok ()
  else
    Error ("expected " ^ Path.to_string path ^ " to contain `" ^ needle ^ "`")

let with_tempdir_result = fun ?prefix fn ->
  match Fs.with_tempdir ?prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)
