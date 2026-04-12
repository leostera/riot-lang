open Std
module Test = Std.Test

let stderr_payload_size = 256 * 1_024

let stderr_payload = String.make stderr_payload_size 'e'

let stdout_payload = "stdout-finished"

let self_executable = fun () ->
  match Env.args with
  | exe :: _ -> exe
  | [] -> panic "missing argv[0] for std_command_tests"

let run_capture = fun () ->
  let cmd = Command.make (self_executable ()) ~args:[ "capture-both-streams" ] in
  Command.output cmd |> Result.expect ~msg:"failed to run capture helper"

let test_command_output_drains_stdout_and_stderr = fun _ctx ->
  let output = run_capture () in
  if not (Int.equal output.status 0) then
    Error ("expected capture helper to exit 0, got " ^ Int.to_string output.status)
  else if not (String.equal output.stdout stdout_payload) then
    Error ("unexpected stdout payload: " ^ output.stdout)
  else if not (Int.equal (String.length output.stderr) stderr_payload_size) then
    Error ("unexpected stderr length: " ^ Int.to_string (String.length output.stderr))
  else
    Ok ()

let meta_tests = [
  Test.case "command output drains stdout and stderr without deadlock" test_command_output_drains_stdout_and_stderr;
]

let capture_main = fun () ->
  ignore (Unix.alarm 1);
  eprint stderr_payload;
  print stdout_payload;
  ignore (Unix.alarm 0);
  Ok ()

let meta_main = fun ~args ->
  let normalize_args = function
    | [] -> [ "std_command_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  Test.Cli.main ~name:"std_command_tests" ~tests:meta_tests ~args:(normalize_args args)

let main = fun ~args ->
  match args with
  | _ :: "capture-both-streams" :: _ -> capture_main ()
  | _ -> meta_main ~args

let () = Runtime.run ~main ~args:Env.args ()
