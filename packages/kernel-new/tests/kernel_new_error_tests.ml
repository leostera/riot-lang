open Std
module Test = Std.Test
module Kernel = Kernel_new

let test_error_envelope_reports_module_and_system = fun _ctx ->
  let error = Kernel.Error.of_env (Kernel.Env.System Kernel.SystemError.PermissionDenied) in
  let module_name = Kernel.Error.module_name error in
  let system = Kernel.Error.system error in
  if
    Kernel.String.equal module_name "env"
    && system = Some Kernel.SystemError.PermissionDenied
    && Kernel.String.equal
      (Kernel.Error.to_string error)
      (Kernel.String.concat
        ""
        [ "env: "; Kernel.SystemError.to_string Kernel.SystemError.PermissionDenied ])
  then
    Ok ()
  else
    Error "expected envelope helpers to preserve module and system context"

let test_error_envelope_extracts_nested_process_file_system_errors = fun _ctx ->
  let error = Kernel.Error.of_process
    (Kernel.Process.File (Kernel.Fs.File.System Kernel.SystemError.BadFileDescriptor)) in
  if
    Kernel.String.equal (Kernel.Error.module_name error) "process"
    && Kernel.Error.system error = Some Kernel.SystemError.BadFileDescriptor
  then
    Ok ()
  else
    Error "expected process file errors to expose their nested system cause"

let tests = [
  Test.case "Error envelope reports module and system context" test_error_envelope_reports_module_and_system;
  Test.case "Error envelope extracts nested process file system errors" test_error_envelope_extracts_nested_process_file_system_errors;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_error_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
