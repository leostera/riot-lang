open Std
module Test = Std.Test
module Kernel = Kernel

let test_error_envelope_reports_module_and_system = fun _ctx ->
  let error = Kernel.Error.from_env (Kernel.Env.System Kernel.SystemError.PermissionDenied) in
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
  let error = Kernel.Error.from_process
    (Kernel.Process.File (Kernel.Fs.File.System Kernel.SystemError.BadFileDescriptor)) in
  if
    Kernel.String.equal (Kernel.Error.module_name error) "process"
    && Kernel.Error.system error = Some Kernel.SystemError.BadFileDescriptor
  then
    Ok ()
  else
    Error "expected process file errors to expose their nested system cause"

let test_error_envelope_reports_stable_module_tags = fun _ctx ->
  let read_dir = Kernel.Error.from_fs_read_dir Kernel.Fs.ReadDir.Closed in
  let net_addr = Kernel.Error.from_net_addr (Kernel.Net.Addr.HostNotFound { host = "localhost" }) in
  let socket_addr = Kernel.Error.from_net_socket_addr
    (Kernel.Net.SocketAddr.InvalidPort { port = (-1) }) in
  let timer = Kernel.Error.from_time_timer
    (Kernel.Time.Timer.InvalidTimeoutNs { timeout_ns = (-1L) }) in
  if
    Kernel.String.equal (Kernel.Error.module_name read_dir) "fs.read_dir"
    && Kernel.String.equal (Kernel.Error.module_name net_addr) "net.addr"
    && Kernel.String.equal (Kernel.Error.module_name socket_addr) "net.socket_addr"
    && Kernel.String.equal (Kernel.Error.module_name timer) "time.timer"
  then
    Ok ()
  else
    Error "expected envelope helpers to report stable module tags"

let test_error_envelope_system_is_none_for_non_system_errors = fun _ctx ->
  let ip_addr = Kernel.Error.from_net_ip_addr (Kernel.Net.IpAddr.InvalidText { value = "not-an-ip" }) in
  let timer = Kernel.Error.from_time_timer (Kernel.Time.Timer.InvalidTimeoutNs { timeout_ns = 0L }) in
  if Kernel.Error.system ip_addr = None && Kernel.Error.system timer = None then
    Ok ()
  else
    Error "expected envelope helpers to keep non-system errors distinct"

let test_error_envelope_extracts_nested_read_dir_file_system_errors = fun _ctx ->
  let error = Kernel.Error.from_fs_read_dir
    (Kernel.Fs.ReadDir.File (Kernel.Fs.File.System Kernel.SystemError.NoSuchFileOrDirectory)) in
  if
    Kernel.String.equal (Kernel.Error.module_name error) "fs.read_dir"
    && Kernel.Error.system error = Some Kernel.SystemError.NoSuchFileOrDirectory
  then
    Ok ()
  else
    Error "expected read_dir file errors to expose their nested system cause"

let test_system_error_of_code_maps_known_values = fun _ctx ->
  if
    Kernel.SystemError.from_code 3 = Kernel.SystemError.NoSuchFileOrDirectory
    && Kernel.SystemError.from_code 12 = Kernel.SystemError.WouldBlock
    && Kernel.SystemError.from_code 27 = Kernel.SystemError.DirectoryNotEmpty
  then
    Ok ()
  else
    Error "expected SystemError.from_code to keep representative known mappings stable"

let test_system_error_of_code_preserves_unknown_payloads = fun _ctx ->
  match Kernel.SystemError.from_code 9_999 with
  | Kernel.SystemError.Unknown 9_999 ->
      if Kernel.SystemError.to_string (Kernel.SystemError.Unknown 9_999) = "unknown kernel error" then
        Ok ()
      else
        Error "expected unknown system errors to keep the stable textual fallback"
  | _ -> Error "expected SystemError.from_code to preserve unknown numeric payloads"

let test_error_to_string_prefixes_the_stable_module_name = fun _ctx ->
  let error = Kernel.Error.from_net_addr (Kernel.Net.Addr.HostNotFound { host = "riot.invalid" })
  |> Kernel.Error.to_string in
  if String.starts_with ~prefix:"net.addr: " error then
    Ok ()
  else
    Error "expected Error.to_string to prefix the stable module name"

let tests = [
  Test.case "Error envelope reports module and system context" test_error_envelope_reports_module_and_system;
  Test.case "Error envelope extracts nested process file system errors" test_error_envelope_extracts_nested_process_file_system_errors;
  Test.case "Error envelope reports stable module tags" test_error_envelope_reports_stable_module_tags;
  Test.case "Error envelope keeps non-system errors distinct" test_error_envelope_system_is_none_for_non_system_errors;
  Test.case "Error envelope extracts nested read_dir file system errors" test_error_envelope_extracts_nested_read_dir_file_system_errors;
  Test.case "SystemError.from_code maps representative known values" test_system_error_of_code_maps_known_values;
  Test.case "SystemError.from_code preserves unknown payloads" test_system_error_of_code_preserves_unknown_payloads;
  Test.case "Error.to_string prefixes the stable module name" test_error_to_string_prefixes_the_stable_module_name;
]

let main ~args = Test.Cli.main ~name:"kernel_new_error_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
