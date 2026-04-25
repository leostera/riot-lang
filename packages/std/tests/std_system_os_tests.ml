open Std

let test_os_predicates_match_current_os = fun _ctx ->
  match System.OS.current with
  | System.OS.Unix ->
      if System.unix && not System.win32 && not System.cygwin then
        Ok ()
      else Error "System unix flags should agree with System.OS.current"
  | System.OS.Win32 ->
      if System.win32 && not System.unix && not System.cygwin then
        Ok ()
      else Error "System win32 flags should agree with System.OS.current"
  | System.OS.Cygwin ->
      if System.cygwin && not System.win32 && not System.unix then
        Ok ()
      else Error "System cygwin flags should agree with System.OS.current"

let tests = Test.[ case "System OS predicates agree with the current OS" test_os_predicates_match_current_os ]

let main ~args = Test.Cli.main ~name:"std_system_os_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
