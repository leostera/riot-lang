open Std

let test_flush_without_log_config = fun _ctx ->
  Config.load_string "";
  let _ = Log.start_link () in
  yield ();
  Log.flush ();
  Ok ()

let tests =
  Test.[ case "Log.flush returns when no log config section exists" test_flush_without_log_config; ]

let main ~args = Test.Cli.main ~name:"std_log_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
