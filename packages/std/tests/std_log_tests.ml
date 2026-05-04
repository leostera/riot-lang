open Std

let test_flush_without_log_config = fun _ctx ->
  Config.load_string "";
  let _ = Log.start_link () in
  Log.flush ();
  Ok ()

let with_env = fun var value fn ->
  let previous = Env.remove ~var in
  (
    match value with
    | Some value -> ignore (Env.set ~var ~value)
    | None -> ()
  );
  let result =
    try fn () with
    | exn ->
        (
          match previous with
          | Some value -> ignore (Env.set ~var ~value)
          | None -> ignore (Env.remove ~var)
        );
        raise exn
  in
  (
    match previous with
    | Some value -> ignore (Env.set ~var ~value)
    | None -> ignore (Env.remove ~var)
  );
  result

let test_start_link_reads_riot_log_level = fun _ctx ->
  let assert_riot_log_level = fun value expected ->
    with_env
      "RIOT_LOG"
      (Some value)
      (fun () ->
        Log.set_level Log.Info;
        let _ = Log.start_link () in
        let actual = Log.get_level () in
        Test.assert_equal ~expected:expected ~actual:actual)
  in
  let cases = [
    ("trace", Log.Trace);
    ("DEBUG", Log.Debug);
    ("Info", Log.Info);
    ("warn", Log.Warn);
    ("ERROR", Log.Error);
  ]
  in
  List.for_each cases ~fn:(fun (value, expected) -> assert_riot_log_level value expected);
  with_env
    "RIOT_LOG"
    (Some "loud")
    (fun () ->
      Log.set_level Log.Warn;
      let _ = Log.start_link () in
      Test.assert_equal ~expected:Log.Warn ~actual:(Log.get_level ()));
  Ok ()

let tests =
  Test.[
    case
      ~size:Large
      "Log.flush returns when no log config section exists"
      test_flush_without_log_config;
    case "Log.start_link reads RIOT_LOG level" test_start_link_reads_riot_log_level;
  ]

let main ~args = Test.Cli.main ~name:"std_log_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
