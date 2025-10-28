open Std

type Telemetry.event += TestEvent of { value : int }
type Telemetry.event += AnotherEvent of { name : string }

let test_emit_and_receive =
  Test.case "telemetry: emit and receive" @@ fun () ->
  let _pid = Telemetry.start () in
  Telemetry.detach_all ();

  let received = ref [] in

  Telemetry.attach "test-handler" (fun event ->
      match event with
      | TestEvent { value } -> received := value :: !received
      | _ -> ());

  Telemetry.emit (TestEvent { value = 42 });
  Telemetry.emit (TestEvent { value = 99 });

  Telemetry.stop ();

  match !received with
  | [ 99; 42 ] -> Ok ()
  | _ ->
      Error
        (Printf.sprintf "Expected [99; 42], got %s"
           (String.concat ", " (List.map string_of_int !received)))

let test_multiple_handlers =
  Test.case "telemetry: multiple handlers" @@ fun () ->
  let _pid = Telemetry.start () in
  Telemetry.detach_all ();

  let handler1_called = ref false in
  let handler2_called = ref false in

  Telemetry.attach "handler1" (fun event ->
      match event with TestEvent _ -> handler1_called := true | _ -> ());

  Telemetry.attach "handler2" (fun event ->
      match event with TestEvent _ -> handler2_called := true | _ -> ());

  Telemetry.emit (TestEvent { value = 1 });

  Telemetry.stop ();

  if !handler1_called && !handler2_called then Ok ()
  else Error "Both handlers should be called"

let test_handler_replacement =
  Test.case "telemetry: handler replacement" @@ fun () ->
  let _pid = Telemetry.start () in
  Telemetry.detach_all ();

  let first_called = ref false in
  let second_called = ref false in

  Telemetry.attach "my-handler" (fun _ -> first_called := true);
  Telemetry.attach "my-handler" (fun _ -> second_called := true);

  Telemetry.emit (TestEvent { value = 1 });

  Telemetry.stop ();

  if (not !first_called) && !second_called then Ok ()
  else Error "Only second handler should be called"

let test_detach =
  Test.case "telemetry: detach handler" @@ fun () ->
  let _pid = Telemetry.start () in
  Telemetry.detach_all ();

  let called = ref false in

  Telemetry.attach "test-handler" (fun _ -> called := true);
  Telemetry.detach "test-handler";

  Telemetry.emit (TestEvent { value = 1 });

  if not !called then Ok ()
  else Error "Handler should not be called after detach"

let test_pattern_matching =
  Test.case "telemetry: pattern matching" @@ fun () ->
  let _pid = Telemetry.start () in
  Telemetry.detach_all ();

  let test_count = ref 0 in
  let another_count = ref 0 in

  Telemetry.attach "counter" (fun event ->
      match event with
      | TestEvent _ -> test_count := !test_count + 1
      | AnotherEvent _ -> another_count := !another_count + 1
      | _ -> ());

  Telemetry.emit (TestEvent { value = 1 });
  Telemetry.emit (TestEvent { value = 2 });
  Telemetry.emit (AnotherEvent { name = "test" });

  Telemetry.stop ();

  if !test_count = 2 && !another_count = 1 then Ok ()
  else
    Error
      (Printf.sprintf "Expected test=2, another=1, got test=%d, another=%d"
         !test_count !another_count)

let test_handler_exception_isolation =
  Test.case "telemetry: exception isolation" @@ fun () ->
  let _pid = Telemetry.start () in
  Telemetry.detach_all ();

  let good_handler_called = ref false in

  Telemetry.attach "bad-handler" (fun _ -> failwith "boom");
  Telemetry.attach "good-handler" (fun _ -> good_handler_called := true);

  Telemetry.emit (TestEvent { value = 1 });

  Telemetry.stop ();

  if !good_handler_called then Ok ()
  else Error "Good handler should still be called despite bad handler exception"

let name = "Telemetry"

let tests =
  [
    test_emit_and_receive;
    test_multiple_handlers;
    test_handler_replacement;
    test_detach;
    test_pattern_matching;
    test_handler_exception_isolation;
  ]

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args
