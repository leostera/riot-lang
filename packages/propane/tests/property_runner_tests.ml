open Std
open Propane

let dummy_ctx: Test.ctx = {
  suite_name = "propane/property_runner_tests";
  context_store = Test.Context.Store.create ();
  test_name = "dummy";
  test_index = 0;
  source_file = None;
  binary_path = None;
  built_binaries = [];
  workspace_root = None;
  package_name = Some "propane";
  fixture = None;
  progress_handler = Test.Context.no_progress_handler;
}

let with_env_bindings = fun bindings fn ->
  let saved = List.map bindings ~fn:(fun (name, _) -> (name, Env.get Env.String ~var:name)) in
  let restore () =
    List.for_each
      saved
      ~fn:(fun (name, value) ->
        match value with
        | Some old ->
            let _ = Env.set ~var:name ~value:old in
            ()
        | None ->
            let _ = Env.set ~var:name ~value:"" in
            ())
  in
  List.for_each
    bindings
    ~fn:(fun (name, value) ->
      match value with
      | Some current ->
          let _ = Env.set ~var:name ~value:current in
          ()
      | None ->
          let _ = Env.set ~var:name ~value:"" in
          ());
  try
    let result = fn () in
    restore ();
    result
  with
  | exn ->
      restore ();
      raise exn

let failing_int_arb =
  Arbitrary.make ~shrink:Shrinker.int ~print:Printer.int ~small:Int.abs (Generator.return 100)

let test_for_all_uses_the_default_name = fun _ctx ->
  let prop = Property.for_all Arbitrary.int (fun _ -> true) in
  if Property.get_name prop = "unnamed property" then
    Ok ()
  else
    Error "for_all should use the default display name"

let test_check_reports_success = fun _ctx ->
  let prop = Property.for_all Arbitrary.int (fun _ -> true) in
  match Property.check prop with
  | Property.Success -> Ok ()
  | _ -> Error "check should report success when every case passes"

let test_check_reports_failure_and_shrinks = fun _ctx ->
  let prop = Property.for_all failing_int_arb (fun value -> value < 0) in
  match Property.check prop with
  | Property.Failure { counter_example; shrink_steps } ->
      if counter_example = "0" && shrink_steps > 0 then
        Ok ()
      else
        Error "check should shrink failing integers toward 0"
  | _ -> Error "check should report a failing property"

let test_check_reports_custom_failure_messages = fun _ctx ->
  let prop = Property.for_all Arbitrary.int (fun _ -> fail "boom") in
  match Property.check prop with
  | Property.Failure { counter_example; _ } ->
      if String.contains counter_example "Message: boom" then
        Ok ()
      else
        Error "custom failure messages should be included in the failure report"
  | _ -> Error "fail should turn into a property failure"

let test_check_reports_exceptions = fun _ctx ->
  let prop = Property.for_all Arbitrary.int (fun _ -> raise (Failure "boom")) in
  match Property.check prop with
  | Property.Error { exception_; backtrace } ->
      if
        String.contains (Exception.to_string exception_) "boom"
        && String.contains backtrace "Backtrace:"
      then
        Ok ()
      else
        Error "unexpected exception reports should keep the exception and backtrace"
  | _ -> Error "unexpected exceptions should produce Property.Error"

let test_check_respects_max_shrink_steps = fun _ctx ->
  let shrink value =
    if value <= 0 then
      []
    else
      [ value - 1 ]
  in
  let arb = Arbitrary.make ~shrink ~print:Printer.int ~small:Int.abs (Generator.return 3) in
  let config = { Property.default_config with max_shrink_steps = 1 } in
  let prop = Property.for_all arb (fun _ -> false) in
  match Property.check ~config prop with
  | Property.Failure { counter_example; shrink_steps } ->
      if counter_example = "2" && shrink_steps = 1 then
        Ok ()
      else
        Error "check should stop shrinking after the configured number of steps"
  | _ -> Error "expected a failing property"

let test_check_is_reproducible_for_a_fixed_seed = fun _ctx ->
  let arb = Arbitrary.make ~print:Printer.int Generator.int in
  let prop = Property.for_all arb (fun _ -> false) in
  let config = { Property.default_config with test_count = 1; seed = Some 123 } in
  match (Property.check ~config prop, Property.check ~config prop) with
  | (
      Property.Failure { counter_example = left_counter_example; shrink_steps = left_shrink_steps },
      Property.Failure { counter_example = right_counter_example; shrink_steps = right_shrink_steps }
    ) ->
      if
        left_counter_example = right_counter_example && left_shrink_steps = right_shrink_steps
      then
        Ok ()
      else
        Error "fixed seeds should produce identical failures"
  | _ -> Error "expected both fixed-seed runs to fail in the same way"

let test_assume_false_discards_cases = fun _ctx ->
  let prop =
    Property.for_all
      Arbitrary.int
      (fun _ ->
        assume false;
        true)
  in
  match Property.check ~config:{ Property.default_config with test_count = 5 } prop with
  | Property.Assumption_violated -> Ok ()
  | _ -> Error "assume false should discard cases until the assumption budget is exhausted"

let test_implies_uses_assumption_semantics = fun _ctx ->
  let prop = Property.for_all Arbitrary.int (fun _ -> implies false false) in
  match Property.check ~config:{ Property.default_config with test_count = 5 } prop with
  | Property.Assumption_violated -> Ok ()
  | _ -> Error "implies false _ should discard cases, not fail them"

let test_missing_printer_uses_a_placeholder = fun _ctx ->
  let arb = Arbitrary.make ~shrink:Shrinker.int (Generator.return 10) in
  let prop = Property.for_all arb (fun _ -> false) in
  match Property.check prop with
  | Property.Failure { counter_example; _ } ->
      if counter_example = "<no printer available>" then
        Ok ()
      else
        Error "missing printers should produce a readable placeholder"
  | _ -> Error "expected a failing property"

let test_missing_shrinker_reports_the_original_value = fun _ctx ->
  let arb = Arbitrary.make ~print:Printer.int (Generator.return 42) in
  let prop = Property.for_all arb (fun _ -> false) in
  match Property.check prop with
  | Property.Failure { counter_example; shrink_steps } ->
      if counter_example = "42" && shrink_steps = 0 then
        Ok ()
      else
        Error "properties without shrinkers should report the original failing value"
  | _ -> Error "expected a failing property"

let test_small_influences_shrink_choice = fun _ctx ->
  let shrink _ = [ 20; 1 ] in
  let arb = Arbitrary.make ~shrink ~print:Printer.int ~small:Int.abs (Generator.return 100) in
  let prop = Property.for_all arb (fun _ -> false) in
  match Property.check prop with
  | Property.Failure { counter_example; _ } ->
      if counter_example = "1" then
        Ok ()
      else
        Error "small metrics should rank shrink candidates before recursive shrinking"
  | _ -> Error "expected a failing property"

let test_size_schedule_uses_the_configured_max_size = fun _ctx ->
  let seen = ref [] in
  let arb =
    Arbitrary.make ~print:Printer.int (Generator.sized (fun size -> Generator.return size))
  in
  let prop =
    Property.for_all
      arb
      (fun size ->
        seen := !seen @ [ size ];
        true)
  in
  let config = { Property.default_config with test_count = 5; max_size = 20; seed = Some 1 } in
  match Property.check ~config prop with
  | Property.Success ->
      if !seen = [ 0; 5; 10; 15; 20; ] then
        Ok ()
      else
        Error "size schedule should ramp from 0 to max_size during the property run"
  | _ -> Error "expected the size schedule property to pass"

let test_property_reads_PROPANE_TESTS = fun _ctx ->
  with_env_bindings
    [ ("PROPANE_TESTS", Some "17"); ]
    (fun () ->
      let test_case = Property.property "env examples" Arbitrary.int (fun _ -> true) in
      let expected = Test.property "expected examples" ~examples:17 (fun _ctx -> Ok ()) in
      if test_case.test_type = expected.test_type then
        Ok ()
      else
        Error "PROPANE_TESTS should control the example count of wrapped properties")

let test_property_reads_PROPANE_SEED = fun _ctx ->
  with_env_bindings
    [ ("PROPANE_TESTS", Some "1"); ("PROPANE_SEED", Some "123"); ]
    (fun () ->
      let arb = Arbitrary.make ~print:Printer.int Generator.int in
      let test_case = Property.property "env seed" arb (fun _ -> false) in
      match (test_case.fn dummy_ctx, test_case.fn dummy_ctx) with
      | (Error left, Error right) ->
          if left = right then
            Ok ()
          else
            Error "PROPANE_SEED should make wrapped property failures reproducible"
      | _ -> Error "expected the env-seeded property to fail twice in the same way")

let test_property_reads_PROPANE_MAX_SIZE = fun _ctx ->
  with_env_bindings
    [ ("PROPANE_TESTS", Some "5"); ("PROPANE_MAX_SIZE", Some "4"); ("PROPANE_SEED", Some "7"); ]
    (fun () ->
      let seen = ref [] in
      let arb =
        Arbitrary.make ~print:Printer.int (Generator.sized (fun size -> Generator.return size))
      in
      let test_case =
        Property.property
          "env max size"
          arb
          (fun size ->
            seen := !seen @ [ size ];
            true)
      in
      match test_case.fn dummy_ctx with
      | Ok () ->
          if !seen = [ 0; 1; 2; 3; 4; ] then
            Ok ()
          else
            Error "PROPANE_MAX_SIZE should drive the size growth of wrapped properties"
      | Error err -> Error ("wrapped property should have succeeded: " ^ err))

let tests =
  Test.[
    case "for_all uses the default name" test_for_all_uses_the_default_name;
    case "check reports success" test_check_reports_success;
    case "check reports failure and shrinks" test_check_reports_failure_and_shrinks;
    case "check reports custom failure messages" test_check_reports_custom_failure_messages;
    case "check reports exceptions" test_check_reports_exceptions;
    case "check respects max shrink steps" test_check_respects_max_shrink_steps;
    case "check is reproducible for a fixed seed" test_check_is_reproducible_for_a_fixed_seed;
    case "assume false discards cases" test_assume_false_discards_cases;
    case "implies uses assumption semantics" test_implies_uses_assumption_semantics;
    case "missing printer uses a placeholder" test_missing_printer_uses_a_placeholder;
    case
      "missing shrinker reports the original value"
      test_missing_shrinker_reports_the_original_value;
    case "small influences shrink choice" test_small_influences_shrink_choice;
    case
      "size schedule uses the configured max size"
      test_size_schedule_uses_the_configured_max_size;
    case "property reads PROPANE_TESTS" test_property_reads_PROPANE_TESTS;
    case "property reads PROPANE_SEED" test_property_reads_PROPANE_SEED;
    case "property reads PROPANE_MAX_SIZE" test_property_reads_PROPANE_MAX_SIZE;
  ]

let main ~args =
  Test.Cli.main
    ~execution_mode:Test.Cli.Linear
    ~name:"propane/property_runner_tests"
    ~tests
    ~args
    ()

let () = Runtime.run ~main ~args:Env.args ()
