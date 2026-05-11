open Std
open Propane

type point = { x: int; y: int }

let dummy_ctx: Test.ctx = {
  suite_name = "propane/integration_tests";
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

let point_arb =
  let gen =
    Generator.map
      (fun (x, y) -> { x; y })
      (Generator.pair (Generator.int_range 0 10) (Generator.int_range 0 10))
  in
  let shrink point =
    let xs = List.map (Shrinker.towards 0 point.x) ~fn:(fun x -> { x; y = point.y }) in
    let ys = List.map (Shrinker.towards 0 point.y) ~fn:(fun y -> { x = point.x; y }) in
    xs @ ys
  in
  let print point = "{ x = " ^ Int.to_string point.x ^ "; y = " ^ Int.to_string point.y ^ " }" in
  Arbitrary.make ~shrink ~print ~small:(fun point -> Int.abs point.x + Int.abs point.y) gen

let test_readme_style_property_runs_through_the_test_wrapper = fun _ctx ->
  let test_case =
    property
      "list reverse is involutive"
      Arbitrary.(list int)
      (fun values -> List.reverse (List.reverse values) = values)
  in
  match test_case.fn dummy_ctx with
  | Ok () -> Ok ()
  | Error err -> Error ("README-style property should succeed: " ^ err)

let test_custom_point_arbitrary_shrinks_toward_the_origin = fun _ctx ->
  let prop = Property.for_all point_arb (fun _point -> false) in
  match Property.check prop with
  | Property.Failure { counter_example; _ } ->
      if counter_example = "{ x = 0; y = 0 }" then
        Ok ()
      else
        Error "custom point arbitrary should shrink failing values toward the origin"
  | _ -> Error "expected the custom point property to fail"

let test_list_failures_shrink_to_a_singleton_minimum = fun _ctx ->
  let arb =
    Arbitrary.make
      ~shrink:(Shrinker.list Shrinker.int)
      ~print:(Printer.list Printer.int)
      ~small:List.length
      (Generator.return [ 3; 2 ])
  in
  let prop = Property.for_all arb List.is_empty in
  match Property.check prop with
  | Property.Failure { counter_example; _ } ->
      if counter_example = "[0]" then
        Ok ()
      else
        Error ("expected list shrinking to end at [0], got " ^ counter_example)
  | _ -> Error "expected the failing list property to produce a counter-example"

let test_option_shrinking_can_reach_none = fun _ctx ->
  let arb =
    Arbitrary.make
      ~shrink:(Shrinker.option Shrinker.int)
      ~print:(Printer.option Printer.int)
      ~small:(fun value ->
        match value with
        | None -> 0
        | Some payload -> 1 + Int.abs payload)
      (Generator.return (Some 10))
  in
  let prop =
    Property.for_all
      arb
      (fun value ->
        match value with
        | None -> false
        | Some payload -> payload < 0)
  in
  match Property.check prop with
  | Property.Failure { counter_example; _ } ->
      if counter_example = "None" then
        Ok ()
      else
        Error "option shrinking should be able to reach None when it is the true minimal failure"
  | _ -> Error "expected the option property to fail"

let test_hashmap_failures_use_a_stable_printer = fun _ctx ->
  let arb =
    Arbitrary.make
      ~print:(Printer.hashmap Printer.int Printer.int)
      (Generator.return (Collections.HashMap.from_list [ (2, 1); (1, 2); ]))
  in
  let test_case = Property.property "stable hashmap report" arb (fun _ -> false) in
  match test_case.fn dummy_ctx with
  | Error report ->
      if String.contains report "map{1 => 2; 2 => 1}" then
        Ok ()
      else
        Error "hashmap failure reports should use the stable printer ordering"
  | Ok () -> Error "expected the stable hashmap report property to fail"

let tests =
  Test.[
    case
      "readme style property runs through the test wrapper"
      test_readme_style_property_runs_through_the_test_wrapper;
    case
      "custom point arbitrary shrinks toward the origin"
      test_custom_point_arbitrary_shrinks_toward_the_origin;
    case
      "list failures shrink to a singleton minimum"
      test_list_failures_shrink_to_a_singleton_minimum;
    case "option shrinking can reach none" test_option_shrinking_can_reach_none;
    case "hashmap failures use a stable printer" test_hashmap_failures_use_a_stable_printer;
  ]

let main ~args = Test.Cli.main ~name:"propane/integration_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
