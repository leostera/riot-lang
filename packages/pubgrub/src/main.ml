open Std

let v major minor patch = Pubgrub.make_version ~major ~minor ~patch

let assert_solution expected_count result =
  match result with
  | Ok (Pubgrub.Solver.Success solution) ->
      if List.length solution = expected_count then Ok ()
      else Error (format "Wrong number of packages: got %d, expected %d"
               (List.length solution) expected_count)
  | Ok (Pubgrub.Solver.Failure _) -> Error "Unexpected conflict"
  | Error err -> Error (format "Error: %s" err)

let assert_conflict result =
  match result with
  | Ok (Pubgrub.Solver.Failure _) -> Ok ()
  | Ok (Pubgrub.Solver.Success _) -> Error "Expected conflict but found solution"
  | Error err -> Error (format "Error: %s" err)

let test_empty_root =
  Test.case "Empty root package" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [];
    assert_solution 1 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_single_dependency =
  Test.case "Single direct dependency" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("foo", Pubgrub.full)];
    Pubgrub.add_package provider "foo" (v 1 0 0) [];
    assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_two_dependencies =
  Test.case "Two independent dependencies" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("foo", Pubgrub.full); ("bar", Pubgrub.full)];
    Pubgrub.add_package provider "foo" (v 1 0 0) [];
    Pubgrub.add_package provider "bar" (v 1 0 0) [];
    assert_solution 3 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_transitive_chain =
  Test.case "Transitive dependency chain (3 levels)" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("a", Pubgrub.full)];
    Pubgrub.add_package provider "a" (v 1 0 0) [("b", Pubgrub.full)];
    Pubgrub.add_package provider "b" (v 1 0 0) [("c", Pubgrub.full)];
    Pubgrub.add_package provider "c" (v 1 0 0) [];
    assert_solution 4 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_deep_chain =
  Test.case "Deep dependency chain (5 levels)" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("a", Pubgrub.full)];
    Pubgrub.add_package provider "a" (v 1 0 0) [("b", Pubgrub.full)];
    Pubgrub.add_package provider "b" (v 1 0 0) [("c", Pubgrub.full)];
    Pubgrub.add_package provider "c" (v 1 0 0) [("d", Pubgrub.full)];
    Pubgrub.add_package provider "d" (v 1 0 0) [("e", Pubgrub.full)];
    Pubgrub.add_package provider "e" (v 1 0 0) [];
    assert_solution 6 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_diamond_dependency =
  Test.case "Diamond dependency" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("a", Pubgrub.full); ("b", Pubgrub.full)];
    Pubgrub.add_package provider "a" (v 1 0 0) [("c", Pubgrub.full)];
    Pubgrub.add_package provider "b" (v 1 0 0) [("c", Pubgrub.full)];
    Pubgrub.add_package provider "c" (v 1 0 0) [];
    assert_solution 4 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_multiple_versions_picks_latest =
  Test.case "Multiple versions available - picks latest" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("foo", Pubgrub.full)];
    Pubgrub.add_package provider "foo" (v 1 0 0) [];
    Pubgrub.add_package provider "foo" (v 1 5 0) [];
    Pubgrub.add_package provider "foo" (v 2 0 0) [];
    Pubgrub.add_package provider "foo" (v 2 1 0) [];
    assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_version_constraint_lower =
  Test.case "Version constraint: >= 2.0.0" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("foo", Pubgrub.higher_than (v 2 0 0))];
    Pubgrub.add_package provider "foo" (v 1 0 0) [];
    Pubgrub.add_package provider "foo" (v 2 0 0) [];
    Pubgrub.add_package provider "foo" (v 3 0 0) [];
    assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_version_constraint_upper =
  Test.case "Version constraint: < 2.0.0" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("foo", Pubgrub.strictly_lower_than (v 2 0 0))];
    Pubgrub.add_package provider "foo" (v 1 0 0) [];
    Pubgrub.add_package provider "foo" (v 1 5 0) [];
    Pubgrub.add_package provider "foo" (v 2 0 0) [];
    assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_version_range =
  Test.case "Version range: >= 1.0.0 and < 2.0.0" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("foo", Pubgrub.between (v 1 0 0) (v 2 0 0))];
    Pubgrub.add_package provider "foo" (v 0 9 0) [];
    Pubgrub.add_package provider "foo" (v 1 0 0) [];
    Pubgrub.add_package provider "foo" (v 1 5 0) [];
    Pubgrub.add_package provider "foo" (v 2 0 0) [];
    assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_wide_tree =
  Test.case "Wide dependency tree (5 deps)" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [
      ("a", Pubgrub.full); ("b", Pubgrub.full); ("c", Pubgrub.full); 
      ("d", Pubgrub.full); ("e", Pubgrub.full)
    ];
    Pubgrub.add_package provider "a" (v 1 0 0) [];
    Pubgrub.add_package provider "b" (v 1 0 0) [];
    Pubgrub.add_package provider "c" (v 1 0 0) [];
    Pubgrub.add_package provider "d" (v 1 0 0) [];
    Pubgrub.add_package provider "e" (v 1 0 0) [];
    assert_solution 6 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_nested_diamonds =
  Test.case "Nested diamond dependencies" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("a", Pubgrub.full); ("b", Pubgrub.full)];
    Pubgrub.add_package provider "a" (v 1 0 0) [("c", Pubgrub.full); ("d", Pubgrub.full)];
    Pubgrub.add_package provider "b" (v 1 0 0) [("c", Pubgrub.full); ("d", Pubgrub.full)];
    Pubgrub.add_package provider "c" (v 1 0 0) [("e", Pubgrub.full)];
    Pubgrub.add_package provider "d" (v 1 0 0) [("e", Pubgrub.full)];
    Pubgrub.add_package provider "e" (v 1 0 0) [];
    assert_solution 6 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_complex_graph =
  Test.case "Complex dependency graph" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("a", Pubgrub.full); ("b", Pubgrub.full)];
    Pubgrub.add_package provider "a" (v 1 0 0) [("c", Pubgrub.full); ("d", Pubgrub.full)];
    Pubgrub.add_package provider "b" (v 1 0 0) [("d", Pubgrub.full); ("e", Pubgrub.full)];
    Pubgrub.add_package provider "c" (v 1 0 0) [("f", Pubgrub.full)];
    Pubgrub.add_package provider "d" (v 1 0 0) [("f", Pubgrub.full); ("g", Pubgrub.full)];
    Pubgrub.add_package provider "e" (v 1 0 0) [("g", Pubgrub.full)];
    Pubgrub.add_package provider "f" (v 1 0 0) [];
    Pubgrub.add_package provider "g" (v 1 0 0) [];
    assert_solution 8 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_many_versions =
  Test.case "Package with many versions (10)" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("foo", Pubgrub.full)];
    for i = 0 to 9 do
      Pubgrub.add_package provider "foo" (v 1 i 0) []
    done;
    assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_semantic_versions =
  Test.case "Semantic version ordering" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("foo", Pubgrub.full)];
    Pubgrub.add_package provider "foo" (v 0 1 0) [];
    Pubgrub.add_package provider "foo" (v 0 9 0) [];
    Pubgrub.add_package provider "foo" (v 1 0 0) [];
    Pubgrub.add_package provider "foo" (v 1 0 1) [];
    Pubgrub.add_package provider "foo" (v 1 1 0) [];
    Pubgrub.add_package provider "foo" (v 2 0 0) [];
    assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_transitive_constraints =
  Test.case "Transitive version constraints" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("a", Pubgrub.full)];
    Pubgrub.add_package provider "a" (v 1 0 0) [("b", Pubgrub.higher_than (v 2 0 0))];
    Pubgrub.add_package provider "b" (v 1 0 0) [];
    Pubgrub.add_package provider "b" (v 2 0 0) [];
    Pubgrub.add_package provider "b" (v 3 0 0) [];
    assert_solution 3 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_larger_graph =
  Test.case "Larger dependency graph (15 packages)" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("a", Pubgrub.full); ("b", Pubgrub.full); ("c", Pubgrub.full)];
    Pubgrub.add_package provider "a" (v 1 0 0) [("d", Pubgrub.full); ("e", Pubgrub.full)];
    Pubgrub.add_package provider "b" (v 1 0 0) [("f", Pubgrub.full); ("g", Pubgrub.full)];
    Pubgrub.add_package provider "c" (v 1 0 0) [("h", Pubgrub.full); ("i", Pubgrub.full)];
    Pubgrub.add_package provider "d" (v 1 0 0) [("j", Pubgrub.full)];
    Pubgrub.add_package provider "e" (v 1 0 0) [("k", Pubgrub.full)];
    Pubgrub.add_package provider "f" (v 1 0 0) [("l", Pubgrub.full)];
    Pubgrub.add_package provider "g" (v 1 0 0) [("m", Pubgrub.full)];
    Pubgrub.add_package provider "h" (v 1 0 0) [("n", Pubgrub.full)];
    Pubgrub.add_package provider "i" (v 1 0 0) [];
    Pubgrub.add_package provider "j" (v 1 0 0) [];
    Pubgrub.add_package provider "k" (v 1 0 0) [];
    Pubgrub.add_package provider "l" (v 1 0 0) [];
    Pubgrub.add_package provider "m" (v 1 0 0) [];
    Pubgrub.add_package provider "n" (v 1 0 0) [];
    assert_solution 15 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_version_selection_strategy =
  Test.case "Version selection with many options" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("foo", Pubgrub.full)];
    for major = 0 to 2 do
      for minor = 0 to 3 do
        for patch = 0 to 2 do
          Pubgrub.add_package provider "foo" (v major minor patch) []
        done
      done
    done;
    assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_very_deep_chain =
  Test.case "Very deep dependency chain (10 levels)" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("a", Pubgrub.full)];
    Pubgrub.add_package provider "a" (v 1 0 0) [("b", Pubgrub.full)];
    Pubgrub.add_package provider "b" (v 1 0 0) [("c", Pubgrub.full)];
    Pubgrub.add_package provider "c" (v 1 0 0) [("d", Pubgrub.full)];
    Pubgrub.add_package provider "d" (v 1 0 0) [("e", Pubgrub.full)];
    Pubgrub.add_package provider "e" (v 1 0 0) [("f", Pubgrub.full)];
    Pubgrub.add_package provider "f" (v 1 0 0) [("g", Pubgrub.full)];
    Pubgrub.add_package provider "g" (v 1 0 0) [("h", Pubgrub.full)];
    Pubgrub.add_package provider "h" (v 1 0 0) [("i", Pubgrub.full)];
    Pubgrub.add_package provider "i" (v 1 0 0) [];
    assert_solution 10 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_very_wide_tree =
  Test.case "Very wide dependency tree (10 deps)" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [
      ("a", Pubgrub.full); ("b", Pubgrub.full); ("c", Pubgrub.full);
      ("d", Pubgrub.full); ("e", Pubgrub.full); ("f", Pubgrub.full);
      ("g", Pubgrub.full); ("h", Pubgrub.full); ("i", Pubgrub.full);
      ("j", Pubgrub.full)
    ];
    Pubgrub.add_package provider "a" (v 1 0 0) [];
    Pubgrub.add_package provider "b" (v 1 0 0) [];
    Pubgrub.add_package provider "c" (v 1 0 0) [];
    Pubgrub.add_package provider "d" (v 1 0 0) [];
    Pubgrub.add_package provider "e" (v 1 0 0) [];
    Pubgrub.add_package provider "f" (v 1 0 0) [];
    Pubgrub.add_package provider "g" (v 1 0 0) [];
    Pubgrub.add_package provider "h" (v 1 0 0) [];
    Pubgrub.add_package provider "i" (v 1 0 0) [];
    Pubgrub.add_package provider "j" (v 1 0 0) [];
    assert_solution 11 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_triple_diamond =
  Test.case "Triple diamond pattern" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("a", Pubgrub.full); ("b", Pubgrub.full)];
    Pubgrub.add_package provider "a" (v 1 0 0) [("c", Pubgrub.full); ("d", Pubgrub.full)];
    Pubgrub.add_package provider "b" (v 1 0 0) [("c", Pubgrub.full); ("d", Pubgrub.full)];
    Pubgrub.add_package provider "c" (v 1 0 0) [("e", Pubgrub.full); ("f", Pubgrub.full)];
    Pubgrub.add_package provider "d" (v 1 0 0) [("e", Pubgrub.full); ("f", Pubgrub.full)];
    Pubgrub.add_package provider "e" (v 1 0 0) [("g", Pubgrub.full)];
    Pubgrub.add_package provider "f" (v 1 0 0) [("g", Pubgrub.full)];
    Pubgrub.add_package provider "g" (v 1 0 0) [];
    assert_solution 8 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_many_versions_20 =
  Test.case "Package with 20 versions" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("foo", Pubgrub.full)];
    for i = 0 to 19 do
      Pubgrub.add_package provider "foo" (v 1 i 0) []
    done;
    assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_many_versions_50 =
  Test.case "Package with 50 versions" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("foo", Pubgrub.full)];
    for i = 0 to 49 do
      Pubgrub.add_package provider "foo" (v 1 i 0) []
    done;
    assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_constraint_range_narrow =
  Test.case "Narrow version range constraint" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("foo", Pubgrub.between (v 1 5 0) (v 1 7 0))];
    for i = 0 to 10 do
      Pubgrub.add_package provider "foo" (v 1 i 0) []
    done;
    assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_patch_versions =
  Test.case "Patch version selection" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("foo", Pubgrub.full)];
    for i = 0 to 10 do
      Pubgrub.add_package provider "foo" (v 1 0 i) []
    done;
    assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_branching_graph =
  Test.case "Branching dependency graph" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("a", Pubgrub.full)];
    Pubgrub.add_package provider "a" (v 1 0 0) [("b", Pubgrub.full); ("c", Pubgrub.full); ("d", Pubgrub.full)];
    Pubgrub.add_package provider "b" (v 1 0 0) [("e", Pubgrub.full)];
    Pubgrub.add_package provider "c" (v 1 0 0) [("f", Pubgrub.full)];
    Pubgrub.add_package provider "d" (v 1 0 0) [("g", Pubgrub.full)];
    Pubgrub.add_package provider "e" (v 1 0 0) [];
    Pubgrub.add_package provider "f" (v 1 0 0) [];
    Pubgrub.add_package provider "g" (v 1 0 0) [];
    assert_solution 8 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_deep_and_wide =
  Test.case "Deep and wide combined (20 packages)" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("a", Pubgrub.full); ("b", Pubgrub.full); ("c", Pubgrub.full); ("d", Pubgrub.full)];
    Pubgrub.add_package provider "a" (v 1 0 0) [("e", Pubgrub.full); ("f", Pubgrub.full)];
    Pubgrub.add_package provider "b" (v 1 0 0) [("g", Pubgrub.full); ("h", Pubgrub.full)];
    Pubgrub.add_package provider "c" (v 1 0 0) [("i", Pubgrub.full); ("j", Pubgrub.full)];
    Pubgrub.add_package provider "d" (v 1 0 0) [("k", Pubgrub.full); ("l", Pubgrub.full)];
    Pubgrub.add_package provider "e" (v 1 0 0) [("m", Pubgrub.full)];
    Pubgrub.add_package provider "f" (v 1 0 0) [("n", Pubgrub.full)];
    Pubgrub.add_package provider "g" (v 1 0 0) [("o", Pubgrub.full)];
    Pubgrub.add_package provider "h" (v 1 0 0) [("p", Pubgrub.full)];
    Pubgrub.add_package provider "i" (v 1 0 0) [("q", Pubgrub.full)];
    Pubgrub.add_package provider "j" (v 1 0 0) [("r", Pubgrub.full)];
    Pubgrub.add_package provider "k" (v 1 0 0) [("s", Pubgrub.full)];
    Pubgrub.add_package provider "l" (v 1 0 0) [("t", Pubgrub.full)];
    Pubgrub.add_package provider "m" (v 1 0 0) [];
    Pubgrub.add_package provider "n" (v 1 0 0) [];
    Pubgrub.add_package provider "o" (v 1 0 0) [];
    Pubgrub.add_package provider "p" (v 1 0 0) [];
    Pubgrub.add_package provider "q" (v 1 0 0) [];
    Pubgrub.add_package provider "r" (v 1 0 0) [];
    Pubgrub.add_package provider "s" (v 1 0 0) [];
    Pubgrub.add_package provider "t" (v 1 0 0) [];
    assert_solution 21 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let generate_web_framework_tests () =
  let tests = [] in
  let tests = ref tests in
  for i = 1 to 25 do
    let test = Test.case (format "Web framework scenario %d" i) (fun () ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [
        ("http", Pubgrub.full); ("router", Pubgrub.full); ("middleware", Pubgrub.full)
      ];
      Pubgrub.add_package provider "http" (v (1 + (i mod 3)) 0 0) [("sockets", Pubgrub.full)];
      Pubgrub.add_package provider "router" (v 1 (i mod 5) 0) [("path-parser", Pubgrub.full)];
      Pubgrub.add_package provider "middleware" (v 2 0 0) [("logger", Pubgrub.full)];
      Pubgrub.add_package provider "sockets" (v 1 0 0) [];
      Pubgrub.add_package provider "path-parser" (v 1 0 0) [];
      Pubgrub.add_package provider "logger" (v 1 0 0) [];
      assert_solution 7 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
    ) in
    tests := test :: !tests
  done;
  List.rev !tests

let generate_database_tests () =
  let tests = [] in
  let tests = ref tests in
  for i = 1 to 25 do
    let test = Test.case (format "Database scenario %d" i) (fun () ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [
        ("db-driver", Pubgrub.full); ("migrations", Pubgrub.full); ("orm", Pubgrub.full)
      ];
      Pubgrub.add_package provider "db-driver" (v (1 + (i mod 4)) 0 0) [("connection-pool", Pubgrub.full)];
      Pubgrub.add_package provider "migrations" (v 1 (i mod 6) 0) [("sql-parser", Pubgrub.full)];
      Pubgrub.add_package provider "orm" (v 2 0 0) [("query-builder", Pubgrub.full)];
      Pubgrub.add_package provider "connection-pool" (v 1 0 0) [];
      Pubgrub.add_package provider "sql-parser" (v 1 0 0) [];
      Pubgrub.add_package provider "query-builder" (v 1 0 0) [];
      assert_solution 7 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
    ) in
    tests := test :: !tests
  done;
  List.rev !tests

let generate_compiler_tests () =
  let tests = [] in
  let tests = ref tests in
  for i = 1 to 22 do
    let test = Test.case (format "Compiler toolchain scenario %d" i) (fun () ->
      let provider = Pubgrub.create_offline () in
      Pubgrub.add_package provider "root" (v 1 0 0) [
        ("lexer", Pubgrub.full); ("parser", Pubgrub.full); ("codegen", Pubgrub.full); ("optimizer", Pubgrub.full)
      ];
      Pubgrub.add_package provider "lexer" (v (1 + (i mod 3)) 0 0) [("regex", Pubgrub.full)];
      Pubgrub.add_package provider "parser" (v 1 (i mod 4) 0) [("ast", Pubgrub.full)];
      Pubgrub.add_package provider "codegen" (v 2 0 0) [("llvm-bindings", Pubgrub.full)];
      Pubgrub.add_package provider "optimizer" (v 1 0 0) [("analysis", Pubgrub.full)];
      Pubgrub.add_package provider "regex" (v 1 0 0) [];
      Pubgrub.add_package provider "ast" (v 1 0 0) [];
      Pubgrub.add_package provider "llvm-bindings" (v 1 0 0) [];
      Pubgrub.add_package provider "analysis" (v 1 0 0) [];
      assert_solution 9 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
    ) in
    tests := test :: !tests
  done;
  List.rev !tests

let test_large_graph_30_packages =
  Test.case "Large graph: 30 packages with mixed dependencies" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("a", Pubgrub.full); ("b", Pubgrub.full); ("c", Pubgrub.full)];
    Pubgrub.add_package provider "a" (v 1 0 0) [("d", Pubgrub.full); ("e", Pubgrub.full); ("f", Pubgrub.full)];
    Pubgrub.add_package provider "b" (v 1 0 0) [("g", Pubgrub.full); ("h", Pubgrub.full); ("i", Pubgrub.full)];
    Pubgrub.add_package provider "c" (v 1 0 0) [("j", Pubgrub.full); ("k", Pubgrub.full); ("l", Pubgrub.full)];
    Pubgrub.add_package provider "d" (v 1 0 0) [("m0", Pubgrub.full); ("n0", Pubgrub.full); ("o0", Pubgrub.full)];
    Pubgrub.add_package provider "e" (v 1 0 0) [("m1", Pubgrub.full); ("n1", Pubgrub.full); ("o1", Pubgrub.full)];
    Pubgrub.add_package provider "f" (v 1 0 0) [("m2", Pubgrub.full); ("n2", Pubgrub.full); ("o2", Pubgrub.full)];
    List.iter (fun pkg -> Pubgrub.add_package provider pkg (v 1 0 0) []) 
      ["g"; "h"; "i"; "j"; "k"; "l"; "m0"; "n0"; "o0"; "m1"; "n1"; "o1"; "m2"; "n2"; "o2"];
    assert_solution 22 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_conflict_missing_dependency =
  Test.case "Conflict: missing dependency" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("nonexistent", Pubgrub.full)];
    assert_conflict (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_balanced_tree =
  Test.case "Balanced binary tree of dependencies" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("l1-a", Pubgrub.full); ("l1-b", Pubgrub.full)];
    Pubgrub.add_package provider "l1-a" (v 1 0 0) [("l2-a", Pubgrub.full); ("l2-b", Pubgrub.full)];
    Pubgrub.add_package provider "l1-b" (v 1 0 0) [("l2-c", Pubgrub.full); ("l2-d", Pubgrub.full)];
    Pubgrub.add_package provider "l2-a" (v 1 0 0) [("l3-a", Pubgrub.full); ("l3-b", Pubgrub.full)];
    Pubgrub.add_package provider "l2-b" (v 1 0 0) [("l3-c", Pubgrub.full); ("l3-d", Pubgrub.full)];
    Pubgrub.add_package provider "l2-c" (v 1 0 0) [("l3-e", Pubgrub.full); ("l3-f", Pubgrub.full)];
    Pubgrub.add_package provider "l2-d" (v 1 0 0) [("l3-g", Pubgrub.full); ("l3-h", Pubgrub.full)];
    List.iter (fun pkg -> Pubgrub.add_package provider pkg (v 1 0 0) []) 
      ["l3-a"; "l3-b"; "l3-c"; "l3-d"; "l3-e"; "l3-f"; "l3-g"; "l3-h"];
    assert_solution 15 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_monorepo_structure =
  Test.case "Monorepo: multiple packages with shared deps" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [
      ("pkg-a", Pubgrub.full); ("pkg-b", Pubgrub.full); ("pkg-c", Pubgrub.full)
    ];
    Pubgrub.add_package provider "pkg-a" (v 1 0 0) [("shared-utils", Pubgrub.full); ("dep-a", Pubgrub.full)];
    Pubgrub.add_package provider "pkg-b" (v 1 0 0) [("shared-utils", Pubgrub.full); ("dep-b", Pubgrub.full)];
    Pubgrub.add_package provider "pkg-c" (v 1 0 0) [("shared-utils", Pubgrub.full); ("dep-c", Pubgrub.full)];
    Pubgrub.add_package provider "shared-utils" (v 1 0 0) [("common", Pubgrub.full)];
    Pubgrub.add_package provider "dep-a" (v 1 0 0) [];
    Pubgrub.add_package provider "dep-b" (v 1 0 0) [];
    Pubgrub.add_package provider "dep-c" (v 1 0 0) [];
    Pubgrub.add_package provider "common" (v 1 0 0) [];
    assert_solution 9 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_massive_graph_100_packages =
  Test.case "Massive graph: 100+ packages with complex dependencies" (fun () ->
    let provider = Pubgrub.create_offline () in
    let deps = ref [] in
    for i = 0 to 9 do
      deps := (format "dep-%d" i, Pubgrub.full) :: !deps
    done;
    Pubgrub.add_package provider "root" (v 1 0 0) !deps;
    for i = 0 to 9 do
      let sub_deps = ref [] in
      for j = 0 to 9 do
        sub_deps := (format "sub-%d-%d" i j, Pubgrub.full) :: !sub_deps
      done;
      Pubgrub.add_package provider (format "dep-%d" i) (v 1 0 0) !sub_deps;
      for j = 0 to 9 do
        Pubgrub.add_package provider (format "sub-%d-%d" i j) (v 1 0 0) []
      done
    done;
    assert_solution 111 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_massive_versions =
  Test.case "Massive versions: package with 100 versions" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("lib", Pubgrub.full)];
    for i = 0 to 99 do
      Pubgrub.add_package provider "lib" (v 1 i 0) []
    done;
    assert_solution 2 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_complex_constraint_web =
  Test.case "Complex constraints: realistic web stack" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [
      ("http-server", Pubgrub.between (v 2 0 0) (v 3 0 0));
      ("database", Pubgrub.higher_than (v 1 5 0));
      ("cache", Pubgrub.full);
    ];
    for i = 0 to 5 do
      Pubgrub.add_package provider "http-server" (v 2 i 0) [
        ("router", Pubgrub.between (v 1 0 0) (v 2 0 0));
        ("middleware", Pubgrub.full);
      ];
    done;
    for i = 0 to 10 do
      Pubgrub.add_package provider "database" (v 1 i 0) [("connection-pool", Pubgrub.full)];
    done;
    Pubgrub.add_package provider "cache" (v 1 0 0) [("redis-client", Pubgrub.full)];
    for i = 0 to 3 do
      Pubgrub.add_package provider "router" (v 1 i 0) [("path-parser", Pubgrub.full)];
    done;
    Pubgrub.add_package provider "middleware" (v 1 0 0) [("logger", Pubgrub.full)];
    Pubgrub.add_package provider "connection-pool" (v 1 0 0) [];
    Pubgrub.add_package provider "redis-client" (v 1 0 0) [];
    Pubgrub.add_package provider "path-parser" (v 1 0 0) [];
    Pubgrub.add_package provider "logger" (v 1 0 0) [];
    assert_solution 10 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_deep_shared_dependency =
  Test.case "Deep graph with shared transitive dependencies" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [
      ("frontend", Pubgrub.full); ("backend", Pubgrub.full); ("shared", Pubgrub.full)
    ];
    Pubgrub.add_package provider "frontend" (v 1 0 0) [
      ("ui-lib", Pubgrub.full); ("state", Pubgrub.full); ("shared-utils", Pubgrub.full)
    ];
    Pubgrub.add_package provider "backend" (v 1 0 0) [
      ("api", Pubgrub.full); ("auth", Pubgrub.full); ("shared-utils", Pubgrub.full)
    ];
    Pubgrub.add_package provider "shared" (v 1 0 0) [("shared-utils", Pubgrub.full); ("types", Pubgrub.full)];
    Pubgrub.add_package provider "ui-lib" (v 1 0 0) [("renderer", Pubgrub.full)];
    Pubgrub.add_package provider "state" (v 1 0 0) [("store", Pubgrub.full)];
    Pubgrub.add_package provider "api" (v 1 0 0) [("router", Pubgrub.full)];
    Pubgrub.add_package provider "auth" (v 1 0 0) [("jwt", Pubgrub.full); ("crypto", Pubgrub.full)];
    Pubgrub.add_package provider "shared-utils" (v 1 0 0) [("validation", Pubgrub.full)];
    Pubgrub.add_package provider "types" (v 1 0 0) [];
    Pubgrub.add_package provider "renderer" (v 1 0 0) [];
    Pubgrub.add_package provider "store" (v 1 0 0) [];
    Pubgrub.add_package provider "router" (v 1 0 0) [];
    Pubgrub.add_package provider "jwt" (v 1 0 0) [];
    Pubgrub.add_package provider "crypto" (v 1 0 0) [];
    Pubgrub.add_package provider "validation" (v 1 0 0) [];
    assert_solution 16 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_plugin_system =
  Test.case "Plugin system with core and extensions" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [
      ("core", Pubgrub.full); ("plugin-a", Pubgrub.full); ("plugin-b", Pubgrub.full); ("plugin-c", Pubgrub.full)
    ];
    Pubgrub.add_package provider "core" (v 1 0 0) [("api", Pubgrub.full); ("runtime", Pubgrub.full)];
    Pubgrub.add_package provider "plugin-a" (v 1 0 0) [("core", Pubgrub.full); ("helper-a", Pubgrub.full)];
    Pubgrub.add_package provider "plugin-b" (v 1 0 0) [("core", Pubgrub.full); ("helper-b", Pubgrub.full)];
    Pubgrub.add_package provider "plugin-c" (v 1 0 0) [("core", Pubgrub.full); ("helper-c", Pubgrub.full)];
    Pubgrub.add_package provider "api" (v 1 0 0) [];
    Pubgrub.add_package provider "runtime" (v 1 0 0) [];
    Pubgrub.add_package provider "helper-a" (v 1 0 0) [];
    Pubgrub.add_package provider "helper-b" (v 1 0 0) [];
    Pubgrub.add_package provider "helper-c" (v 1 0 0) [];
    assert_solution 10 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let test_multi_level_constraints =
  Test.case "Multi-level version constraints (4 levels)" (fun () ->
    let provider = Pubgrub.create_offline () in
    Pubgrub.add_package provider "root" (v 1 0 0) [("a", Pubgrub.full)];
    Pubgrub.add_package provider "a" (v 1 0 0) [("b", Pubgrub.higher_than (v 2 0 0))];
    Pubgrub.add_package provider "b" (v 3 0 0) [("c", Pubgrub.between (v 1 0 0) (v 3 0 0))];
    Pubgrub.add_package provider "c" (v 2 0 0) [("d", Pubgrub.strictly_lower_than (v 5 0 0))];
    for i = 0 to 10 do
      Pubgrub.add_package provider "b" (v i 0 0) [("c", Pubgrub.between (v 1 0 0) (v 3 0 0))];
      Pubgrub.add_package provider "c" (v i 0 0) [("d", Pubgrub.strictly_lower_than (v 5 0 0))];
      Pubgrub.add_package provider "d" (v i 0 0) [];
    done;
    assert_solution 5 (Pubgrub.solve (Pubgrub.to_provider provider) "root" (v 1 0 0))
  )

let () =
  let base_tests = [
    test_empty_root;
    test_single_dependency;
    test_two_dependencies;
    test_transitive_chain;
    test_deep_chain;
    test_diamond_dependency;
    test_nested_diamonds;
    test_complex_graph;
    test_multiple_versions_picks_latest;
    test_many_versions;
    test_semantic_versions;
    test_version_constraint_lower;
    test_version_constraint_upper;
    test_version_range;
    test_transitive_constraints;
    test_wide_tree;
    test_larger_graph;
    test_version_selection_strategy;
    test_very_deep_chain;
    test_very_wide_tree;
    test_triple_diamond;
    test_many_versions_20;
    test_many_versions_50;
    test_constraint_range_narrow;
    test_patch_versions;
    test_branching_graph;
    test_deep_and_wide;
    test_large_graph_30_packages;
    test_conflict_missing_dependency;
    test_balanced_tree;
    test_monorepo_structure;
    test_massive_graph_100_packages;
    test_massive_versions;
    test_complex_constraint_web;
    test_deep_shared_dependency;
    test_plugin_system;
    test_multi_level_constraints;
  ] in
  let web_tests = generate_web_framework_tests () in
  let db_tests = generate_database_tests () in
  let compiler_tests = generate_compiler_tests () in
  let all_tests = base_tests @ web_tests @ db_tests @ compiler_tests in
  Test.run all_tests
