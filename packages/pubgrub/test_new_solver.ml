open Std

let v major minor patch = Version.make ~major ~minor ~patch

let test_basic () =
  Log.(set_level Debug);
  
  let provider = Pubgrub.create_offline () in
  Pubgrub.add_package provider "root" (v 1 0 0) [];
  
  match New_solver.solve (Pubgrub.to_provider provider) "root" (v 1 0 0) with
  | Ok (New_solver.Success solution) ->
      println "✓ Basic test passed";
      List.iter (fun (pkg, ver) ->
        println (format "  %s@%s" pkg (Version.to_string ver))
      ) solution
  | Ok (New_solver.Failure _) ->
      println "✗ Unexpected failure"
  | Error err ->
      println (format "✗ Error: %s" err)

let test_compute_pending () =
  println "\n=== Testing compute_pending ===";
  
  (* Create a simple state with root decided and one dependency *)
  let solution = Partial_solution.empty () in
  let solution = Partial_solution.add_decision solution "root" (v 1 0 0) in
  
  let incompats = HashMap.create () in
  let dep_graph = New_solver.DependencyGraph.empty () in
  let dep_graph = New_solver.DependencyGraph.add_dependencies 
    dep_graph "root" (v 1 0 0) [("foo", Ranges.full)] in
  
  let state = {
    New_solver.solution;
    incompatibilities = incompats;
    dependency_graph = dep_graph;
  } in
  
  let pending = New_solver.compute_pending state in
  println (format "Pending list has %d packages" (List.length pending));
  List.iter (fun (pkg, _) ->
    println (format "  - %s" pkg)
  ) pending;
  
  if List.length pending = 1 then
    println "✓ compute_pending test passed"
  else
    println "✗ compute_pending test failed"

let () =
  test_basic ();
  test_compute_pending ()
