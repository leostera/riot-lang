open Std
open Pubgrub

let v major minor patch = make_version ~major ~minor ~patch

let test_simple_solve () =
  Log.info "Test: Simple dependency resolution";
  
  let provider = create_offline () in
  
  (* root depends on foo *)
  add_package provider "root" (v 1 0 0) [ ("foo", full) ];
  
  (* foo has two versions *)
  add_package provider "foo" (v 1 0 0) [];
  add_package provider "foo" (v 2 0 0) [];
  
  match solve (to_provider provider) "root" (v 1 0 0) with
  | Ok (Solver.Success solution) ->
      Log.info "✓ Solution found with %d packages" (List.length solution);
      List.iter (fun (pkg, ver) ->
        Log.info "  %s@%s" pkg (version_to_string ver)
      ) solution
  | Ok (Solver.Failure _) ->
      Log.error "✗ Unexpected failure"
  | Error err ->
      Log.error "✗ Error: %s" err

let test_transitive_deps () =
  Log.info "Test: Transitive dependencies";
  
  let provider = create_offline () in
  
  (* root -> foo -> bar *)
  add_package provider "root" (v 1 0 0) [ ("foo", full) ];
  add_package provider "foo" (v 1 0 0) [ ("bar", full) ];
  add_package provider "bar" (v 1 0 0) [];
  
  match solve (to_provider provider) "root" (v 1 0 0) with
  | Ok (Solver.Success solution) ->
      Log.info "✓ Solution found with %d packages" (List.length solution);
      List.iter (fun (pkg, ver) ->
        Log.info "  %s@%s" pkg (version_to_string ver)
      ) solution
  | Ok (Solver.Failure _) ->
      Log.error "✗ Unexpected failure"
  | Error err ->
      Log.error "✗ Error: %s" err

let () =
  test_simple_solve ();
  println "";
  test_transitive_deps ()
