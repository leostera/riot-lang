open Std
open Pubgrub

let v major minor patch = make_version ~major ~minor ~patch

let () =
  Log.(set_level Debug);
  let provider = create_offline () in
  add_package provider "root" (v 1 0 0) [ ("nonexistent", full) ];
  
  match solve (to_provider provider) "root" (v 1 0 0) with
  | Ok (Solver.Failure incompat) -> 
      println "✓ Got expected conflict";
      println "Conflict: %s" (Report.explain_conflict incompat)
  | Ok (Solver.Success solution) ->
      println "✗ Expected conflict but found solution with %d packages:" 
        (List.length solution);
      List.iter (fun (pkg, ver) -> 
        println "  %s@%s" pkg (version_to_string ver)) solution
  | Error err -> 
      println "✗ Error: %s" err
