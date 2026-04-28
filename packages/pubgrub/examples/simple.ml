open Std
open Pubgrub

let v = fun major minor patch -> make_version ~major ~minor ~patch

let main ~args:_ =
  Log.info "Creating offline dependency provider...";
  let provider = create_offline () in
  add_package
    provider
    "root"
    (v 1 0 0)
    [ ("menu", full); ("icons", full); ];
  add_package
    provider
    "menu"
    (v 1 0 0)
    [ ("dropdown", full); ];
  add_package
    provider
    "dropdown"
    (v 1 0 0)
    [ ("icons", higher_than (v 2 0 0)); ];
  add_package
    provider
    "icons"
    (v 1 0 0)
    [];
  add_package
    provider
    "icons"
    (v 2 0 0)
    [];
  Log.info "Running solver...";
  let () =
    match solve
      (to_provider provider)
      "root"
      (v 1 0 0) with
    | Ok (Solver.Success solution) ->
        Log.info "Solution found:";
        List.for_each
          solution
          ~fn:(fun (pkg, ver) -> Log.info ("  " ^ pkg ^ "@" ^ (version_to_string ver)))
    | Ok (Solver.Failure conflict) -> Log.error ("No solution found:\n" ^ (explain_conflict conflict))
    | Error err -> Log.error ("Error: " ^ err)
  in
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
