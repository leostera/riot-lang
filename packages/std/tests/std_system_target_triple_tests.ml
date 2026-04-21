open Std

let test_target_triple_roundtrips_the_current_host = fun _ctx ->
  let rendered = System.TargetTriple.to_string System.host_triple in
  match System.TargetTriple.from_string rendered with
  | Ok parsed when System.TargetTriple.equal parsed System.host_triple -> Ok ()
  | Ok _ -> Error "System.TargetTriple.from_string should roundtrip System.host_triple"
  | Error message -> Error ("expected current host triple to parse, got: " ^ message)

let test_target_triple_rejects_incomplete_values = fun _ctx ->
  match System.TargetTriple.from_string "wasm32-wasi" with
  | Ok _ -> Error "System.TargetTriple.from_string should reject incomplete target triples"
  | Error _ -> Ok ()

let test_host_triple_matches_current_target_triple = fun _ctx ->
  if System.TargetTriple.equal System.host_triple System.TargetTriple.current then
    Ok ()
  else
    Error "System.host_triple should match System.TargetTriple.current"

let tests =
  Test.[
    case "System.TargetTriple roundtrips the current host" test_target_triple_roundtrips_the_current_host;
    case "System.TargetTriple rejects incomplete values" test_target_triple_rejects_incomplete_values;
    case "System.host_triple matches System.TargetTriple.current" test_host_triple_matches_current_target_triple;
  ]

let () =
  Runtime.run
    ~main:(fun ~args -> Test.Cli.main ~name:"std_system_target_triple_tests" ~tests ~args ())
    ~args:Env.args
    ()
