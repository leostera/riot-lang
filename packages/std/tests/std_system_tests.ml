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

let test_os_predicates_match_current_os = fun _ctx ->
  match System.OS.current with
  | System.OS.Unix ->
      if System.unix && not System.win32 && not System.cygwin then
        Ok ()
      else
        Error "System unix flags should agree with System.OS.current"
  | System.OS.Win32 ->
      if System.win32 && not System.unix && not System.cygwin then
        Ok ()
      else
        Error "System win32 flags should agree with System.OS.current"
  | System.OS.Cygwin ->
      if System.cygwin && not System.win32 && not System.unix then
        Ok ()
      else
        Error "System cygwin flags should agree with System.OS.current"

let tests =
  Test.[
    case "System.TargetTriple roundtrips the current host" test_target_triple_roundtrips_the_current_host;
    case "System.TargetTriple rejects incomplete values" test_target_triple_rejects_incomplete_values;
    case "System.host_triple matches System.TargetTriple.current" test_host_triple_matches_current_target_triple;
    case "System OS predicates agree with the current OS" test_os_predicates_match_current_os;
  ]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name:"system" ~tests ~args) ~args:Env.args ()
