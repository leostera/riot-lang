open Std

let test_process_id_is_positive = fun _ctx ->
  if Process.id () > 0l then
    Ok ()
  else
    Error "expected Process.id () to be positive"

let test_process_default_stdio_inherits = fun _ctx ->
  match Process.default_stdio with
  | {
    stdin = Process.Stdin.Inherit;
    stdout = Process.Stdout.Inherit;
    stderr = Process.Stderr.Inherit;
  } -> Ok ()
  | _ -> Error "expected Process.default_stdio to inherit all stdio streams"

let tests =
  Test.[
    case "Process.id returns a positive OS pid" test_process_id_is_positive;
    case "Process.default_stdio inherits stdio" test_process_default_stdio_inherits;
  ]

let main ~args = Test.Cli.main ~name:"Process" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
