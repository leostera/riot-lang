open Std

let test_measure_returns_result = fun _ctx ->
  let (result, _duration) = Timer.measure (fun () -> "measured") in
  if String.equal result "measured" then
    Ok ()
  else
    Error "Timer.measure should return the wrapped function result"

let test_measure_returns_elapsed_duration = fun _ctx ->
  let (_result, duration) =
    Timer.measure
      (fun () ->
        let acc = ref 0 in
        for value = 0 to 10_000 do
          acc := !acc + value
        done;
        !acc)
  in
  match Time.Duration.compare duration Time.Duration.zero with
  | Order.GT -> Ok ()
  | _ -> Error "Timer.measure should return a positive elapsed duration"

let tests =
  Test.[
    case "measure returns the wrapped result" test_measure_returns_result;
    case "measure returns elapsed duration" test_measure_returns_elapsed_duration;
  ]

let main ~args = Test.Cli.main ~name:"timer" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
