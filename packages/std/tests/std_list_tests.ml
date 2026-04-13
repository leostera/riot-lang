open Std
module Test = Std.Test

let test_list_concat_preserves_input_order = fun _ctx ->
  let actual = List.concat [ [ "a"; "b" ]; [ "c" ]; [ "d"; "e" ] ] in
  let expected = [ "a"; "b"; "c"; "d"; "e" ] in
  if actual = expected then
    Ok ()
  else
    Error ("expected List.concat to preserve input order ["
    ^ String.concat ", " expected
    ^ "] but got ["
    ^ String.concat ", " actual
    ^ "]")

let tests = Test.[
  case "List.concat preserves input order" test_list_concat_preserves_input_order;
]

let name = "Std List Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
