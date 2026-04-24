open Std

let test_equal_true_true = fun _ctx ->
  if Bool.equal true true then
    Ok ()
  else
    Error "expected Bool.equal true true"

let test_equal_true_false = fun _ctx ->
  if not (Bool.equal true false) then
    Ok ()
  else
    Error "expected Bool.equal true false to be false"

let test_compare_false_true = fun _ctx ->
  if Bool.compare false true = Order.LT then
    Ok ()
  else
    Error "expected false < true"

let test_compare_true_false = fun _ctx ->
  if Bool.compare true false = Order.GT then
    Ok ()
  else
    Error "expected true > false"

let test_not_true = fun _ctx ->
  if not (Bool.not true) then
    Ok ()
  else
    Error "expected Bool.not true = false"

let test_to_string_false = fun _ctx ->
  if String.equal (Bool.to_string false) "false" then
    Ok ()
  else
    Error "expected Bool.to_string false = false"

let tests =
  Test.[
    case "Bool.equal true true" test_equal_true_true;
    case "Bool.equal true false" test_equal_true_false;
    case "Bool.compare false true" test_compare_false_true;
    case "Bool.compare true false" test_compare_true_false;
    case "Bool.not true" test_not_true;
    case "Bool.to_string false" test_to_string_false;
  ]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name:"bool" ~tests ~args ()) ~args:Env.args ()
