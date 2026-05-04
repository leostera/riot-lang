open Std

let test_zero_and_one = fun _ctx ->
  if Int.equal Int.zero 0 && Int.equal Int.one 1 then
    Ok ()
  else
    Error "expected Int.zero = 0 and Int.one = 1"

let test_add_sub_mul_div_rem = fun _ctx ->
  if
    Int.equal (Int.add 2 3) 5
    && Int.equal (Int.sub 7 2) 5
    && Int.equal (Int.mul 3 4) 12
    && Int.equal (Int.div 12 3) 4
    && Int.equal (Int.rem 13 5) 3
  then
    Ok ()
  else
    Error "expected Int arithmetic wrappers to match runtime arithmetic"

let test_equal_and_compare = fun _ctx ->
  if Int.equal 7 7 && Int.compare 3 5 = Order.LT && Int.compare 5 3 = Order.GT then
    Ok ()
  else
    Error "expected Int.equal and Int.compare to behave consistently"

let test_abs = fun _ctx ->
  if Int.equal (Int.abs (-7)) 7 then
    Ok ()
  else
    Error "expected Int.abs -7 = 7"

let test_min_and_max = fun _ctx ->
  if Int.equal (Int.min 4 9) 4 && Int.equal (Int.max 4 9) 9 then
    Ok ()
  else
    Error "expected Int.min/max to choose smaller/larger values"

let test_succ_and_pred = fun _ctx ->
  if Int.equal (Int.succ 4) 5 && Int.equal (Int.pred 4) 3 then
    Ok ()
  else
    Error "expected Int.succ/pred to adjust values by one"

let test_from_float = fun _ctx ->
  if Int.equal (Int.from_float 4.8) 4 then
    Ok ()
  else
    Error "expected Int.from_float 4.8 = 4"

let test_parse_and_of_string = fun _ctx ->
  if
    Int.equal (Int.from_string "42") 42
    && Int.parse "42" = Some 42
    && Int.from_string_opt "42" = Some 42
  then
    Ok ()
  else
    Error "expected Int string parsers to parse decimal strings"

let test_parse_invalid = fun _ctx ->
  if Int.parse "abc" = None && Int.from_string_opt "abc" = None then
    Ok ()
  else
    Error "expected invalid int strings to return None"

let test_hash = fun _ctx ->
  if Int.equal (Int.hash 12) (Int.hash 12) then
    Ok ()
  else
    Error "expected Int.hash to be stable across repeated calls"

let test_to_string = fun _ctx ->
  if String.equal (Int.to_string 12) "12" then
    Ok ()
  else
    Error "expected Int.to_string 12 = 12"

let tests =
  Test.[
    case "Int.zero and Int.one expose additive identities" test_zero_and_one;
    case "Int arithmetic wrappers match runtime arithmetic" test_add_sub_mul_div_rem;
    case "Int.equal and Int.compare order values" test_equal_and_compare;
    case "Int.abs returns magnitude" test_abs;
    case "Int.min and Int.max choose extremal values" test_min_and_max;
    case "Int.succ and Int.pred shift by one" test_succ_and_pred;
    case "Int.from_float truncates toward zero" test_from_float;
    case "Int string parsing handles decimal numbers" test_parse_and_of_string;
    case "Int string parsing rejects invalid strings" test_parse_invalid;
    case "Int.hash stays stable for ints" test_hash;
    case "Int.to_string renders decimals" test_to_string;
  ]

let main ~args = Test.Cli.main ~name:"int" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
