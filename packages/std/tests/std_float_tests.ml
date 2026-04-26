open Std

let test_from_int_and_to_int = fun _ctx ->
  if Float.equal (Float.from_int 7) 7.0 && Int.equal (Float.to_int 7.9) 7 then
    Ok ()
  else
    Error "expected Float.from_int/to_int to convert numerics"

let test_parse_valid = fun _ctx ->
  match Float.parse "3.5" with
  | Some value when Float.equal value 3.5 -> Ok ()
  | _ -> Error "expected Float.parse 3.5 = Some 3.5"

let test_parse_invalid = fun _ctx ->
  match Float.parse "abc" with
  | None -> Ok ()
  | Some _ -> Error "expected Float.parse abc = None"

let test_to_string_precision = fun _ctx ->
  if String.equal (Float.to_string ~precision:2 3.141_59) "3.14" then
    Ok ()
  else
    Error "expected Float.to_string ~precision:2 3.14159 = 3.14"

let test_is_finite = fun _ctx ->
  if Float.is_finite 3.5 && not (Float.is_finite Float.infinity) then
    Ok ()
  else
    Error "expected Float.is_finite to distinguish finite values"

let test_is_infinite = fun _ctx ->
  if Float.is_infinite Float.infinity && not (Float.is_infinite 3.5) then
    Ok ()
  else
    Error "expected Float.is_infinite to detect infinities"

let test_is_nan = fun _ctx ->
  if Float.is_nan Float.nan && not (Float.is_nan 0.0) then
    Ok ()
  else
    Error "expected Float.is_nan to detect NaN"

let test_rem_and_abs = fun _ctx ->
  if Float.equal (Float.rem 7.5 2.0) 1.5 && Float.equal (Float.abs (-2.5)) 2.5 then
    Ok ()
  else
    Error "expected Float.rem and Float.abs to behave numerically"

let test_sqrt_and_cbrt = fun _ctx ->
  if
    Float.equal (Float.round (Float.sqrt 4.0)) 2.0 && Float.equal (Float.round (Float.cbrt 8.0)) 2.0
  then
    Ok ()
  else
    Error "expected Float.sqrt/cbrt to compute roots"

let test_floor_ceil_round = fun _ctx ->
  if
    Float.equal (Float.floor 3.9) 3.0
    && Float.equal (Float.ceil 3.1) 4.0
    && Float.equal (Float.round 3.5) 4.0
  then
    Ok ()
  else
    Error "expected floor/ceil/round to behave predictably"

let test_pow = fun _ctx ->
  if Float.equal (Float.pow 2.0 5.0) 32.0 then
    Ok ()
  else
    Error "expected Float.pow 2 5 = 32"

let test_operator_syntax = fun _ctx ->
  let open Float in
  if
    Float.((7.5 + 0.5) = 8.0 && (7.5 - 0.5) = 7.0 && (3.0 * 2.0) = 6.0 && (7.5 / 2.5) = 3.0 && 7.5
    > 2.5)
  then
    Ok ()
  else
    Error "expected Float operators to mirror the runtime float operations"

let tests =
  Test.[
    case "Float.from_int and Float.to_int convert numerics" test_from_int_and_to_int;
    case "Float.parse accepts valid decimal strings" test_parse_valid;
    case "Float.parse rejects invalid strings" test_parse_invalid;
    case "Float.to_string respects precision" test_to_string_precision;
    case "Float.is_finite distinguishes finite values" test_is_finite;
    case "Float.is_infinite detects infinities" test_is_infinite;
    case "Float.is_nan detects NaN" test_is_nan;
    case "Float.rem and Float.abs behave numerically" test_rem_and_abs;
    case "Float.sqrt and Float.cbrt compute roots" test_sqrt_and_cbrt;
    case "Float.floor ceil and round produce expected values" test_floor_ceil_round;
    case "Float.pow exponentiates values" test_pow;
    case "Float operators mirror runtime float operations" test_operator_syntax;
  ]

let main ~args = Test.Cli.main ~name:"float" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
