open Std

let test_zero_min_and_max_constants = fun _ctx ->
  if not (Int32.equal Int32.zero 0l) then
    Error "Int32.zero should be 0l"
  else if not (Int32.equal Int32.min_int (-2_147_483_648l)) then
    Error "Int32.min_int should match the runtime value"
  else if not (Int32.equal Int32.max_int 2_147_483_647l) then
    Error "Int32.max_int should match the runtime value"
  else
    Ok ()

let test_from_int_to_int_roundtrip = fun _ctx ->
  let value = Int32.from_int 123 in
  if not (Int.equal (Int32.to_int value) 123) then
    Error "Int32.from_int/to_int should roundtrip small values"
  else
    Ok ()

let test_neg_and_abs = fun _ctx ->
  if not (Int32.equal (Int32.neg 5l) (-5l)) then
    Error "Int32.neg should negate the value"
  else if not (Int32.equal (Int32.abs (-5l)) 5l) then
    Error "Int32.abs should remove the sign"
  else
    Ok ()

let test_basic_arithmetic = fun _ctx ->
  if not (Int32.equal (Int32.add 7l 3l) 10l) then
    Error "Int32.add returned the wrong sum"
  else if not (Int32.equal (Int32.sub 7l 3l) 4l) then
    Error "Int32.sub returned the wrong difference"
  else if not (Int32.equal (Int32.mul 7l 3l) 21l) then
    Error "Int32.mul returned the wrong product"
  else if not (Int32.equal (Int32.div 7l 3l) 2l) then
    Error "Int32.div should truncate toward zero"
  else if not (Int32.equal (Int32.rem 7l 3l) 1l) then
    Error "Int32.rem returned the wrong remainder"
  else
    Ok ()

let test_operator_syntax = fun _ctx ->
  let open Int32 in
    if
      Int32.((7l + 3l) = 10l
      && (7l - 3l) = 4l
      && (7l * 3l) = 21l
      && (7l / 3l) = 2l
      && (7l mod 3l) = 1l
      && 7l > 3l)
    then
      Ok ()
    else
      Error "Int32 operators should mirror the named helpers"

let test_bitwise_operations = fun _ctx ->
  if not (Int32.equal (Int32.logand 0b1100l 0b1010l) 0b1000l) then
    Error "Int32.logand returned the wrong mask"
  else if not (Int32.equal (Int32.logor 0b1100l 0b0011l) 0b1111l) then
    Error "Int32.logor returned the wrong union"
  else if not (Int32.equal (Int32.logxor 0b1100l 0b1010l) 0b0110l) then
    Error "Int32.logxor returned the wrong xor"
  else
    Ok ()

let test_shift_left = fun _ctx ->
  if not (Int32.equal (Int32.shift_left 1l 3) 8l) then
    Error "Int32.shift_left should shift by the requested number of bits"
  else
    Ok ()

let test_shift_right_logical = fun _ctx ->
  if not (Int32.equal (Int32.shift_right_logical (-1l) 1) 2_147_483_647l) then
    Error "Int32.shift_right_logical should fill the left side with zeros"
  else
    Ok ()

let test_from_float_truncates_toward_zero = fun _ctx ->
  if not (Int32.equal (Int32.from_float 12.9) 12l) then
    Error "Int32.from_float should truncate positive floats toward zero"
  else if not (Int32.equal (Int32.from_float (-12.9)) (-12l)) then
    Error "Int32.from_float should truncate negative floats toward zero"
  else
    Ok ()

let test_bits_of_float_roundtrip = fun _ctx ->
  let value = 1.5 in
  let bits = Int32.bits_of_float value in
  let roundtrip = Int32.float_of_bits bits in
  if Float.equal roundtrip value then
    Ok ()
  else
    Error "Int32.bits_of_float/float_of_bits should roundtrip the value"

let test_parse_accepts_decimal_text = fun _ctx ->
  match Int32.parse "-42" with
  | Some value when Int32.equal value (-42l) -> Ok ()
  | Some _ -> Error "Int32.parse returned the wrong parsed value"
  | None -> Error "Int32.parse rejected valid decimal text"

let test_parse_rejects_invalid_text = fun _ctx ->
  match Int32.parse "not-an-int32" with
  | None -> Ok ()
  | Some _ -> Error "Int32.parse should reject invalid text"

let test_to_string_renders_signed_decimal = fun _ctx ->
  if String.equal (Int32.to_string (-123l)) "-123" then
    Ok ()
  else
    Error "Int32.to_string should render signed decimal text"

let tests =
  Test.[
    case "zero min and max constants match runtime values" test_zero_min_and_max_constants;
    case "from_int and to_int roundtrip small values" test_from_int_to_int_roundtrip;
    case "neg and abs handle signs" test_neg_and_abs;
    case "basic arithmetic matches int32 semantics" test_basic_arithmetic;
    case "operator syntax mirrors named helpers" test_operator_syntax;
    case "bitwise operations return expected results" test_bitwise_operations;
    case "shift_left moves bits left" test_shift_left;
    case "shift_right_logical zero-fills the left edge" test_shift_right_logical;
    case "from_float truncates toward zero" test_from_float_truncates_toward_zero;
    case "bits_of_float and float_of_bits roundtrip" test_bits_of_float_roundtrip;
    case "parse accepts decimal text" test_parse_accepts_decimal_text;
    case "parse rejects invalid text" test_parse_rejects_invalid_text;
    case "to_string renders signed decimal" test_to_string_renders_signed_decimal;
  ]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name:"int32" ~tests ~args ()) ~args:Env.args ()
