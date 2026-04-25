open Std

let test_zero_min_and_max_constants = fun _ctx ->
  if not (Int64.equal Int64.zero 0L) then
    Error "Int64.zero should be 0L"
  else if not (Int64.equal Int64.min_int (-9_223_372_036_854_775_808L)) then
    Error "Int64.min_int should match the runtime value"
  else if not (Int64.equal Int64.max_int 9_223_372_036_854_775_807L) then
    Error "Int64.max_int should match the runtime value"
  else
    Ok ()

let test_from_int_to_int_roundtrip = fun _ctx ->
  let value = Int64.from_int 123 in
  if not (Int.equal (Int64.to_int value) 123) then
    Error "Int64.from_int/to_int should roundtrip small values"
  else
    Ok ()

let test_from_int32_to_int32_roundtrip = fun _ctx ->
  let value = Int64.from_int32 123l in
  if not (Int32.equal (Int64.to_int32 value) 123l) then
    Error "Int64.from_int32/to_int32 should preserve 32-bit values"
  else
    Ok ()

let test_logical_operations = fun _ctx ->
  if not (Int64.equal (Int64.lognot 0L) (-1L)) then
    Error "Int64.lognot 0L should be -1L"
  else if not (Int64.equal (Int64.logand 0b1100L 0b1010L) 0b1000L) then
    Error "Int64.logand returned the wrong mask"
  else if not (Int64.equal (Int64.logor 0b1100L 0b0011L) 0b1111L) then
    Error "Int64.logor returned the wrong union"
  else if not (Int64.equal (Int64.logxor 0b1100L 0b1010L) 0b0110L) then
    Error "Int64.logxor returned the wrong xor"
  else
    Ok ()

let test_shift_operations = fun _ctx ->
  if not (Int64.equal (Int64.shift_left 1L 40) 1_099_511_627_776L) then
    Error "Int64.shift_left returned the wrong shifted value"
  else if not (Int64.equal (Int64.shift_right_logical (-1L) 1) 9_223_372_036_854_775_807L) then
    Error "Int64.shift_right_logical should fill the left side with zeros"
  else
    Ok ()

let test_basic_arithmetic = fun _ctx ->
  if not (Int64.equal (Int64.add 7L 3L) 10L) then
    Error "Int64.add returned the wrong sum"
  else if not (Int64.equal (Int64.sub 7L 3L) 4L) then
    Error "Int64.sub returned the wrong difference"
  else if not (Int64.equal (Int64.mul 7L 3L) 21L) then
    Error "Int64.mul returned the wrong product"
  else if not (Int64.equal (Int64.div 7L 3L) 2L) then
    Error "Int64.div should truncate toward zero"
  else if not (Int64.equal (Int64.rem 7L 3L) 1L) then
    Error "Int64.rem returned the wrong remainder"
  else
    Ok ()

let test_operator_syntax = fun _ctx ->
  let open Int64 in
    if
      Int64.((7L + 3L) = 10L
      && (7L - 3L) = 4L
      && (7L * 3L) = 21L
      && (7L / 3L) = 2L
      && (7L mod 3L) = 1L
      && 7L > 3L)
    then
      Ok ()
    else
      Error "Int64 operators should mirror the named helpers"

let test_succ_and_pred = fun _ctx ->
  if not (Int64.equal (Int64.succ 9L) 10L) then
    Error "Int64.succ returned the wrong next value"
  else if not (Int64.equal (Int64.pred 9L) 8L) then
    Error "Int64.pred returned the wrong previous value"
  else
    Ok ()

let test_from_float_truncates_toward_zero = fun _ctx ->
  if not (Int64.equal (Int64.from_float 12.9) 12L) then
    Error "Int64.from_float should truncate positive floats toward zero"
  else if not (Int64.equal (Int64.from_float (-12.9)) (-12L)) then
    Error "Int64.from_float should truncate negative floats toward zero"
  else
    Ok ()

let test_parse_accepts_max_int = fun _ctx ->
  match Int64.parse "9223372036854775807" with
  | Some value when Int64.equal value Int64.max_int -> Ok ()
  | Some _ -> Error "Int64.parse returned the wrong parsed value"
  | None -> Error "Int64.parse rejected max_int text"

let test_parse_rejects_invalid_text = fun _ctx ->
  match Int64.parse "foo" with
  | None -> Ok ()
  | Some _ -> Error "Int64.parse should reject invalid text"

let test_bits_of_float_roundtrip = fun _ctx ->
  let value = 3.141_592_653_589_793 in
  let bits = Int64.bits_of_float value in
  let roundtrip = Int64.float_of_bits bits in
  if Float.equal roundtrip value then
    Ok ()
  else
    Error "Int64.bits_of_float/float_of_bits should roundtrip the value"

let test_hash_is_stable_for_the_same_value = fun _ctx ->
  if Int.equal (Int64.hash 42L) (Int64.hash 42L) then
    Ok ()
  else
    Error "Int64.hash should be stable for the same value"

let tests =
  Test.[
    case "zero min and max constants match runtime values" test_zero_min_and_max_constants;
    case "from_int and to_int roundtrip small values" test_from_int_to_int_roundtrip;
    case "from_int32 and to_int32 roundtrip 32-bit values" test_from_int32_to_int32_roundtrip;
    case "logical operations return expected results" test_logical_operations;
    case "shift operations preserve int64 semantics" test_shift_operations;
    case "basic arithmetic matches int64 semantics" test_basic_arithmetic;
    case "operator syntax mirrors named helpers" test_operator_syntax;
    case "succ and pred move by one" test_succ_and_pred;
    case "from_float truncates toward zero" test_from_float_truncates_toward_zero;
    case "parse accepts max_int text" test_parse_accepts_max_int;
    case "parse rejects invalid text" test_parse_rejects_invalid_text;
    case "bits_of_float and float_of_bits roundtrip" test_bits_of_float_roundtrip;
    case "hash is stable for the same value" test_hash_is_stable_for_the_same_value;
  ]

let main ~args = Test.Cli.main ~name:"int64" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
