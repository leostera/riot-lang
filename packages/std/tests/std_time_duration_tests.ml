open Std
module Duration = Time.Duration

let test_zero_is_zero = fun _ctx ->
  if Duration.is_zero Duration.zero then
    Ok ()
  else
    Error "Duration.zero should report as zero"

let test_from_days_converts_to_seconds = fun _ctx ->
  if Int.equal (Duration.to_secs (Duration.from_days 2)) 172_800 then
    Ok ()
  else
    Error "Duration.from_days should convert days into whole seconds"

let test_from_hours_converts_to_seconds = fun _ctx ->
  if Int.equal (Duration.to_secs (Duration.from_hours 3)) 10_800 then
    Ok ()
  else
    Error "Duration.from_hours should convert hours into whole seconds"

let test_from_mins_converts_to_seconds = fun _ctx ->
  if Int.equal (Duration.to_secs (Duration.from_mins 5)) 300 then
    Ok ()
  else
    Error "Duration.from_mins should convert minutes into whole seconds"

let test_from_millis_preserves_fractional_seconds = fun _ctx ->
  if Float.equal (Duration.to_secs_float (Duration.from_millis 1_500)) 1.5 then
    Ok ()
  else
    Error "Duration.from_millis should preserve fractional seconds"

let test_from_micros_converts_to_microseconds = fun _ctx ->
  if Int.equal (Duration.to_micros (Duration.from_micros 1_234_567)) 1_234_567 then
    Ok ()
  else
    Error "Duration.from_micros should preserve the requested value"

let test_from_nanos_converts_to_nanoseconds = fun _ctx ->
  if Int64.equal (Duration.to_nanos (Duration.from_nanos 1_234_567_890)) 1_234_567_890L then
    Ok ()
  else
    Error "Duration.from_nanos should preserve the requested value"

let test_from_weeks_converts_to_seconds = fun _ctx ->
  if Int.equal (Duration.to_secs (Duration.from_weeks 2)) 1_209_600 then
    Ok ()
  else
    Error "Duration.from_weeks should convert weeks into whole seconds"

let test_from_secs_float_preserves_fractional_part = fun _ctx ->
  if Int.equal (Duration.to_millis (Duration.from_secs_float 2.75)) 2_750 then
    Ok ()
  else
    Error "Duration.from_secs_float should preserve the fractional part"

let test_to_secs_string_uses_default_precision = fun _ctx ->
  if String.equal (Duration.to_secs_string (Duration.from_secs_float 1.234)) "1.23" then
    Ok ()
  else
    Error "Duration.to_secs_string should default to two decimal places"

let test_to_secs_string_respects_requested_precision = fun _ctx ->
  if String.equal (Duration.to_secs_string ~precision:3 (Duration.from_secs_float 1.125)) "1.125" then
    Ok ()
  else
    Error "Duration.to_secs_string should respect the requested precision"

let test_to_secs_string_with_zero_precision_returns_whole_seconds = fun _ctx ->
  if String.equal (Duration.to_secs_string ~precision:0 (Duration.from_secs_float 1.2)) "1" then
    Ok ()
  else
    Error "Duration.to_secs_string with zero precision should omit the decimal part"

let test_subsec_millis_returns_fractional_milliseconds = fun _ctx ->
  if Int.equal (Duration.subsec_millis (Duration.make ~secs:5 ~nanos:123_456_789)) 123 then
    Ok ()
  else
    Error "Duration.subsec_millis should return the millisecond fraction"

let test_subsec_micros_returns_fractional_microseconds = fun _ctx ->
  if Int.equal (Duration.subsec_micros (Duration.make ~secs:5 ~nanos:123_456_789)) 123_456 then
    Ok ()
  else
    Error "Duration.subsec_micros should return the microsecond fraction"

let test_subsec_nanos_returns_fractional_nanoseconds = fun _ctx ->
  if Int.equal (Duration.subsec_nanos (Duration.make ~secs:5 ~nanos:123_456_789)) 123_456_789 then
    Ok ()
  else
    Error "Duration.subsec_nanos should return the nanosecond fraction"

let test_add_normalizes_nanoseconds = fun _ctx ->
  let result = Duration.add
    (Duration.make ~secs:1 ~nanos:900_000_000)
    (Duration.make ~secs:2 ~nanos:200_000_000) in
  if Int.equal (Duration.to_secs result) 4 && Int.equal (Duration.subsec_nanos result) 100_000_000 then
    Ok ()
  else
    Error "Duration.add should normalize nanoseconds into the seconds component"

let test_sub_returns_positive_difference = fun _ctx ->
  let result = Duration.sub
    (Duration.make ~secs:3 ~nanos:200_000_000)
    (Duration.make ~secs:1 ~nanos:500_000_000) in
  if Int.equal (Duration.to_secs result) 1 && Int.equal (Duration.subsec_nanos result) 700_000_000 then
    Ok ()
  else
    Error "Duration.sub should preserve positive differences"

let test_sub_clamps_negative_results_to_zero = fun _ctx ->
  if Duration.is_zero (Duration.sub (Duration.from_secs 1) (Duration.from_secs 5)) then
    Ok ()
  else
    Error "Duration.sub should clamp negative results to zero"

let test_mul_scales_both_seconds_and_nanoseconds = fun _ctx ->
  let result = Duration.mul (Duration.make ~secs:1 ~nanos:500_000_000) 3 in
  if Int.equal (Duration.to_secs result) 4 && Int.equal (Duration.subsec_nanos result) 500_000_000 then
    Ok ()
  else
    Error "Duration.mul should scale both seconds and nanoseconds"

let test_mul_by_zero_returns_zero = fun _ctx ->
  if Duration.is_zero (Duration.mul (Duration.from_secs 5) 0) then
    Ok ()
  else
    Error "Duration.mul by zero should return zero"

let test_div_scales_down_fractional_values = fun _ctx ->
  if Int.equal (Duration.to_millis (Duration.div (Duration.from_millis 1_500) 2)) 750 then
    Ok ()
  else
    Error "Duration.div should divide total nanoseconds"

let test_checked_add_returns_some_for_non_overflowing_inputs = fun _ctx ->
  match Duration.checked_add (Duration.from_secs 5) (Duration.from_millis 500) with
  | Some result when Int.equal (Duration.to_millis result) 5_500 -> Ok ()
  | Some _ -> Error "Duration.checked_add returned the wrong result"
  | None -> Error "Duration.checked_add should succeed for non-overflowing inputs"

let test_checked_mul_rejects_negative_factors = fun _ctx ->
  match Duration.checked_mul (Duration.from_secs 5) (-1) with
  | None -> Ok ()
  | Some _ -> Error "Duration.checked_mul should reject negative factors"

let test_checked_div_rejects_zero = fun _ctx ->
  match Duration.checked_div (Duration.from_secs 5) 0 with
  | None -> Ok ()
  | Some _ -> Error "Duration.checked_div should reject zero divisors"

let test_saturating_sub_clamps_underflow_to_zero = fun _ctx ->
  if Duration.is_zero (Duration.saturating_sub (Duration.from_secs 1) (Duration.from_secs 5)) then
    Ok ()
  else
    Error "Duration.saturating_sub should clamp underflow to zero"

let test_mul_f64_scales_fractionally = fun _ctx ->
  if Int.equal (Duration.to_millis (Duration.mul_f64 (Duration.from_secs 2) 1.25)) 2_500 then
    Ok ()
  else
    Error "Duration.mul_f64 should scale durations by floating-point factors"

let test_div_f64_scales_fractionally = fun _ctx ->
  if Int.equal (Duration.to_millis (Duration.div_f64 (Duration.from_secs 3) 2.0)) 1_500 then
    Ok ()
  else
    Error "Duration.div_f64 should divide durations by floating-point divisors"

let test_abs_diff_is_symmetric = fun _ctx ->
  let left = Duration.abs_diff (Duration.from_secs 5) (Duration.from_secs 2) in
  let right = Duration.abs_diff (Duration.from_secs 2) (Duration.from_secs 5) in
  if Duration.equal left right && Int.equal (Duration.to_secs left) 3 then
    Ok ()
  else
    Error "Duration.abs_diff should return the same result in either order"

let test_min_returns_the_smaller_duration = fun _ctx ->
  if
    Duration.equal
      (Duration.min (Duration.from_secs 2) (Duration.from_secs 5))
      (Duration.from_secs 2)
  then
    Ok ()
  else
    Error "Duration.min should return the smaller duration"

let test_max_returns_the_larger_duration = fun _ctx ->
  if
    Duration.equal
      (Duration.max (Duration.from_secs 2) (Duration.from_secs 5))
      (Duration.from_secs 5)
  then
    Ok ()
  else
    Error "Duration.max should return the larger duration"

let test_compare_and_equal_are_consistent = fun _ctx ->
  let left = Duration.from_millis 500 in
  let right = Duration.from_micros 500_000 in
  if Duration.equal left right && Duration.compare left right = Order.EQ then
    Ok ()
  else
    Error "Duration.compare and Duration.equal should agree on equivalent values"

let tests =
  Test.[
    case "zero is zero" test_zero_is_zero;
    case "from_days converts to seconds" test_from_days_converts_to_seconds;
    case "from_hours converts to seconds" test_from_hours_converts_to_seconds;
    case "from_mins converts to seconds" test_from_mins_converts_to_seconds;
    case "from_millis preserves fractional seconds" test_from_millis_preserves_fractional_seconds;
    case "from_micros preserves microseconds" test_from_micros_converts_to_microseconds;
    case "from_nanos preserves nanoseconds" test_from_nanos_converts_to_nanoseconds;
    case "from_weeks converts to seconds" test_from_weeks_converts_to_seconds;
    case "from_secs_float preserves fractional parts" test_from_secs_float_preserves_fractional_part;
    case "to_secs_string uses default precision" test_to_secs_string_uses_default_precision;
    case "to_secs_string respects explicit precision" test_to_secs_string_respects_requested_precision;
    case "to_secs_string supports zero precision" test_to_secs_string_with_zero_precision_returns_whole_seconds;
    case "subsec_millis returns fractional milliseconds" test_subsec_millis_returns_fractional_milliseconds;
    case "subsec_micros returns fractional microseconds" test_subsec_micros_returns_fractional_microseconds;
    case "subsec_nanos returns fractional nanoseconds" test_subsec_nanos_returns_fractional_nanoseconds;
    case "add normalizes nanoseconds" test_add_normalizes_nanoseconds;
    case "sub returns positive differences" test_sub_returns_positive_difference;
    case "sub clamps negative results to zero" test_sub_clamps_negative_results_to_zero;
    case "mul scales both seconds and nanoseconds" test_mul_scales_both_seconds_and_nanoseconds;
    case "mul by zero returns zero" test_mul_by_zero_returns_zero;
    case "div scales down fractional values" test_div_scales_down_fractional_values;
    case "checked_add succeeds on safe inputs" test_checked_add_returns_some_for_non_overflowing_inputs;
    case "checked_mul rejects negative factors" test_checked_mul_rejects_negative_factors;
    case "checked_div rejects zero" test_checked_div_rejects_zero;
    case "saturating_sub clamps underflow to zero" test_saturating_sub_clamps_underflow_to_zero;
    case "mul_f64 scales fractionally" test_mul_f64_scales_fractionally;
    case "div_f64 scales fractionally" test_div_f64_scales_fractionally;
    case "abs_diff is symmetric" test_abs_diff_is_symmetric;
    case "min returns the smaller duration" test_min_returns_the_smaller_duration;
    case "max returns the larger duration" test_max_returns_the_larger_duration;
    case "compare and equal are consistent" test_compare_and_equal_are_consistent;
  ]

let main ~args = Test.Cli.main ~name:"time_duration" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
