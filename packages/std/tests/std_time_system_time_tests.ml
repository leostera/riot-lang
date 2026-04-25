open Std
module Duration = Time.Duration
module SystemTime = Time.SystemTime

let test_epoch_is_zero = fun _ctx ->
  if
    Int.equal (SystemTime.secs SystemTime.epoch) 0
    && Int64.equal (SystemTime.nanos SystemTime.epoch) 0L
    && Int.equal (SystemTime.to_unix_timestamp SystemTime.epoch) 0
  then
    Ok ()
  else
    Error "expected SystemTime.epoch to be zero in every representation"

let test_from_seconds_preserves_fractional_part = fun _ctx ->
  let time = SystemTime.from_seconds 1.5 in
  if
    Int.equal (SystemTime.secs time) 1
    && Float.equal (SystemTime.secs_float time) 1.5
    && Int64.equal (SystemTime.nanos time) 1_500_000_000L
  then
    Ok ()
  else
    Error "expected SystemTime.from_seconds 1.5 to represent exactly 1.5 seconds"

let test_from_nanos_preserves_exact_timestamp = fun _ctx ->
  let time = SystemTime.from_nanos 1_500_000_000L in
  if
    Int.equal (SystemTime.secs time) 1
    && Float.equal (SystemTime.secs_float time) 1.5
    && Int64.equal (SystemTime.nanos time) 1_500_000_000L
  then
    Ok ()
  else
    Error "expected SystemTime.from_nanos 1_500_000_000L to represent exactly 1.5 seconds"

let test_now_is_typically_nondecreasing = fun _ctx ->
  let first = SystemTime.now () in
  sleep (Duration.from_millis 10);
  let second = SystemTime.now () in
  if SystemTime.compare first second != Order.GT then
    Ok ()
  else
    Error "expected SystemTime.now to be nondecreasing in normal operation"

let test_duration_since_same_time_is_zero = fun _ctx ->
  let time = SystemTime.from_nanos 42L in
  if Duration.is_zero (SystemTime.duration_since ~earlier:time time) then
    Ok ()
  else
    Error "expected SystemTime.duration_since on the same timestamp to return zero"

let test_elapsed_is_non_negative = fun _ctx ->
  let start = SystemTime.now () in
  let elapsed = SystemTime.elapsed start in
  if Duration.compare elapsed Duration.zero != Order.LT then
    Ok ()
  else
    Error "expected SystemTime.elapsed to be non-negative"

let test_add_then_duration_since_recovers_duration = fun _ctx ->
  let start = SystemTime.from_seconds 5.25 in
  let delta = Duration.from_millis 750 in
  let finish = SystemTime.add start delta in
  if Duration.equal (SystemTime.duration_since ~earlier:start finish) delta then
    Ok ()
  else
    Error "expected SystemTime.add followed by duration_since to recover the original duration"

let test_sub_is_inverse_of_add_for_representable_durations = fun _ctx ->
  let start = SystemTime.from_seconds 10.5 in
  let delta = Duration.from_millis 250 in
  let shifted =
    SystemTime.sub start delta
    |> fun time ->
      SystemTime.add time delta
  in
  if SystemTime.equal shifted start then
    Ok ()
  else
    Error "expected SystemTime.sub followed by add to recover the original time"

let test_checked_add_returns_some_for_representable_values = fun _ctx ->
  match SystemTime.checked_add SystemTime.epoch (Duration.from_secs 1) with
  | Some time when Int.equal (SystemTime.secs time) 1 -> Ok ()
  | Some _ -> Error "expected SystemTime.checked_add to return the right time"
  | None -> Error "expected SystemTime.checked_add to succeed for a small duration"

let test_checked_sub_underflow_returns_none = fun _ctx ->
  match SystemTime.checked_sub SystemTime.epoch (Duration.from_secs 1) with
  | None -> Ok ()
  | Some _ -> Error "expected SystemTime.checked_sub epoch 1s to return None"

let test_compare_equal_min_and_max_obey_ordering_laws = fun _ctx ->
  let first = SystemTime.from_seconds 1.0 in
  let second = SystemTime.from_seconds 2.0 in
  if
    SystemTime.equal first first
    && SystemTime.compare first second = Order.LT
    && SystemTime.equal (SystemTime.min first second) first
    && SystemTime.equal (SystemTime.max first second) second
  then
    Ok ()
  else
    Error "expected SystemTime.compare/equal/min/max to obey standard ordering laws"

let test_to_unix_timestamp_drops_fractional_seconds = fun _ctx ->
  if Int.equal (SystemTime.to_unix_timestamp (SystemTime.from_seconds 7.9)) 7 then
    Ok ()
  else
    Error "expected SystemTime.to_unix_timestamp to drop fractional seconds"

let test_duration_since_epoch_is_non_negative = fun _ctx ->
  if Duration.compare (SystemTime.duration_since_epoch ()) Duration.zero != Order.LT then
    Ok ()
  else
    Error "expected SystemTime.duration_since_epoch to be non-negative"

let tests =
  Test.[
    case "SystemTime.epoch is zero in every representation" test_epoch_is_zero;
    case "SystemTime.from_seconds preserves fractional seconds" test_from_seconds_preserves_fractional_part;
    case "SystemTime.from_nanos preserves the exact timestamp" test_from_nanos_preserves_exact_timestamp;
    case "SystemTime.now is typically nondecreasing" test_now_is_typically_nondecreasing;
    case "SystemTime.duration_since on the same timestamp returns zero" test_duration_since_same_time_is_zero;
    case "SystemTime.elapsed is non-negative" test_elapsed_is_non_negative;
    case "SystemTime.add then duration_since recovers the duration" test_add_then_duration_since_recovers_duration;
    case "SystemTime.sub is inverse of add for representable durations" test_sub_is_inverse_of_add_for_representable_durations;
    case "SystemTime.checked_add succeeds for representable values" test_checked_add_returns_some_for_representable_values;
    case "SystemTime.checked_sub returns None on underflow" test_checked_sub_underflow_returns_none;
    case "SystemTime.compare/equal/min/max obey ordering laws" test_compare_equal_min_and_max_obey_ordering_laws;
    case "SystemTime.to_unix_timestamp drops fractional seconds" test_to_unix_timestamp_drops_fractional_seconds;
    case "SystemTime.duration_since_epoch is non-negative" test_duration_since_epoch_is_non_negative;
  ]

let main ~args = Test.Cli.main ~name:"Time.SystemTime" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
