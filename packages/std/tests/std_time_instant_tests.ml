open Std
module Duration = Time.Duration
module Instant = Time.Instant

let test_now_is_nondecreasing = fun _ctx ->
  let first = Instant.now () in
  sleep (Duration.from_millis 10);
  let second = Instant.now () in
  if Instant.compare first second != Order.GT then
    Ok ()
  else
    Error "expected Instant.now to be nondecreasing"

let test_duration_since_same_instant_is_zero = fun _ctx ->
  let instant = Instant.now () in
  if Duration.is_zero (Instant.duration_since ~earlier:instant instant) then
    Ok ()
  else
    Error "expected Instant.duration_since on the same instant to return zero"

let test_saturating_duration_since_swapped_returns_zero = fun _ctx ->
  let earlier = Instant.now () in
  let later = Instant.add earlier (Duration.from_millis 5) in
  if Duration.is_zero (Instant.saturating_duration_since ~earlier:later earlier) then
    Ok ()
  else
    Error "expected Instant.saturating_duration_since to clamp swapped instants to zero"

let test_elapsed_is_non_negative = fun _ctx ->
  let start = Instant.now () in
  let elapsed = Instant.elapsed start in
  if Duration.compare elapsed Duration.zero != Order.LT then
    Ok ()
  else
    Error "expected Instant.elapsed to be non-negative"

let test_add_then_duration_since_recovers_duration = fun _ctx ->
  let start = Instant.now () in
  let delta = Duration.from_millis 250 in
  let finish = Instant.add start delta in
  if Duration.equal (Instant.duration_since ~earlier:start finish) delta then
    Ok ()
  else
    Error "expected Instant.add followed by duration_since to recover the original duration"

let test_checked_add_returns_some_for_representable_values = fun _ctx ->
  match Instant.checked_add (Instant.now ()) (Duration.from_secs 1) with
  | Some _ -> Ok ()
  | None -> Error "expected Instant.checked_add to succeed for a small duration"

let test_checked_sub_underflow_returns_none = fun _ctx ->
  match Instant.checked_sub (Instant.now ()) (Duration.from_secs Int.max_int) with
  | None -> Ok ()
  | Some _ -> Error "expected Instant.checked_sub to return None on underflow"

let test_compare_equal_min_and_max_obey_ordering_laws = fun _ctx ->
  let first = Instant.now () in
  let second = Instant.add first (Duration.from_millis 10) in
  if
    Instant.equal first first
    && Instant.compare first second = Order.LT
    && Instant.equal (Instant.min first second) first
    && Instant.equal (Instant.max first second) second
  then
    Ok ()
  else
    Error "expected Instant.compare/equal/min/max to obey standard ordering laws"

let test_duration_since_panics_when_earlier_is_later = fun _ctx ->
  let later = Instant.now () in
  let earlier = Instant.sub later (Duration.from_millis 5) in
  try
    let _ = Instant.duration_since ~earlier:later earlier in
    Error "expected Instant.duration_since to panic when earlier > later"
  with
  | _ -> Ok ()

let tests =
  Test.[
    case "Instant.now is nondecreasing" test_now_is_nondecreasing;
    case "Instant.duration_since on the same instant returns zero" test_duration_since_same_instant_is_zero;
    case "Instant.saturating_duration_since clamps swapped instants to zero" test_saturating_duration_since_swapped_returns_zero;
    case "Instant.elapsed is non-negative" test_elapsed_is_non_negative;
    case "Instant.add then duration_since recovers the duration" test_add_then_duration_since_recovers_duration;
    case "Instant.checked_add succeeds for representable values" test_checked_add_returns_some_for_representable_values;
    case "Instant.checked_sub returns None on underflow" test_checked_sub_underflow_returns_none;
    case "Instant.compare/equal/min/max obey ordering laws" test_compare_equal_min_and_max_obey_ordering_laws;
    case "Instant.duration_since panics when earlier > later" test_duration_since_panics_when_earlier_is_later;
  ]

let () =
  Runtime.run
    ~main:(fun ~args -> Test.Cli.main ~name:"Time.Instant" ~tests ~args ())
    ~args:Env.args
    ()
