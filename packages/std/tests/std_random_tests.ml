open Std

let seeded_rng = fun seed -> Random.Rng.standard ~seed () |> Result.unwrap

let sample_list = fun ~rng distribution ~len -> Random.sample ~rng (Random.Distribution.list ~len distribution) |> Result.unwrap

let test_init_with_same_seed_repeats_default_sequence = fun _ctx ->
  let distribution = Random.Distribution.int 10_000 in
  Random.init ~seed:"abc" () |> Result.unwrap;
  let first = Random.sample (Random.Distribution.list ~len:8 distribution) |> Result.unwrap in
  Random.init ~seed:"abc" () |> Result.unwrap;
  let second = Random.sample (Random.Distribution.list ~len:8 distribution) |> Result.unwrap in
  if first = second then
    Ok ()
  else Error "expected Random.init with the same seed to reproduce the same default sequence"

let test_seeded_standard_rngs_are_deterministic = fun _ctx ->
  let left = sample_list ~rng:(seeded_rng "same-seed") (Random.Distribution.int 10_000) ~len:8 in
  let right = sample_list ~rng:(seeded_rng "same-seed") (Random.Distribution.int 10_000) ~len:8 in
  if left = right then
    Ok ()
  else Error "expected Random.Rng.standard ~seed to be deterministic"

let test_explicit_rng_is_independent_from_default_rng = fun _ctx ->
  let distribution = Random.Distribution.list ~len:6 (Random.Distribution.int 10_000) in
  Random.init ~seed:"default-a" () |> Result.unwrap;
  let first = Random.sample ~rng:(seeded_rng "explicit") distribution |> Result.unwrap in
  Random.init ~seed:"default-b" () |> Result.unwrap;
  let second = Random.sample ~rng:(seeded_rng "explicit") distribution |> Result.unwrap in
  if first = second then
    Ok ()
  else Error "expected explicit RNG sampling to be independent from the default RNG"

let test_bits_are_non_negative = fun _ctx ->
  match Random.bits ~rng:(seeded_rng "bits") () with
  | Ok value when value >= 0 -> Ok ()
  | Ok value -> Error ("expected Random.bits to be non-negative, got " ^ Int.to_string value)
  | Error err -> Error ("expected Random.bits to succeed, got " ^ Random.error_to_string err)

let test_bool_is_deterministic_for_the_same_seed = fun _ctx ->
  let left = sample_list ~rng:(seeded_rng "bool-seed") Random.Distribution.bool ~len:12 in
  let right = sample_list ~rng:(seeded_rng "bool-seed") Random.Distribution.bool ~len:12 in
  if left = right then
    Ok ()
  else Error "expected Random.bool to be deterministic for the same seed"

let test_int_with_bound_one_is_always_zero = fun _ctx ->
  let samples = sample_list ~rng:(seeded_rng "int-bound-1") (Random.Distribution.int 1) ~len:12 in
  if List.all samples ~fn:(Int.equal 0) then
    Ok ()
  else Error "expected Random.int 1 to always return 0"

let test_int_with_positive_bound_stays_in_range = fun _ctx ->
  let bound = 17 in
  let samples = sample_list ~rng:(seeded_rng "int-range") (Random.Distribution.int bound) ~len:32 in
  if List.all samples ~fn:(
    fun value -> value >= 0 && value < bound
  ) then
    Ok ()
  else Error "expected Random.int to stay inside [0, bound)"

let test_int_rejects_non_positive_bounds = fun _ctx ->
  match Random.int ~rng:(seeded_rng "int-error") 0 with
  | Error (Random.InvalidIntBound { bound = 0 }) -> Ok ()
  | _ -> Error "expected Random.int 0 to return InvalidIntBound"

let test_int_range_with_equal_bounds_returns_that_value = fun _ctx ->
  let samples = sample_list ~rng:(seeded_rng "int-range-equal") (Random.Distribution.int_range ~min:4 ~max:4) ~len:12 in
  if List.all samples ~fn:(Int.equal 4) then
    Ok ()
  else Error "expected Random.int_range with equal bounds to return that exact value"

let test_int_range_rejects_invalid_bounds = fun _ctx ->
  match Random.int_range ~rng:(seeded_rng "int-range-error") ~min:5 ~max:3 () with
  | Error (Random.InvalidIntRange { min = 5; max = 3 }) -> Ok ()
  | _ -> Error "expected Random.int_range to reject min > max"

let test_int32_with_bound_one_is_zero = fun _ctx ->
  let samples = sample_list ~rng:(seeded_rng "int32-bound-1") (Random.Distribution.int32 1l) ~len:12 in
  if List.all samples ~fn:(Int32.equal 0l) then
    Ok ()
  else Error "expected Random.int32 1l to always return 0l"

let test_int32_rejects_zero_bound = fun _ctx ->
  match Random.int32 ~rng:(seeded_rng "int32-error") 0l with
  | Error (Random.InvalidInt32Bound { bound }) when Int32.equal bound 0l -> Ok ()
  | _ -> Error "expected Random.int32 0l to return InvalidInt32Bound"

let test_int64_with_bound_one_is_zero = fun _ctx ->
  let samples = sample_list ~rng:(seeded_rng "int64-bound-1") (Random.Distribution.int64 1L) ~len:12 in
  if List.all samples ~fn:(Int64.equal 0L) then
    Ok ()
  else Error "expected Random.int64 1L to always return 0L"

let test_int64_rejects_zero_bound = fun _ctx ->
  match Random.int64 ~rng:(seeded_rng "int64-error") 0L with
  | Error (Random.InvalidInt64Bound { bound }) when Int64.equal bound 0L -> Ok ()
  | _ -> Error "expected Random.int64 0L to return InvalidInt64Bound"

let test_float_with_zero_bound_returns_zero = fun _ctx ->
  let samples = sample_list ~rng:(seeded_rng "float-zero") (Random.Distribution.float 0.0) ~len:12 in
  if List.all samples ~fn:(Float.equal 0.0) then
    Ok ()
  else Error "expected Random.float 0.0 to always return 0.0"

let test_float_with_positive_bound_stays_in_range = fun _ctx ->
  let bound = 3.5 in
  let samples = sample_list ~rng:(seeded_rng "float-range") (Random.Distribution.float bound) ~len:20 in
  if List.all samples ~fn:(
    fun value -> value >= 0.0 && value < bound
  ) then
    Ok ()
  else Error "expected Random.float to stay inside [0.0, bound)"

let test_float_range_with_equal_bounds_returns_that_value = fun _ctx ->
  let samples = sample_list ~rng:(seeded_rng "float-range-equal") (Random.Distribution.float_range ~min:1.25 ~max:1.25) ~len:12 in
  if List.all samples ~fn:(Float.equal 1.25) then
    Ok ()
  else Error "expected Random.float_range with equal bounds to return that exact bound"

let test_float_range_rejects_invalid_bounds = fun _ctx ->
  match Random.float_range ~rng:(seeded_rng "float-range-error") ~min:2.0 ~max:1.0 () with
  | Error (Random.InvalidFloatRange { min; max }) when Float.equal min 2.0 && Float.equal max 1.0 -> Ok ()
  | _ -> Error "expected Random.float_range to reject min > max"

let test_bernoulli_boundary_probabilities_are_constant = fun _ctx ->
  let always_false = sample_list ~rng:(seeded_rng "bernoulli-zero") (Random.Distribution.bernoulli ~p:0.0) ~len:12 in
  let always_true = sample_list ~rng:(seeded_rng "bernoulli-one") (Random.Distribution.bernoulli ~p:1.0) ~len:12 in
  if List.all always_false ~fn:(Bool.equal false) && List.all always_true ~fn:(Bool.equal true) then
    Ok ()
  else Error "expected bernoulli boundary probabilities to return constant values"

let test_bernoulli_rejects_invalid_probabilities = fun _ctx ->
  match Random.sample ~rng:(seeded_rng "bernoulli-error") (Random.Distribution.bernoulli ~p:1.5) with
  | Error (Random.InvalidProbability { probability }) when Float.equal probability 1.5 -> Ok ()
  | _ -> Error "expected bernoulli to reject probabilities outside [0, 1]"

let test_one_of_empty_population_errors = fun _ctx ->
  match Random.one_of ~rng:(seeded_rng "one-of-empty") [] with
  | Error Random.EmptyPopulation -> Ok ()
  | _ -> Error "expected Random.one_of [] to return EmptyPopulation"

let test_one_of_singleton_returns_the_only_element = fun _ctx ->
  let samples = sample_list ~rng:(seeded_rng "one-of-singleton") (Random.Distribution.one_of [ "only" ]) ~len:12 in
  if List.all samples ~fn:(String.equal "only") then
    Ok ()
  else Error "expected Random.one_of on a singleton list to return its only element"

let test_choose_n_zero_returns_empty = fun _ctx ->
  match Random.choose_n ~rng:(seeded_rng "choose-n-zero") [ 1; 2; 3 ] 0 with
  | Ok [] -> Ok ()
  | _ -> Error "expected Random.choose_n count 0 to return []"

let test_choose_n_full_population_returns_each_element_once = fun _ctx ->
  let input =
    [
      1;
      2;
      3;
      4;
    ]
  in
  match Random.choose_n ~rng:(seeded_rng "choose-n-full") input 4 with
  | Ok sample ->
      if List.length sample = 4 && List.sort sample ~compare:Int.compare = List.sort input ~compare:Int.compare then
        Ok ()
      else Error "expected Random.choose_n full-size sample to contain every input exactly once"
  | Error err -> Error ("expected Random.choose_n full-size sample to succeed, got " ^ Random.error_to_string err)

let test_choose_n_rejects_oversized_requests = fun _ctx ->
  match Random.choose_n ~rng:(seeded_rng "choose-n-error") [ 1; 2; 3 ] 4 with
  | Error (Random.InvalidSampleSize { requested = 4; available = 3 }) -> Ok ()
  | _ -> Error "expected Random.choose_n to reject requests larger than the population"

let tests = Test.[
  case "Random.init with the same seed reproduces the default sequence" test_init_with_same_seed_repeats_default_sequence;
  case "Random.Rng.standard with the same seed is deterministic" test_seeded_standard_rngs_are_deterministic;
  case "Random.sample with an explicit RNG is independent from the default RNG" test_explicit_rng_is_independent_from_default_rng;
  case "Random.bits returns a non-negative int" test_bits_are_non_negative;
  case "Random.bool is deterministic for the same seed" test_bool_is_deterministic_for_the_same_seed;
  case "Random.int 1 always returns 0" test_int_with_bound_one_is_always_zero;
  case "Random.int with a positive bound stays in range" test_int_with_positive_bound_stays_in_range;
  case "Random.int rejects non-positive bounds" test_int_rejects_non_positive_bounds;
  case "Random.int_range with equal bounds returns that value" test_int_range_with_equal_bounds_returns_that_value;
  case "Random.int_range rejects invalid bounds" test_int_range_rejects_invalid_bounds;
  case "Random.int32 1l always returns 0l" test_int32_with_bound_one_is_zero;
  case "Random.int32 rejects zero bounds" test_int32_rejects_zero_bound;
  case "Random.int64 1L always returns 0L" test_int64_with_bound_one_is_zero;
  case "Random.int64 rejects zero bounds" test_int64_rejects_zero_bound;
  case "Random.float 0.0 always returns 0.0" test_float_with_zero_bound_returns_zero;
  case "Random.float with a positive bound stays in range" test_float_with_positive_bound_stays_in_range;
  case "Random.float_range with equal bounds returns that value" test_float_range_with_equal_bounds_returns_that_value;
  case "Random.float_range rejects invalid bounds" test_float_range_rejects_invalid_bounds;
  case "Random.Distribution.bernoulli respects boundary probabilities" test_bernoulli_boundary_probabilities_are_constant;
  case "Random.Distribution.bernoulli rejects invalid probabilities" test_bernoulli_rejects_invalid_probabilities;
  case "Random.one_of [] returns EmptyPopulation" test_one_of_empty_population_errors;
  case "Random.one_of on a singleton list returns the only element" test_one_of_singleton_returns_the_only_element;
  case "Random.choose_n count 0 returns an empty list" test_choose_n_zero_returns_empty;
  case "Random.choose_n full-size sample returns every element once" test_choose_n_full_population_returns_each_element_once;
  case "Random.choose_n rejects oversized requests" test_choose_n_rejects_oversized_requests;
]

let main ~args = Test.Cli.main ~name:"Random" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
