open Std

(* === TYPES === *)

type property_result =
  | Success
  | Failure of { counter_example: string; shrink_steps: int }
  | Error of { exception_: exn; backtrace: string }
  | Assumption_violated

type config = {
  test_count: int;
  max_shrink_steps: int;
  max_size: int;
  seed: int option;
  verbose: bool;
}

let default_config = {
  test_count = 100;
  max_shrink_steps = 1_000;
  max_size = 100;
  seed = None;
  verbose = false;
}

(* Internal property representation *)

type 'value property_internal = {
  name: string;
  arbitrary: 'value Arbitrary.t;
  predicate: 'value -> bool;
}

type test_property =
  | Prop: 'value property_internal -> test_property

(* === EXCEPTIONS FOR CONTROL FLOW === *)

exception Assumption_failed

exception Property_failed of string

(* === ASSUMPTIONS === *)

let implies = fun precondition conclusion ->
  if not precondition then
    raise Assumption_failed;
  conclusion

let assume = fun cond ->
  if not cond then
    raise Assumption_failed

let assume_fail = fun () -> raise Assumption_failed

(* === FAILURE REPORTING === *)

let fail = fun msg -> raise (Property_failed msg)

(* === RUNNING PROPERTIES === *)

let get_name = fun (Prop prop) -> prop.name

let order_candidates = fun arb candidates ->
  match arb.Arbitrary.small with
  | None -> candidates
  | Some small ->
      List.sort candidates ~compare:(fun left right -> Int.compare (small left) (small right))

let size_for_test = fun config index ->
  let max_size = Int.max config.max_size 0 in
  if config.test_count <= 1 then
    max_size
  else
    (index * max_size) / (config.test_count - 1)

(* Shrink a counter-example to find minimal failing value *)

let shrink_counter_example = fun ?on_progress ~iteration ~total arb value predicate max_steps ->
  match arb.Arbitrary.shrink with
  | None -> (value, 0)
  | Some shrinker ->
      let rec shrink_loop current steps_taken =
        if steps_taken >= max_steps then
          (current, steps_taken)
        else
          let candidates =
            Shrinker.shrink shrinker current
            |> order_candidates arb
          in
          let rec try_candidates = function
            | [] -> (current, steps_taken)
            | candidate :: rest ->
                (* Test if this smaller value also fails *)
                let fails =
                  try
                    let result = predicate candidate in
                    not result
                  with
                  | Assumption_failed -> false
                  | Property_failed _ -> true
                  | _ -> true
                in
                if fails then
                  let next_step = steps_taken + 1 in
                  Option.for_each
                    on_progress
                    ~fn:(fun on_progress ->
                      on_progress
                        (
                          Test.Context.PropertyShrinkStep {
                            current = iteration;
                            total;
                            step = next_step;
                            max_steps;
                          }
                        ));
                  shrink_loop candidate next_step
                else
                  (* This candidate passes, try next one *)
                  try_candidates rest
          in
          try_candidates candidates
      in
      shrink_loop value 0

let check = fun ?(config = default_config) ?(on_progress = fun _ -> ()) (Prop prop) ->
  let rnd =
    match config.seed with
    | Some seed ->
        Random.Rng.standard ~seed:(Int.to_string seed) ()
        |> Result.expect ~msg:"failed to build propane rng"
    | None ->
        Random.Rng.standard ()
        |> Result.expect ~msg:"failed to build propane rng"
  in
  let arbitrary = prop.arbitrary in
  let predicate = prop.predicate in
  (* Track assumption failures *)
  let assumptions_failed = ref 0 in
  let tests_run = ref 0 in
  let rec test_loop n =
    if n >= config.test_count then
      Success
    else if !assumptions_failed > config.test_count * 10 then
      Assumption_violated
    else
      (* Generate a test value *)
      let size = size_for_test config n in
      let value = Generator.generate_with_size rnd size arbitrary.gen in
      (* Test the property *)
      let test_result =
        try
          let result = predicate value in
          if result then
            Some `Passed
          else
            Some (`Failed value)
        with
        | Assumption_failed ->
            assumptions_failed := !assumptions_failed + 1;
            on_progress
              (
                Test.Context.PropertyAssumptionRejected {
                  current = n + 1;
                  total = config.test_count;
                  size;
                  rejected_count = !assumptions_failed;
                }
              );
            None
        | Property_failed msg -> Some (`Failed_with_msg (value, msg))
        | exn ->
            let backtrace = Exception.raw_backtrace_to_string (Exception.get_raw_backtrace ()) in
            Some (`Exception (value, exn, backtrace))
      in
      match test_result with
      | None ->
          (* Assumption failed, try next test *)
          test_loop n
      | Some `Passed ->
          tests_run := !tests_run + 1;
          on_progress
            (Test.Context.PropertyIterationPassed {
              current = n + 1;
              total = config.test_count;
              size;
            });
          if config.verbose then
            println
              ("Test "
              ^ Int.to_string !tests_run
              ^ "/"
              ^ Int.to_string config.test_count
              ^ " passed (size="
              ^ Int.to_string size
              ^ ")");
          test_loop (n + 1)
      | Some (`Failed value) ->
          (* Property failed! Shrink to find minimal counter-example *)
          on_progress
            (Test.Context.PropertyCounterExampleFound {
              current = n + 1;
              total = config.test_count;
              size;
            });
          let (minimal_value, shrink_steps) =
            shrink_counter_example
              ~on_progress
              ~iteration:(n + 1)
              ~total:config.test_count
              arbitrary
              value
              predicate
              config.max_shrink_steps
          in
          let counter_example =
            match arbitrary.print with
            | Some printer -> printer minimal_value
            | None -> "<no printer available>"
          in
          Failure { counter_example; shrink_steps }
      | Some (`Failed_with_msg (value, msg)) ->
          on_progress
            (Test.Context.PropertyCounterExampleFound {
              current = n + 1;
              total = config.test_count;
              size;
            });
          let (minimal_value, shrink_steps) =
            shrink_counter_example
              ~on_progress
              ~iteration:(n + 1)
              ~total:config.test_count
              arbitrary
              value
              predicate
              config.max_shrink_steps
          in
          let value_str =
            match arbitrary.print with
            | Some printer -> printer minimal_value
            | None -> "<no printer available>"
          in
          let counter_example = value_str ^ "\nMessage: " ^ msg in
          Failure { counter_example; shrink_steps }
      | Some (`Exception (value, exn, backtrace)) ->
          on_progress
            (Test.Context.PropertyCounterExampleFound {
              current = n + 1;
              total = config.test_count;
              size;
            });
          let (minimal_value, shrink_steps) =
            shrink_counter_example
              ~on_progress
              ~iteration:(n + 1)
              ~total:config.test_count
              arbitrary
              value
              (fun v ->
                try
                  let _ = predicate v in
                  true
                with
                | _ -> false)
              config.max_shrink_steps
          in
          let counter_example_str =
            match arbitrary.print with
            | Some printer -> printer minimal_value
            | None -> "<no printer available>"
          in
          Error {
            exception_ = exn;
            backtrace = counter_example_str ^ "\nBacktrace:\n" ^ backtrace;
          }
  in
  test_loop 0

(* === CREATING PROPERTIES === *)

let for_all = fun arbitrary predicate -> Prop { name = "unnamed property"; arbitrary; predicate }

let get_test_count_from_env = fun () ->
  Env.get Env.Int ~var:"PROPANE_TESTS"
  |> Option.unwrap_or ~default:default_config.test_count

let get_seed_from_env = fun () -> Env.get Env.Int ~var:"PROPANE_SEED"

let get_max_size_from_env = fun () ->
  Env.get Env.Int ~var:"PROPANE_MAX_SIZE"
  |> Option.unwrap_or ~default:default_config.max_size

let property = fun name arbitrary predicate ->
  let test_count = get_test_count_from_env () in
  let seed = get_seed_from_env () in
  let max_size = get_max_size_from_env () in
  let config = { default_config with test_count; max_size; seed } in
  let prop = Prop { name; arbitrary; predicate } in
  Test.property
    name
    ~examples:test_count
    (fun ctx ->
      let result = check ~config ~on_progress:(Test.Context.emit_progress ctx) prop in
      match result with
      | Success -> Ok ()
      | Failure { counter_example; shrink_steps } ->
          let msg =
            String.concat
              "\n"
              [
                "Property failed";
                "Counter-example (after " ^ Int.to_string shrink_steps ^ " shrink steps):";
                counter_example;
              ]
          in
          Error msg
      | Error { exception_; backtrace } ->
          let msg =
            String.concat "\n" [ "Exception raised:"; Exception.to_string exception_; backtrace ]
          in
          Error msg
      | Assumption_violated -> Error "Too many test cases violated assumptions (>10x test count)")
