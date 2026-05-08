open Std
open Propane

module HashMap = Collections.HashMap
module ConcurrentHashMap = Collections.ConcurrentHashMap
module Iterator = Iter.Iterator
module MutIterator = Iter.MutIterator
module HashSet = Collections.HashSet

type Message.t +=
  | Concurrent_hashmap_go
  | Concurrent_hashmap_worker_done of int

type operation =
  | Insert of int * int
  | Get of int
  | Remove of int
  | HasKey of int
  | Clear
  | Entry of int
  | ComputeUpsertAdd of int * int
  | ComputeRemove of int
  | ComputeAbort of int

type observation = {
  entries: (int * int) list;
  keys: int list;
  values: int list;
  iter_entries: (int * int) list;
  mut_iter_entries: (int * int) list;
  length: int;
  is_empty: bool;
  fold_sum: int;
  for_each_entries: (int * int) list;
}

let examples = 400

let sort_ints = fun values -> List.sort values ~compare:Int.compare

let sort_pairs = fun values ->
  List.sort
    values
    ~compare:(fun (left_key, left_value) (right_key, right_value) ->
      let key_order = Int.compare left_key right_key in
      if key_order = Order.EQ then
        Int.compare left_value right_value
      else
        key_order)

let render_pair = fun (key, value) -> "(" ^ Int.to_string key ^ ", " ^ Int.to_string value ^ ")"

let render_pairs = fun pairs -> "[" ^ String.concat "; " (List.map pairs ~fn:render_pair) ^ "]"

let render_ints = fun values -> "[" ^ String.concat "; " (List.map values ~fn:Int.to_string) ^ "]"

let render_int_option = fun __tmp1 ->
  match __tmp1 with
  | None -> "None"
  | Some value -> "Some(" ^ Int.to_string value ^ ")"

let render_bool = Bool.to_string

let render_entry = fun __tmp1 ->
  match __tmp1 with
  | None -> "Vacant"
  | Some value -> "Occupied(" ^ Int.to_string value ^ ")"

let print_operation = fun __tmp1 ->
  match __tmp1 with
  | Insert (key, value) -> "Insert(" ^ Int.to_string key ^ ", " ^ Int.to_string value ^ ")"
  | Get key -> "Get(" ^ Int.to_string key ^ ")"
  | Remove key -> "Remove(" ^ Int.to_string key ^ ")"
  | HasKey key -> "HasKey(" ^ Int.to_string key ^ ")"
  | Clear -> "Clear"
  | Entry key -> "Entry(" ^ Int.to_string key ^ ")"
  | ComputeUpsertAdd (key, delta) ->
      "ComputeUpsertAdd(" ^ Int.to_string key ^ ", " ^ Int.to_string delta ^ ")"
  | ComputeRemove key -> "ComputeRemove(" ^ Int.to_string key ^ ")"
  | ComputeAbort key -> "ComputeAbort(" ^ Int.to_string key ^ ")"

let render_observation = fun observation ->
  "{ entries = "
  ^ render_pairs observation.entries
  ^ "; keys = "
  ^ render_ints observation.keys
  ^ "; values = "
  ^ render_ints observation.values
  ^ "; iter_entries = "
  ^ render_pairs observation.iter_entries
  ^ "; mut_iter_entries = "
  ^ render_pairs observation.mut_iter_entries
  ^ "; length = "
  ^ Int.to_string observation.length
  ^ "; is_empty = "
  ^ Bool.to_string observation.is_empty
  ^ "; fold_sum = "
  ^ Int.to_string observation.fold_sum
  ^ "; for_each_entries = "
  ^ render_pairs observation.for_each_entries
  ^ " }"

let key_gen = Generator.int_range (-16) 16

let value_gen = Generator.int_range (-128) 128

let pair_gen = Generator.pair key_gen value_gen

let pair_list_arb =
  Arbitrary.make
    ~print:(Printer.list (Printer.pair Printer.int Printer.int))
    (Generator.list_size (Generator.int_range 0 96) pair_gen)

let operation_gen =
  Generator.frequency
    [
      (8, Generator.map (fun (key, value) -> Insert (key, value)) pair_gen);
      (5, Generator.map (fun key -> Get key) key_gen);
      (5, Generator.map (fun key -> Remove key) key_gen);
      (3, Generator.map (fun key -> HasKey key) key_gen);
      (1, Generator.return Clear);
      (3, Generator.map (fun key -> Entry key) key_gen);
      (4, Generator.map (fun (key, delta) -> ComputeUpsertAdd (key, delta)) pair_gen);
      (3, Generator.map (fun key -> ComputeRemove key) key_gen);
      (2, Generator.map (fun key -> ComputeAbort key) key_gen);
    ]

let operations_arb =
  Arbitrary.make
    ~print:(Printer.list print_operation)
    (Generator.list_size (Generator.int_range 0 160) operation_gen)

let assert_property = fun ctx ~examples name property ->
  let config = { Property.default_config with test_count = examples } in
  match Property.check ~config ~on_progress:(Test.Context.emit_progress ctx) property with
  | Property.Success -> Ok ()
  | Property.Failure { counter_example; shrink_steps } ->
      Error (name
      ^ " failed\nCounter-example:\n"
      ^ counter_example
      ^ "\nShrink steps: "
      ^ Int.to_string shrink_steps)
  | Property.Error { exception_ = _; backtrace } ->
      Error (name ^ " raised an unexpected exception\n" ^ backtrace)
  | Property.Assumption_violated -> Error (name ^ " exhausted assumptions")

let hash_entry_to_option = fun __tmp1 ->
  match __tmp1 with
  | HashMap.Occupied value -> Some value
  | HashMap.Vacant -> None

let concurrent_entry_to_option = fun __tmp1 ->
  match __tmp1 with
  | ConcurrentHashMap.Occupied value -> Some value
  | ConcurrentHashMap.Vacant -> None

let observe_hashmap = fun map ->
  let for_each_entries = ref [] in
  HashMap.for_each
    map
    ~fn:(fun key value -> for_each_entries := (key, value) :: !for_each_entries);
  {
    entries = sort_pairs (HashMap.to_list map);
    keys = sort_ints (HashMap.keys map);
    values = sort_ints (HashMap.values map);
    iter_entries = sort_pairs (Iterator.to_list (HashMap.iter map));
    mut_iter_entries = sort_pairs (MutIterator.to_list (HashMap.mut_iter map));
    length = HashMap.length map;
    is_empty = HashMap.is_empty map;
    fold_sum = HashMap.fold_left map ~init:0 ~fn:(fun acc key value -> acc + (key * 31) + value);
    for_each_entries = sort_pairs !for_each_entries;
  }

let observe_concurrent = fun map ->
  let for_each_entries = ref [] in
  ConcurrentHashMap.for_each
    map
    ~fn:(fun key value -> for_each_entries := (key, value) :: !for_each_entries);
  {
    entries = sort_pairs (ConcurrentHashMap.to_list map);
    keys = sort_ints (ConcurrentHashMap.keys map);
    values = sort_ints (ConcurrentHashMap.values map);
    iter_entries = sort_pairs (Iterator.to_list (ConcurrentHashMap.iter map));
    mut_iter_entries = sort_pairs (MutIterator.to_list (ConcurrentHashMap.mut_iter map));
    length = ConcurrentHashMap.length map;
    is_empty = ConcurrentHashMap.is_empty map;
    fold_sum = ConcurrentHashMap.fold_left
      map
      ~init:0
      ~fn:(fun acc key value -> acc + (key * 31) + value);
    for_each_entries = sort_pairs !for_each_entries;
  }

let assert_same_observation = fun step hash concurrent ->
  let expected = observe_hashmap hash in
  let actual = observe_concurrent concurrent in
  if not (expected = actual) then
    Property.fail
      ("step "
      ^ Int.to_string step
      ^ ": expected "
      ^ render_observation expected
      ^ ", got "
      ^ render_observation actual)

let apply_shared_operation = fun step hash concurrent operation ->
  (
    match operation with
    | Insert (key, value) ->
        let expected = HashMap.insert hash ~key ~value in
        let actual = ConcurrentHashMap.insert concurrent ~key ~value in
        if not (expected = actual) then
          Property.fail
            ("step "
            ^ Int.to_string step
            ^ " insert returned "
            ^ render_int_option actual
            ^ ", expected "
            ^ render_int_option expected)
    | Get key ->
        let expected = HashMap.get hash ~key in
        let actual = ConcurrentHashMap.get concurrent ~key in
        if not (expected = actual) then
          Property.fail
            ("step "
            ^ Int.to_string step
            ^ " get returned "
            ^ render_int_option actual
            ^ ", expected "
            ^ render_int_option expected)
    | Remove key ->
        let expected = HashMap.remove hash ~key in
        let actual = ConcurrentHashMap.remove concurrent ~key in
        if not (expected = actual) then
          Property.fail
            ("step "
            ^ Int.to_string step
            ^ " remove returned "
            ^ render_int_option actual
            ^ ", expected "
            ^ render_int_option expected)
    | HasKey key ->
        let expected = HashMap.has_key hash ~key in
        let actual = ConcurrentHashMap.has_key concurrent ~key in
        if not (Bool.equal expected actual) then
          Property.fail
            ("step "
            ^ Int.to_string step
            ^ " has_key returned "
            ^ render_bool actual
            ^ ", expected "
            ^ render_bool expected)
    | Clear ->
        HashMap.clear hash;
        ConcurrentHashMap.clear concurrent
    | Entry key ->
        let expected = hash_entry_to_option (HashMap.entry hash ~key) in
        let actual = concurrent_entry_to_option (ConcurrentHashMap.entry concurrent ~key) in
        if not (expected = actual) then
          Property.fail
            ("step "
            ^ Int.to_string step
            ^ " entry returned "
            ^ render_entry actual
            ^ ", expected "
            ^ render_entry expected)
    | ComputeUpsertAdd (key, delta) ->
        let expected_previous =
          HashMap.compute
            hash
            ~key
            ~fn:(fun value ->
              let next = Option.unwrap_or value ~default:0 + delta in
              HashMap.Insert (next, value))
        in
        let actual_previous =
          ConcurrentHashMap.compute
            concurrent
            ~key
            ~fn:(fun value ->
              let next = Option.unwrap_or value ~default:0 + delta in
              ConcurrentHashMap.Insert (next, value))
        in
        if not (expected_previous = actual_previous) then
          Property.fail
            ("step "
            ^ Int.to_string step
            ^ " compute upsert returned "
            ^ render_int_option actual_previous
            ^ ", expected "
            ^ render_int_option expected_previous)
    | ComputeRemove key ->
        let expected =
          HashMap.compute
            hash
            ~key
            ~fn:(fun value ->
              match value with
              | None -> HashMap.Abort None
              | Some removed -> HashMap.Remove (Some removed))
        in
        let actual =
          ConcurrentHashMap.compute
            concurrent
            ~key
            ~fn:(fun value ->
              match value with
              | None -> ConcurrentHashMap.Abort None
              | Some removed -> ConcurrentHashMap.Remove (Some removed))
        in
        if not (expected = actual) then
          Property.fail
            ("step "
            ^ Int.to_string step
            ^ " compute remove returned "
            ^ render_int_option actual
            ^ ", expected "
            ^ render_int_option expected)
    | ComputeAbort key ->
        let expected = HashMap.compute hash ~key ~fn:(fun value -> HashMap.Abort value) in
        let actual =
          ConcurrentHashMap.compute concurrent ~key ~fn:(fun value -> ConcurrentHashMap.Abort value)
        in
        if not (expected = actual) then
          Property.fail
            ("step "
            ^ Int.to_string step
            ^ " compute abort returned "
            ^ render_int_option actual
            ^ ", expected "
            ^ render_int_option expected)
  );
  assert_same_observation step hash concurrent

let constructors_match_hashmap =
  Property.for_all
    pair_list_arb
    (fun pairs ->
      let expected_from_list = HashMap.from_list pairs in
      let actual_from_list = ConcurrentHashMap.from_list pairs in
      assert_same_observation 0 expected_from_list actual_from_list;
      let expected_empty = HashMap.create () in
      let actual_empty = ConcurrentHashMap.create () in
      assert_same_observation 1 expected_empty actual_empty;
      let expected_sized = HashMap.with_capacity ~size:(List.length pairs) in
      let actual_sized = ConcurrentHashMap.with_capacity ~size:(List.length pairs) in
      assert_same_observation 2 expected_sized actual_sized;
      true)

let shared_operation_sequence_matches_hashmap =
  Property.for_all
    operations_arb
    (fun operations ->
      let hash = HashMap.create () in
      let concurrent = ConcurrentHashMap.create () in
      let rec loop step remaining =
        match remaining with
        | [] -> true
        | operation :: rest ->
            apply_shared_operation step hash concurrent operation;
            loop (step + 1) rest
      in
      loop 1 operations)

let traversal_helpers_match_hashmap =
  Property.for_all
    pair_list_arb
    (fun pairs ->
      let expected = HashMap.from_list pairs in
      let actual = ConcurrentHashMap.from_list pairs in
      assert_same_observation 0 expected actual;
      true)

let clear_roundtrip_matches_hashmap =
  Property.for_all
    pair_list_arb
    (fun pairs ->
      let expected = HashMap.from_list pairs in
      let actual = ConcurrentHashMap.from_list pairs in
      HashMap.clear expected;
      ConcurrentHashMap.clear actual;
      assert_same_observation 0 expected actual;
      true)

let test_length_tracks_mutations = fun _ctx ->
  let ( let* ) = fun value fn -> Result.and_then value ~fn in
  let map = ConcurrentHashMap.create () in
  let expect_length expected =
    let actual = ConcurrentHashMap.length map in
    if Int.equal actual expected then
      Ok ()
    else
      Error ("expected length " ^ Int.to_string expected ^ ", got " ^ Int.to_string actual)
  in
  let* () = expect_length 0 in
  let _ = ConcurrentHashMap.insert map ~key:1 ~value:10 in
  let* () = expect_length 1 in
  let _ = ConcurrentHashMap.insert map ~key:1 ~value:20 in
  let* () = expect_length 1 in
  let _ = ConcurrentHashMap.insert map ~key:2 ~value:30 in
  let* () = expect_length 2 in
  let _ =
    ConcurrentHashMap.compute
      map
      ~key:2
      ~fn:(fun value ->
        match value with
        | None -> ConcurrentHashMap.Abort ()
        | Some value -> ConcurrentHashMap.Insert (value + 1, ()))
  in
  let* () = expect_length 2 in
  let _ =
    ConcurrentHashMap.compute
      map
      ~key:3
      ~fn:(fun value -> ConcurrentHashMap.Insert (Option.unwrap_or value ~default:0 + 1, ()))
  in
  let* () = expect_length 3 in
  let _ =
    ConcurrentHashMap.compute map ~key:1 ~fn:(fun _value -> ConcurrentHashMap.Insert (40, ()))
  in
  let* () = expect_length 3 in
  let _ =
    ConcurrentHashMap.compute
      map
      ~key:2
      ~fn:(fun value ->
        match value with
        | None -> ConcurrentHashMap.Abort ()
        | Some _ -> ConcurrentHashMap.Remove ())
  in
  let* () = expect_length 2 in
  let _ = ConcurrentHashMap.remove map ~key:42 in
  let* () = expect_length 2 in
  ConcurrentHashMap.clear map;
  let* () = expect_length 0 in
  if ConcurrentHashMap.is_empty map then
    Ok ()
  else
    Error "expected cleared map to be empty"

let await = fun ~what selector ->
  try Ok (receive ~selector ~timeout:(Time.Duration.from_secs 2) ()) with
  | Receive_timeout -> Error ("timed out waiting for " ^ what)

let wait_for_go = fun () ->
  let _ =
    receive
      ~selector:(fun __tmp1 ->
        match __tmp1 with
        | Concurrent_hashmap_go -> Select ()
        | _ -> Skip)
      ~timeout:(Time.Duration.from_secs 2)
      ()
  in
  ()

let wait_for_workers = fun ~worker_count ->
  let rec loop remaining =
    if remaining = 0 then
      Ok ()
    else
      match await
        ~what:"concurrent hashmap worker completion"
        (fun __tmp1 ->
          match __tmp1 with
          | Concurrent_hashmap_worker_done _ -> Select ()
          | _ -> Skip) with
      | Error _ as error -> error
      | Ok () -> loop (remaining - 1)
  in
  loop worker_count

let spawn_workers = fun ~worker_count ~fn ->
  let parent = self () in
  let rec loop worker acc =
    if worker >= worker_count then
      List.reverse acc
    else
      let pid =
        spawn
          (fun () ->
            wait_for_go ();
            fn worker;
            send parent (Concurrent_hashmap_worker_done worker);
            Ok ())
      in
      loop (worker + 1) (pid :: acc)
  in
  loop 0 []

let test_concurrent_distinct_key_inserts = fun _ctx ->
  let worker_count = 4 in
  let per_worker = 50 in
  let map = ConcurrentHashMap.with_capacity ~size:(worker_count * per_worker * 2) in
  let worker_pids =
    spawn_workers
      ~worker_count
      ~fn:(fun worker ->
        for index = 0 to per_worker - 1 do
          let key = (worker * 1_000_000) + index in
          let _ = ConcurrentHashMap.insert map ~key ~value:key in
          if index mod 7 = 0 then
            yield ()
        done)
  in
  List.for_each worker_pids ~fn:(fun pid -> send pid Concurrent_hashmap_go);
  match wait_for_workers ~worker_count with
  | Error _ as error -> error
  | Ok () ->
      let expected_count = worker_count * per_worker in
      let all_present =
        let rec loop worker index =
          if worker >= worker_count then
            true
          else if index >= per_worker then
            loop (worker + 1) 0
          else
            let key = (worker * 1_000_000) + index in
            ConcurrentHashMap.get map ~key = Some key && loop worker (index + 1)
        in
        loop 0 0
      in
      if not all_present then
        Error "expected ConcurrentHashMap to preserve every distinct concurrent insert"
      else if not (Int.equal (ConcurrentHashMap.length map) expected_count) then
        Error "expected ConcurrentHashMap concurrent inserts to produce the expected length"
      else
        Ok ()

let test_concurrent_compute_increments = fun _ctx ->
  let worker_count = 4 in
  let iterations = 100 in
  let map = ConcurrentHashMap.create () in
  let _ = ConcurrentHashMap.insert map ~key:"counter" ~value:0 in
  let worker_pids =
    spawn_workers
      ~worker_count
      ~fn:(fun worker ->
        for index = 1 to iterations do
          ConcurrentHashMap.compute
            map
            ~key:"counter"
            ~fn:(fun value ->
              let current = Option.unwrap_or value ~default:0 in
              ConcurrentHashMap.Insert (current + 1, ()));
          if (worker + index) mod 11 = 0 then
            yield ()
        done)
  in
  List.for_each worker_pids ~fn:(fun pid -> send pid Concurrent_hashmap_go);
  match wait_for_workers ~worker_count with
  | Error _ as error -> error
  | Ok () ->
      let expected = worker_count * iterations in
      if ConcurrentHashMap.get map ~key:"counter" = Some expected then
        Ok ()
      else
        Error "expected ConcurrentHashMap.compute to preserve every concurrent increment"

let test_concurrent_removes_claim_each_key_once = fun _ctx ->
  let value_count = 120 in
  let worker_count = 4 in
  let rec make_items index acc =
    if index < 0 then
      acc
    else
      make_items (index - 1) ((index, index) :: acc)
  in
  let map = ConcurrentHashMap.from_list (make_items (value_count - 1) []) in
  let claimed = Sync.Atomic.make [] in
  let worker_pids =
    spawn_workers
      ~worker_count
      ~fn:(fun worker ->
        for key = 0 to value_count - 1 do
          (
            match ConcurrentHashMap.remove map ~key with
            | None -> ()
            | Some value ->
                let rec record () =
                  let current = Sync.Atomic.get claimed in
                  if not (Sync.Atomic.compare_and_set claimed current (value :: current)) then
                    record ()
                in
                record ()
          );
          if (worker + key) mod 13 = 0 then
            yield ()
        done)
  in
  List.for_each worker_pids ~fn:(fun pid -> send pid Concurrent_hashmap_go);
  match wait_for_workers ~worker_count with
  | Error _ as error -> error
  | Ok () ->
      let values = Sync.Atomic.get claimed in
      let unique = HashSet.from_list values in
      if
        Int.equal (List.length values) value_count
        && Int.equal (HashSet.length unique) value_count
        && ConcurrentHashMap.is_empty map
      then
        Ok ()
      else
        Error "expected concurrent removers to claim every key exactly once"

let tests =
  Test.[
    case
      "ConcurrentHashMap constructors and from_list match HashMap"
      (fun ctx ->
        assert_property
          ctx
          ~examples
          "constructors_match_hashmap"
          constructors_match_hashmap);
    case
      "ConcurrentHashMap shared operations match HashMap"
      (fun ctx ->
        assert_property
          ctx
          ~examples
          "shared_operation_sequence_matches_hashmap"
          shared_operation_sequence_matches_hashmap);
    case
      "ConcurrentHashMap traversal helpers match HashMap"
      (fun ctx ->
        assert_property
          ctx
          ~examples
          "traversal_helpers_match_hashmap"
          traversal_helpers_match_hashmap);
    case
      "ConcurrentHashMap clear roundtrip matches HashMap"
      (fun ctx ->
        assert_property
          ctx
          ~examples
          "clear_roundtrip_matches_hashmap"
          clear_roundtrip_matches_hashmap);
    case
      "ConcurrentHashMap length tracks insert replace remove and clear"
      test_length_tracks_mutations;
    case
      ~size:Large
      "ConcurrentHashMap preserves concurrent distinct-key inserts"
      test_concurrent_distinct_key_inserts;
    case
      ~size:Large
      "ConcurrentHashMap.compute preserves concurrent increments"
      test_concurrent_compute_increments;
    case
      ~size:Large
      "ConcurrentHashMap concurrent removers claim each key once"
      test_concurrent_removes_claim_each_key_once;
  ]

let main ~args = Test.Cli.main ~name:"concurrent_hashmap" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
