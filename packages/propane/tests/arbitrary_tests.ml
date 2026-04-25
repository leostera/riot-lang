open Std
open Propane

let make_rng = fun seed -> Random.Rng.standard ~seed:(Int.to_string seed) () |> Result.expect ~msg:"failed to create deterministic rng"

let test_make_preserves_supplied_components = fun _ctx ->
  let gen = Generator.return 7 in
  let shrink value = [ value - 1 ] in
  let print value = "value:" ^ Int.to_string value in
  let small value = value in
  let arb = Arbitrary.make ~shrink ~print ~small gen in
  let rng = make_rng 1 in
  let generated = Generator.generate rng arb.gen in
  match arb.shrink, arb.print, arb.small with
  | (Some shrinker, Some printer, Some small_fn) ->
      if generated = 7 && shrinker 7 = [ 6 ] && printer 7 = "value:7" && small_fn 7 = 7 then
        Ok ()
      else Error "Arbitrary.make did not preserve the supplied components"
  | _ -> Error "Arbitrary.make dropped one of the supplied components"

let test_int_wires_generator_shrinker_printer_and_small = fun _ctx ->
  let rng = make_rng 2 in
  let value = Generator.generate rng Arbitrary.int.gen in
  match Arbitrary.int.shrink, Arbitrary.int.print, Arbitrary.int.small with
  | (Some shrinker, Some printer, Some small_fn) ->
      if printer 42 = "42" && List.any (shrinker 100) ~fn:(
        fun candidate -> candidate = 0
      ) && small_fn (-42) = 42 && value >= Int.min_int then
        Ok ()
      else Error "Arbitrary.int is not wired to the expected components"
  | _ -> Error "Arbitrary.int should expose shrink, print, and small"

let test_list_omits_printer_when_element_printer_is_missing = fun _ctx ->
  let elem = Arbitrary.make Generator.int in
  let arb = Arbitrary.list elem in
  match arb.print with
  | None -> Ok ()
  | Some _ -> Error "list printer should be omitted when the element printer is missing"

let test_list_keeps_structural_shrinker_without_element_shrinker = fun _ctx ->
  let elem = Arbitrary.make ~print:Printer.int Generator.int in
  let arb = Arbitrary.list elem in
  match arb.shrink with
  | None -> Error "list arbitrary should still expose a structural shrinker"
  | Some shrinker ->
      if List.any (shrinker [ 1; 2; 3 ]) ~fn:(
        fun candidate -> List.length candidate < 3
      ) then
        Ok ()
      else Error "list arbitrary did not keep structural shrinking"

let test_array_small_is_array_length = fun _ctx ->
  let arb = Arbitrary.array Arbitrary.int in
  match arb.small with
  | None -> Error "array arbitrary should expose a small metric"
  | Some small ->
      let value = Collections.Array.from_list [ 1; 2; 3 ] in
      if small value = 3 then
        Ok ()
      else Error "array small metric should equal array length"

let test_vector_small_is_vector_length = fun _ctx ->
  let arb = Arbitrary.vector Arbitrary.int in
  match arb.small with
  | None -> Error "vector arbitrary should expose a small metric"
  | Some small ->
      let value =
        Collections.Vector.from_list
          [
            1;
            2;
            3;
            4;
          ]
      in
      if small value = 4 then
        Ok ()
      else Error "vector small metric should equal vector length"

let test_hashmap_small_is_entry_count = fun _ctx ->
  let arb = Arbitrary.hashmap Arbitrary.int Arbitrary.string in
  match arb.small with
  | None -> Error "hashmap arbitrary should expose a small metric"
  | Some small ->
      let value =
        Collections.HashMap.from_list
          [
            1, "a";
            2, "b";
          ]
      in
      if small value = 2 then
        Ok ()
      else Error "hashmap small metric should equal entry count"

let test_hashset_small_is_cardinality = fun _ctx ->
  let arb = Arbitrary.hashset Arbitrary.int in
  match arb.small with
  | None -> Error "hashset arbitrary should expose a small metric"
  | Some small ->
      let value = Collections.HashSet.from_list [ 1; 2; 3 ] in
      if small value = 3 then
        Ok ()
      else Error "hashset small metric should equal set cardinality"

let test_queue_small_is_length = fun _ctx ->
  let arb = Arbitrary.queue Arbitrary.int in
  match arb.small with
  | None -> Error "queue arbitrary should expose a small metric"
  | Some small ->
      let value = Collections.Queue.from_list [ 1; 2 ] in
      if small value = 2 then
        Ok ()
      else Error "queue small metric should equal queue length"

let test_deque_small_is_length = fun _ctx ->
  let arb = Arbitrary.deque Arbitrary.int in
  match arb.small with
  | None -> Error "deque arbitrary should expose a small metric"
  | Some small ->
      let value = Collections.Deque.from_list [ 1; 2; 3 ] in
      if small value = 3 then
        Ok ()
      else Error "deque small metric should equal deque length"

let test_heap_small_is_size = fun _ctx ->
  let arb = Arbitrary.heap Arbitrary.int in
  match arb.small with
  | None -> Error "heap arbitrary should expose a small metric"
  | Some small ->
      let value =
        Collections.Heap.from_list
          [
            1;
            2;
            3;
            4;
            5;
          ]
      in
      if small value = 5 then
        Ok ()
      else Error "heap small metric should equal heap size"

let test_pair_requires_both_printers = fun _ctx ->
  let printable = Arbitrary.int in
  let opaque = Arbitrary.make Generator.int in
  let left = Arbitrary.pair printable printable in
  let right = Arbitrary.pair printable opaque in
  match left.print, right.print with
  | (Some _, None) -> Ok ()
  | _ -> Error "pair should expose a printer only when both sides are printable"

let test_pair_requires_both_shrinkers = fun _ctx ->
  let shrinkable = Arbitrary.int in
  let opaque = Arbitrary.make ~print:Printer.int Generator.int in
  let left = Arbitrary.pair shrinkable shrinkable in
  let right = Arbitrary.pair shrinkable opaque in
  match left.shrink, right.shrink with
  | (Some _, None) -> Ok ()
  | _ -> Error "pair should expose a shrinker only when both sides are shrinkable"

let test_option_small_counts_none_and_some = fun _ctx ->
  let arb = Arbitrary.option Arbitrary.int in
  match arb.small with
  | None -> Error "option arbitrary should expose a small metric"
  | Some small ->
      if small None = 0 && small (Some 5) = 6 then
        Ok ()
      else Error "option small metric should treat None as smaller than Some"

let test_result_small_counts_payload_size = fun _ctx ->
  let arb = Arbitrary.result Arbitrary.int Arbitrary.int in
  match arb.small with
  | None -> Error "result arbitrary should expose a small metric"
  | Some small ->
      if small (Ok 5) = 6 && small (Error (-5)) = 6 then
        Ok ()
      else Error "result small metric should include the active branch payload"

let test_map_preserves_inverse_driven_printer_and_small = fun _ctx ->
  let arb = Arbitrary.map Int.to_string Int.parse_unchecked Arbitrary.int in
  match arb.shrink, arb.print, arb.small with
  | (Some shrinker, Some printer, Some small) ->
      if List.contains (shrinker "10") ~value:"0" && printer "12" = "12" && small "-9" = 9 then
        Ok ()
      else Error "Arbitrary.map did not preserve shrink, print, or small via the inverse mapping"
  | _ -> Error "Arbitrary.map should expose shrink, print, and small here"

let test_map_gen_replaces_only_the_generator = fun _ctx ->
  let arb = Arbitrary.map_gen (Generator.return 7) Arbitrary.int in
  let rng = make_rng 3 in
  let value = Generator.generate rng arb.gen in
  match arb.shrink, arb.print, arb.small with
  | (Some shrinker, Some printer, Some small) ->
      if value = 7 && List.contains (shrinker 10) ~value:0 && printer 5 = "5" && small (-4) = 4 then
        Ok ()
      else Error "Arbitrary.map_gen should only replace the generator"
  | _ -> Error "Arbitrary.map_gen should preserve shrink, print, and small"

let tests = Test.[
  case "make preserves supplied components" test_make_preserves_supplied_components;
  case "int wires generator shrinker printer and small" test_int_wires_generator_shrinker_printer_and_small;
  case "list omits printer when element printer is missing" test_list_omits_printer_when_element_printer_is_missing;
  case "list keeps structural shrinker without element shrinker" test_list_keeps_structural_shrinker_without_element_shrinker;
  case "array small is array length" test_array_small_is_array_length;
  case "vector small is vector length" test_vector_small_is_vector_length;
  case "hashmap small is entry count" test_hashmap_small_is_entry_count;
  case "hashset small is cardinality" test_hashset_small_is_cardinality;
  case "queue small is length" test_queue_small_is_length;
  case "deque small is length" test_deque_small_is_length;
  case "heap small is size" test_heap_small_is_size;
  case "pair requires both printers" test_pair_requires_both_printers;
  case "pair requires both shrinkers" test_pair_requires_both_shrinkers;
  case "option small counts none and some" test_option_small_counts_none_and_some;
  case "result small counts payload size" test_result_small_counts_payload_size;
  case "map preserves inverse driven printer and small" test_map_preserves_inverse_driven_printer_and_small;
  case "map_gen replaces only the generator" test_map_gen_replaces_only_the_generator;
]

let main ~args = Test.Cli.main ~name:"propane/arbitrary_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
