open Std
open Propane

let make_rng = fun seed ->
  Random.Rng.standard ~seed:(Int.to_string seed) ()
  |> Result.expect ~msg:"failed to create deterministic propane bench rng"

let rec list_init = fun count fn ->
  let rec loop index acc =
    if index >= count then
      List.reverse acc
    else
      loop (index + 1) (fn index :: acc)
  in
  loop 0 []

let make_one_of = fun branch_count ->
  list_init branch_count (fun index -> Generator.return index)
  |> Generator.one_of

let make_frequency = fun branch_count ~heavy_head ->
  list_init
    branch_count
    (fun index ->
      let weight =
        if heavy_head && index = 0 then
          99
        else
          1
      in
      (weight, Generator.return index))
  |> Generator.frequency

let bench_one_of = fun branch_count ->
  let rng = make_rng (100 + branch_count) in
  let gen = make_one_of branch_count in
  fun () ->
    let _ = Generator.generate rng gen in
    ()

let bench_frequency = fun branch_count ~heavy_head ->
  let rng =
    make_rng
      (
        200 + branch_count + if heavy_head then
          1
        else
          0
      )
  in
  let gen = make_frequency branch_count ~heavy_head in
  fun () ->
    let _ = Generator.generate rng gen in
    ()

let bench_list_generation = fun size ->
  let rng = make_rng (300 + size) in
  let gen = Generator.list_size (Generator.return size) Generator.int in
  fun () ->
    let _ = Generator.generate_with_size rng size gen in
    ()

let bench_string_generation = fun size ->
  let rng = make_rng (400 + size) in
  let gen = Generator.string_size (Generator.return size) Generator.char_printable in
  fun () ->
    let _ = Generator.generate_with_size rng size gen in
    ()

let bench_string_shrinker = fun len ->
  let value = String.make ~len ~char:'z' in
  fun () ->
    let _ = Shrinker.shrink Shrinker.string value in
    ()

let bench_property_check = fun () ->
  let prop = Property.for_all Arbitrary.int (fun value -> value + 0 = value) in
  let config = { Property.default_config with test_count = 100; max_size = 50; seed = Some 123 } in
  fun () ->
    let _ = Property.check ~config prop in
    ()

let bench_assumption_heavy_property = fun () ->
  let prop =
    Property.for_all
      Arbitrary.int
      (fun value ->
        assume (value mod 10 = 0);
        true)
  in
  let config = { Property.default_config with test_count = 50; max_size = 50; seed = Some 321 } in
  fun () ->
    let _ = Property.check ~config prop in
    ()

let bench_shrinking_failure = fun () ->
  let arb =
    Arbitrary.make
      ~shrink:(Shrinker.list Shrinker.int)
      ~print:(Printer.list Printer.int)
      ~small:List.length
      (Generator.return [ 50; 25; 10 ])
  in
  let prop = Property.for_all arb (fun values -> List.is_empty values) in
  let config =
    { Property.default_config with test_count = 1; max_shrink_steps = 100; seed = Some 1 }
  in
  fun () ->
    let _ = Property.check ~config prop in
    ()

let short_config: Bench.bench_config = { iterations = 200; warmup = 20 }

let medium_config: Bench.bench_config = { iterations = 100; warmup = 10 }

let heavy_config: Bench.bench_config = { iterations = 30; warmup = 5 }

let benchmarks =
  Bench.[
    with_config ~config:medium_config "propane one_of 2 branches" (bench_one_of 2);
    with_config ~config:medium_config "propane one_of 64 branches" (bench_one_of 64);
    with_config
      ~config:medium_config
      "propane frequency 64 uniform"
      (bench_frequency 64 ~heavy_head:false);
    with_config
      ~config:medium_config
      "propane frequency 64 heavy head"
      (bench_frequency 64 ~heavy_head:true);
    with_config ~config:short_config "propane list generation size 32" (bench_list_generation 32);
    with_config ~config:heavy_config "propane list generation size 512" (bench_list_generation 512);
    with_config
      ~config:short_config
      "propane string generation size 32"
      (bench_string_generation 32);
    with_config
      ~config:heavy_config
      "propane string generation size 512"
      (bench_string_generation 512);
    with_config ~config:medium_config "propane string shrinker len 256" (bench_string_shrinker 256);
    with_config ~config:short_config "propane property check passing int" (bench_property_check ());
    with_config
      ~config:short_config
      "propane property check assumption heavy"
      (bench_assumption_heavy_property ());
    with_config
      ~config:heavy_config
      "propane property check shrinking list failure"
      (bench_shrinking_failure ());
  ]

let main ~args = Bench.Cli.main ~name:"propane benchmarks" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
