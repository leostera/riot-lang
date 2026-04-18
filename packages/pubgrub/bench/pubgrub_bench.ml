open Std
open Pubgrub

let v = fun major minor patch -> make_version ~major ~minor ~patch

let build_provider = fun versions_per_package ->
  let offline = create_offline () in
  for patch = 0 to versions_per_package - 1 do
    add_package offline "pkg" (v 1 0 patch) []
  done;
  to_provider offline

let build_partial_solution = fun derivation_count ->
  let incompat = Incompatibility.create_external
    [ Term.negative "pkg" (between (v 1 0 0) (v 1 0 derivation_count)) ]
    (Incompatibility.Custom ("pkg", full, "bench")) in
  let rec loop solution remaining =
    if remaining <= 0 then
      solution
    else
      loop (Partial_solution.add_derivation solution "pkg" incompat) (remaining - 1)
  in
  loop (Partial_solution.empty ()) derivation_count

let build_chain_provider = fun depth ->
  let offline = create_offline () in
  let rec loop idx =
    if idx >= depth then
      add_package offline ("pkg-" ^ Int.to_string idx) (v 1 0 0) []
    else (
      add_package
        offline
        ("pkg-" ^ Int.to_string idx)
        (v 1 0 0)
        [ ("pkg-" ^ Int.to_string (idx + 1), full) ];
      loop (idx + 1)
    )
  in
  loop 0;
  to_provider offline

let singleton_range = singleton (v 1 0 5)

let holey_range =
  Ranges.union
    ~compare_v:version_compare
    (between (v 1 0 0) (v 1 0 5))
    (between (v 1 0 10) (v 1 0 15))

let bench_ranges_contains_singleton = fun () ->
  let _ = Ranges.contains ~compare_v:version_compare singleton_range (v 1 0 5) in
  ()

let bench_ranges_contains_holey = fun () ->
  let _ = Ranges.contains ~compare_v:version_compare holey_range (v 1 0 12) in
  ()

let provider_100 = build_provider 100

let provider_1000 = build_provider 1000

let bench_provider_choose_version_100 = fun () ->
  let _ = provider_100.choose_version "pkg" full in
  ()

let bench_provider_choose_version_1000 = fun () ->
  let _ = provider_1000.choose_version "pkg" full in
  ()

let partial_solution_100 = build_partial_solution 100

let bench_partial_solution_constraint_100 = fun () ->
  let _ = Partial_solution.get_constraint partial_solution_100 "pkg" in
  ()

let chain_provider_25 = build_chain_provider 25

let bench_solve_deep_chain = fun () ->
  let _ = solve chain_provider_25 "pkg-0" (v 1 0 0) in
  ()

let medium: Bench.bench_config = { iterations = 200; warmup = 20 }

let heavy: Bench.bench_config = { iterations = 50; warmup = 10 }

let benchmarks =
  Bench.[
    with_config ~config:medium "pubgrub ranges.contains singleton" bench_ranges_contains_singleton;
    with_config ~config:medium "pubgrub ranges.contains holey" bench_ranges_contains_holey;
    with_config ~config:medium "pubgrub provider.choose_version 100" bench_provider_choose_version_100;
    with_config ~config:heavy "pubgrub provider.choose_version 1000" bench_provider_choose_version_1000;
    with_config
      ~config:medium
      "pubgrub partial_solution.get_constraint 100 derivations"
      bench_partial_solution_constraint_100;
    with_config ~config:heavy "pubgrub solve deep chain" bench_solve_deep_chain;
  ]

let () =
  Actors.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"pubgrub benchmarks" ~benchmarks ~args)
    ~args:Env.args
    ()
