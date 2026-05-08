open Std
open Std.Collections

module ConcurrentHashMap = Collections.ConcurrentHashMap

type op_size = {
  label: string;
  count: int;
  config: Bench.bench_config;
}

let tiny_config: Bench.bench_config = { iterations = 20; warmup = 3 }

let small_config: Bench.bench_config = { iterations = 10; warmup = 2 }

let medium_config: Bench.bench_config = { iterations = 5; warmup = 1 }

let large_config: Bench.bench_config = { iterations = 1; warmup = 0 }

let op_sizes = [
  { label = "100 ops"; count = 100; config = tiny_config };
  { label = "1k ops"; count = 1_000; config = small_config };
  { label = "10k ops"; count = 10_000; config = small_config };
  { label = "100k ops"; count = 100_000; config = medium_config };
  { label = "1M ops"; count = 1_000_000; config = large_config };
  { label = "10M ops"; count = 10_000_000; config = large_config };
  { label = "100M ops"; count = 100_000_000; config = large_config };
]

let growth_sizes = [
  { label = "100 items"; count = 100; config = tiny_config };
  { label = "1k items"; count = 1_000; config = small_config };
  { label = "10k items"; count = 10_000; config = small_config };
  { label = "100k items"; count = 100_000; config = medium_config };
  { label = "1M items"; count = 1_000_000; config = large_config };
]

let key_space_for = fun count -> Int.min count 65_536

let fill_concurrent = fun map count ->
  for index = 0 to count - 1 do
    ignore (ConcurrentHashMap.insert map ~key:index ~value:index)
  done

let bench_chm_grow_to = fun count () ->
  let map = ConcurrentHashMap.with_capacity ~size:count in
  fill_concurrent map count

let bench_chm_insert_ops = fun count () ->
  let key_space = key_space_for count in
  let map = ConcurrentHashMap.with_capacity ~size:key_space in
  for index = 0 to count - 1 do
    let key = index mod key_space in
    ignore (ConcurrentHashMap.insert map ~key ~value:index)
  done

let bench_chm_get_ops = fun count () ->
  let key_space = key_space_for count in
  let map = ConcurrentHashMap.with_capacity ~size:key_space in
  fill_concurrent map key_space;
  for index = 0 to count - 1 do
    ignore (ConcurrentHashMap.get map ~key:(index mod key_space))
  done

let bench_chm_has_key_ops = fun count () ->
  let key_space = key_space_for count in
  let map = ConcurrentHashMap.with_capacity ~size:key_space in
  fill_concurrent map key_space;
  for index = 0 to count - 1 do
    ignore (ConcurrentHashMap.has_key map ~key:(index mod key_space))
  done

let bench_chm_compute_ops = fun count () ->
  let map = ConcurrentHashMap.create () in
  ignore (ConcurrentHashMap.insert map ~key:0 ~value:0);
  for _ = 1 to count do
    ConcurrentHashMap.compute
      map
      ~key:0
      ~fn:(fun value ->
        let current = Option.unwrap_or value ~default:0 in
        ConcurrentHashMap.Insert (current + 1, ()))
  done

let bench_chm_mixed_ops = fun count () ->
  let key_space = key_space_for count in
  let map = ConcurrentHashMap.with_capacity ~size:key_space in
  fill_concurrent map key_space;
  for index = 0 to count - 1 do
    let key = index mod key_space in
    match index mod 5 with
    | 0 -> ignore (ConcurrentHashMap.insert map ~key ~value:index)
    | 1 -> ignore (ConcurrentHashMap.get map ~key)
    | 2 -> ignore (ConcurrentHashMap.has_key map ~key)
    | 3 ->
        ConcurrentHashMap.compute
          map
          ~key
          ~fn:(fun value ->
            let current = Option.unwrap_or value ~default:0 in
            ConcurrentHashMap.Insert (current + 1, ()))
    | _ -> ignore (ConcurrentHashMap.remove map ~key)
  done

let case = fun ~config name fn -> Bench.with_config ~config name fn

let growth_benchmarks_for_size = fun { label; count; config } -> [
  case ~config ("CHM: Grow to " ^ label) (bench_chm_grow_to count);
]

let op_benchmarks_for_size = fun { label; count; config } -> [
  case ~config ("CHM: Insert " ^ label ^ " over bounded keys") (bench_chm_insert_ops count);
  case ~config ("CHM: Get " ^ label ^ " over bounded keys") (bench_chm_get_ops count);
  case ~config ("CHM: Has_key " ^ label ^ " over bounded keys") (bench_chm_has_key_ops count);
  case ~config ("CHM: Compute same key " ^ label) (bench_chm_compute_ops count);
  case ~config ("CHM: Mixed " ^ label ^ " over bounded keys") (bench_chm_mixed_ops count);
]

let benchmarks =
  List.concat (List.map growth_sizes ~fn:growth_benchmarks_for_size)
  @ List.concat (List.map op_sizes ~fn:op_benchmarks_for_size)

let main ~args = Bench.Cli.main ~name:"ConcurrentHashMap Benchmarks" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
