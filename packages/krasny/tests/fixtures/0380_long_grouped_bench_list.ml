open Std

let benchmarks =
  Bench.
    [
      (* Insert benchmarks - build entire map from scratch *)
      case "insert: 100 items" bench_insert_100;
      with_config ~config:{ iterations = 10; warmup = 2 } "insert: 10k items"
        bench_insert_10k;
      with_config ~config:{ iterations = 5; warmup = 1 } "insert: 100k items"
        bench_insert_100k;
      with_config ~config:{ iterations = 3; warmup = 1 } "insert: 1M items"
        bench_insert_1m;
      (* Lookup benchmarks - single lookup from different sized maps *)
      case "get: from 100 items" bench_get_from_100;
      with_config ~config:{ iterations = 50; warmup = 5 } "get: from 10k items"
        bench_get_from_10k;
      with_config ~config:{ iterations = 20; warmup = 2 } "get: from 100k items"
        bench_get_from_100k;
      with_config ~config:{ iterations = 10; warmup = 2 } "get: from 1M items"
        bench_get_from_1m;
      with_config ~config:{ iterations = 20; warmup = 2 }
        "get: missing from 100k" bench_get_missing;
      (* Remove benchmarks - single remove from different sized maps *)
      case "remove: from 100 items" bench_remove_from_100;
      with_config ~config:{ iterations = 50; warmup = 5 }
        "remove: from 10k items" bench_remove_from_10k;
      with_config ~config:{ iterations = 20; warmup = 2 }
        "remove: from 100k items" bench_remove_from_100k;
      (* Iteration benchmarks - iterate over entire map *)
      case "iter: 100 items" bench_iter_100;
      with_config ~config:{ iterations = 10; warmup = 2 } "iter: 10k items"
        bench_iter_10k;
    ]
