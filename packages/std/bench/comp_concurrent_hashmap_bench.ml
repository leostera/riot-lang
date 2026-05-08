open Std
open Std.Collections

module ConcurrentHashMap = Collections.ConcurrentHashMap

type Message.t +=
  | Concurrent_hashmap_bench_go
  | Concurrent_hashmap_bench_done of int

type ('key, 'value) locked_hashmap = {
  lock: Sync.Mutex.t;
  map: ('key, 'value) HashMap.t;
}

type op_size = {
  label: string;
  count: int;
  config: Bench.bench_config;
}

let small_config: Bench.bench_config = { iterations = 10; warmup = 2 }

let worker_count = Int.max 1 Thread.available_parallelism

let actor_label =
  if Int.equal worker_count 1 then
    "1 actor"
  else
    Int.to_string worker_count ^ " actors"

let op_sizes = [
  { label = "1k ops"; count = 1_000; config = small_config };
  { label = "10k ops"; count = 10_000; config = small_config };
]

let make_locked_hashmap = fun ~size -> {
  lock = Sync.Mutex.create ();
  map = HashMap.with_capacity ~size;
}

let with_locked = fun locked fn ->
  Sync.Mutex.lock locked.lock;
  let result = fn locked.map in
  Sync.Mutex.unlock locked.lock;
  result

let locked_insert = fun locked ~key ~value ->
  with_locked
    locked
    (fun map ->
      HashMap.insert map ~key ~value)

let locked_get = fun locked ~key -> with_locked locked (fun map -> HashMap.get map ~key)

let locked_remove = fun locked ~key -> with_locked locked (fun map -> HashMap.remove map ~key)

let locked_has_key = fun locked ~key -> with_locked locked (fun map -> HashMap.has_key map ~key)

let locked_compute_upsert_add = fun locked ~key ~delta ->
  with_locked
    locked
    (fun map ->
      HashMap.compute
        map
        ~key
        ~fn:(fun value ->
          let next = Option.unwrap_or value ~default:0 + delta in
          HashMap.Insert (next, value)))

let wait_for_go = fun () ->
  let _ =
    receive
      ~selector:(fun __tmp1 ->
        match __tmp1 with
        | Concurrent_hashmap_bench_go -> Select ()
        | _ -> Skip)
      ()
  in
  ()

let wait_for_workers = fun ~worker_count ->
  let rec loop remaining =
    if remaining = 0 then
      ()
    else
      (
        let _ =
          receive
            ~selector:(fun __tmp1 ->
              match __tmp1 with
              | Concurrent_hashmap_bench_done _ -> Select ()
              | _ -> Skip)
            ()
        in
        loop (remaining - 1)
      )
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
            send parent (Concurrent_hashmap_bench_done worker);
            Ok ())
      in
      loop (worker + 1) (pid :: acc)
  in
  loop 0 []

let run_workers = fun ~worker_count ~fn ->
  let worker_pids = spawn_workers ~worker_count ~fn in
  List.for_each worker_pids ~fn:(fun pid -> send pid Concurrent_hashmap_bench_go);
  wait_for_workers ~worker_count

let worker_start = fun count worker -> (count * worker) / worker_count

let worker_stop = fun count worker -> (count * (worker + 1)) / worker_count

let iter_worker_range = fun count worker ~fn ->
  for index = worker_start count worker to worker_stop count worker - 1 do
    fn index
  done

let worker_key = fun worker index -> (worker * 100_000_000) + index

let fill_concurrent_distinct = fun map ~count ->
  for worker = 0 to worker_count - 1 do
    iter_worker_range
      count
      worker
      ~fn:(fun index ->
        let key = worker_key worker index in
        ignore (ConcurrentHashMap.insert map ~key ~value:key))
  done

let fill_locked_distinct = fun map ~count ->
  for worker = 0 to worker_count - 1 do
    iter_worker_range
      count
      worker
      ~fn:(fun index ->
        let key = worker_key worker index in
        ignore (locked_insert map ~key ~value:key))
  done

let bench_concurrent_parallel_distinct_inserts = fun count () ->
  let map = ConcurrentHashMap.with_capacity ~size:(count * 2) in
  run_workers
    ~worker_count
    ~fn:(fun worker ->
      iter_worker_range
        count
        worker
        ~fn:(fun index ->
          let key = worker_key worker index in
          ignore (ConcurrentHashMap.insert map ~key ~value:key)))

let bench_locked_parallel_distinct_inserts = fun count () ->
  let map = make_locked_hashmap ~size:(count * 2) in
  run_workers
    ~worker_count
    ~fn:(fun worker ->
      iter_worker_range
        count
        worker
        ~fn:(fun index ->
          let key = worker_key worker index in
          ignore (locked_insert map ~key ~value:key)))

let bench_concurrent_parallel_get = fun count () ->
  let map = ConcurrentHashMap.with_capacity ~size:(count * 2) in
  fill_concurrent_distinct map ~count;
  run_workers
    ~worker_count
    ~fn:(fun worker ->
      iter_worker_range
        count
        worker
        ~fn:(fun index -> ignore (ConcurrentHashMap.get map ~key:(worker_key worker index))))

let bench_locked_parallel_get = fun count () ->
  let map = make_locked_hashmap ~size:(count * 2) in
  fill_locked_distinct map ~count;
  run_workers
    ~worker_count
    ~fn:(fun worker ->
      iter_worker_range
        count
        worker
        ~fn:(fun index -> ignore (locked_get map ~key:(worker_key worker index))))

let bench_concurrent_parallel_has_key = fun count () ->
  let map = ConcurrentHashMap.with_capacity ~size:(count * 2) in
  fill_concurrent_distinct map ~count;
  run_workers
    ~worker_count
    ~fn:(fun worker ->
      iter_worker_range
        count
        worker
        ~fn:(fun index -> ignore (ConcurrentHashMap.has_key map ~key:(worker_key worker index))))

let bench_locked_parallel_has_key = fun count () ->
  let map = make_locked_hashmap ~size:(count * 2) in
  fill_locked_distinct map ~count;
  run_workers
    ~worker_count
    ~fn:(fun worker ->
      iter_worker_range
        count
        worker
        ~fn:(fun index -> ignore (locked_has_key map ~key:(worker_key worker index))))

let bench_concurrent_parallel_remove = fun count () ->
  let map = ConcurrentHashMap.with_capacity ~size:(count * 2) in
  fill_concurrent_distinct map ~count;
  run_workers
    ~worker_count
    ~fn:(fun worker ->
      iter_worker_range
        count
        worker
        ~fn:(fun index -> ignore (ConcurrentHashMap.remove map ~key:(worker_key worker index))))

let bench_locked_parallel_remove = fun count () ->
  let map = make_locked_hashmap ~size:(count * 2) in
  fill_locked_distinct map ~count;
  run_workers
    ~worker_count
    ~fn:(fun worker ->
      iter_worker_range
        count
        worker
        ~fn:(fun index -> ignore (locked_remove map ~key:(worker_key worker index))))

let bench_concurrent_parallel_same_key_compute = fun count () ->
  let map = ConcurrentHashMap.create () in
  ignore (ConcurrentHashMap.insert map ~key:0 ~value:0);
  run_workers
    ~worker_count
    ~fn:(fun _worker ->
      iter_worker_range
        count
        _worker
        ~fn:(fun _index ->
          ConcurrentHashMap.compute
            map
            ~key:0
            ~fn:(fun value ->
              let current = Option.unwrap_or value ~default:0 in
              ConcurrentHashMap.Insert (current + 1, ()))))

let bench_locked_parallel_same_key_compute = fun count () ->
  let map = make_locked_hashmap ~size:1 in
  ignore (locked_insert map ~key:0 ~value:0);
  run_workers
    ~worker_count
    ~fn:(fun _worker ->
      iter_worker_range
        count
        _worker
        ~fn:(fun _index -> ignore (locked_compute_upsert_add map ~key:0 ~delta:1)))

let compare_concurrent = fun ~config workload locked_hashmap concurrent_hashmap ->
  Bench.compare_with_config
    ~config
    ("Concurrent HashMap+Mutex vs ConcurrentHashMap: " ^ workload)
    [
      Bench.make_case_with_config ~config ("HM+M: " ^ workload) locked_hashmap;
      Bench.make_case_with_config ~config ("CHM: " ^ workload) concurrent_hashmap;
    ]

let benchmarks_for_size = fun { label; count; config } -> [
  compare_concurrent
    ~config
    ("Insert " ^ label ^ " across " ^ actor_label)
    (bench_locked_parallel_distinct_inserts count)
    (bench_concurrent_parallel_distinct_inserts count);
  compare_concurrent
    ~config
    ("Get " ^ label ^ " across " ^ actor_label)
    (bench_locked_parallel_get count)
    (bench_concurrent_parallel_get count);
  compare_concurrent
    ~config
    ("Check " ^ label ^ " with has_key across " ^ actor_label)
    (bench_locked_parallel_has_key count)
    (bench_concurrent_parallel_has_key count);
  compare_concurrent
    ~config
    ("Remove " ^ label ^ " across " ^ actor_label)
    (bench_locked_parallel_remove count)
    (bench_concurrent_parallel_remove count);
  compare_concurrent
    ~config
    ("Compute same key " ^ label ^ " across " ^ actor_label)
    (bench_locked_parallel_same_key_compute count)
    (bench_concurrent_parallel_same_key_compute count);
]

let benchmarks = List.concat (List.map op_sizes ~fn:benchmarks_for_size)

let main ~args = Bench.Cli.main ~name:"Comparative Concurrent HashMap Benchmarks" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
