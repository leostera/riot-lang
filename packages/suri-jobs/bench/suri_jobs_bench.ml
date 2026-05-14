open Std
open Result.Syntax

module M = Suri_jobs
module Ser = Serde.Ser
module De = Serde.De

type payload = {
  repo_key: string;
  since: string option;
}

type payload_field =
  | Field_repo_key
  | Field_since

type payload_builder = {
  mutable repo_key: string option;
  mutable since: string option option;
}

let payload_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "repo_key" Ser.string (fun (payload: payload) -> payload.repo_key);
          Ser.field "since" (Ser.option Ser.string) (fun (payload: payload) -> payload.since);
        ]
    )

let payload_decode =
  De.record_mut
    ~fields:(De.fields [ De.field "repo_key" Field_repo_key; De.field "since" Field_since ])
    ~create:(fun () -> { repo_key = None; since = None })
    ~step:(fun reader builder field ->
      match field with
      | Some Field_repo_key -> builder.repo_key <- Some (De.read reader De.string)
      | Some Field_since -> builder.since <- Some (De.read reader (De.option De.string))
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match builder.repo_key with
      | None -> De.missing_field ()
      | Some repo_key ->
          let since =
            match builder.since with
            | Some value -> value
            | None -> None
          in
          ({ repo_key; since }: payload))

let queue: (payload, unit) M.Queue.t =
  M.Queue.make
    ~id:(M.QueueId.from_string_unchecked "suri-jobs.bench.payload")
    ~worker:(M.WorkerId.from_string_unchecked "SuriJobsBenchPayload")
    ~encode:payload_encode
    ~decode:payload_decode
    ()

let payload ?since repo_key = ({ repo_key; since }: payload)

let sink = ref 0

let keep value =
  sink := !sink + value

let expect_ok result =
  match result with
  | Ok value -> value
  | Error error -> raise (Failure (M.Error.to_string error))

let rec build_requests acc index total =
  if index >= total then
    List.reverse acc
  else
    let request =
      expect_ok
        (M.Job.enqueue
          ~id:(M.JobId.from_string_unchecked ("bench-job-" ^ Int.to_string index))
          ~priority:(index mod 16)
          queue
          (payload ~since:"2026-05-01T00:00:00Z" ("owner/repo-" ^ Int.to_string index)))
    in
    build_requests (request :: acc) (index + 1) total

let request_batch = build_requests [] 0 1_000

let bench_payload_json_roundtrip = fun () ->
  let sample = payload ~since:"2026-05-01T00:00:00Z" "leostera/hypekit.dev" in
  for index = 1 to 20_000 do
    let encoded = expect_ok (M.Queue.encode_args queue sample) in
    let decoded =
      expect_ok
        (M.Queue.decode_args
          queue
          ~job_id:(M.JobId.from_string_unchecked ("bench-json-" ^ Int.to_string index))
          encoded)
    in
    keep (String.length decoded.repo_key)
  done

let bench_memory_enqueue_many = fun () ->
  let db = M.Memory.create () in
  let stored = expect_ok (M.Memory.enqueue_many db request_batch) in
  keep (List.length stored)

let bench_memory_fetch_complete = fun () ->
  let db = M.Memory.create () in
  ignore (expect_ok (M.Memory.enqueue_many db request_batch));
  let rec loop completed =
    let jobs =
      expect_ok
        (M.Memory.fetch
          db
          queue
          ~limit:50
          ~locked_by:(M.WorkerId.from_string_unchecked "suri-jobs-bench-worker"))
    in
    match jobs with
    | [] -> completed
    | _ ->
        List.for_each jobs ~fn:(fun job -> expect_ok (M.Memory.complete db job.M.Job.stored));
        loop (completed + List.length jobs)
  in
  keep (loop 0)

let hot_path: Bench.bench_config = { iterations = 50; warmup = 5 }

let memory_path: Bench.bench_config = { iterations = 25; warmup = 5 }

let benchmarks =
  Bench.[
    with_config
      ~config:hot_path
      "suri-jobs queue payload json roundtrip"
      bench_payload_json_roundtrip;
    with_config ~config:memory_path "suri-jobs memory enqueue_many 1000" bench_memory_enqueue_many;
    with_config
      ~config:memory_path
      "suri-jobs memory fetch+complete 1000"
      bench_memory_fetch_complete;
  ]

let main ~args = Bench.Cli.main ~name:"suri-jobs benchmarks" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
