open Std
open Propane
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

let queue_id = M.QueueId.from_string_unchecked "suri-jobs.property.payload"

let worker_id = M.WorkerId.from_string_unchecked "SuriJobsPropertyPayload"

let worker_lock = M.WorkerId.from_string_unchecked "suri-jobs-property-worker"

let queue: (payload, unit) M.Queue.t =
  M.Queue.make ~id:queue_id ~worker:worker_id ~encode:payload_encode ~decode:payload_decode ()

let payload repo_key since = ({ repo_key; since }: payload)

let prop_ok result =
  match result with
  | Ok value -> value
  | Error error -> Property.fail (M.Error.to_string error)

let string_arb =
  Arbitrary.make
    ~print:Printer.string
    (Generator.string_size (Generator.int_range 0 64) Generator.char_printable)

let payload_arb =
  Arbitrary.make
    ~print:(fun (payload: payload) ->
      "{ repo_key = "
      ^ Printer.string payload.repo_key
      ^ "; since = "
      ^ Printer.option Printer.string payload.since
      ^ " }")
    (Generator.map2 payload string_arb.gen (Generator.option string_arb.gen))

let priority_list_arb =
  Arbitrary.make
    ~print:(Printer.list Printer.int)
    (Generator.list_size (Generator.int_range 0 32) (Generator.int_range 0 16))

let unique_key_arb =
  Arbitrary.make
    ~print:Printer.string
    (Generator.string_size (Generator.int_range 0 48) Generator.char_printable)

let states = [
  M.State.Available;
  M.State.Scheduled;
  M.State.Executing;
  M.State.Retryable;
  M.State.Completed;
  M.State.Cancelled;
  M.State.Discarded;
  M.State.Suspended;
]

let state_arb =
  Arbitrary.make
    ~print:M.State.to_string
    (Generator.one_of (List.map states ~fn:(fun state -> Generator.return state)))

let rec enqueue_priorities db index = fun priorities ->
  match priorities with
  | [] -> Ok ()
  | priority :: rest ->
      let* request =
        M.Job.enqueue
          ~id:(M.JobId.from_string_unchecked ("property-priority-" ^ Int.to_string index))
          ~priority
          queue
          (payload ("repo/" ^ Int.to_string index) None)
      in
      let* _stored = M.Memory.enqueue db request in
      enqueue_priorities db (index + 1) rest

let queue_json_roundtrips_payload =
  property
    "queue json roundtrips payloads"
    payload_arb
    (fun payload ->
      let encoded = prop_ok (M.Queue.encode_args queue payload) in
      let decoded =
        prop_ok
          (M.Queue.decode_args
            queue
            ~job_id:(M.JobId.from_string_unchecked "property-json-roundtrip")
            encoded)
      in
      decoded = payload)

let state_string_roundtrips =
  property
    "state string roundtrips"
    state_arb
    (fun state -> prop_ok (M.State.from_string (M.State.to_string state)) = state)

let memory_fetches_by_priority =
  property
    "memory fetches jobs by priority"
    priority_list_arb
    (fun priorities ->
      let db = M.Memory.create () in
      prop_ok (enqueue_priorities db 0 priorities);
      let fetched =
        prop_ok (M.Memory.fetch db queue ~limit:(List.length priorities) ~locked_by:worker_lock)
      in
      let fetched_priorities = List.map fetched ~fn:(fun job -> job.M.Job.stored.M.Job.priority) in
      fetched_priorities = List.sort priorities ~compare:Int.compare)

let memory_unique_key_is_active_idempotence =
  property
    "memory unique keys return the active job"
    unique_key_arb
    (fun key ->
      let db = M.Memory.create () in
      let unique_key = M.UniqueKey.from_string_unchecked ("property:" ^ key) in
      let first =
        prop_ok
          (M.Job.enqueue
            ~id:(M.JobId.from_string_unchecked "property-unique-first")
            ~unique_key
            queue
            (payload "repo/first" None))
      in
      let second =
        prop_ok
          (M.Job.enqueue
            ~id:(M.JobId.from_string_unchecked "property-unique-second")
            ~unique_key
            queue
            (payload "repo/second" None))
      in
      let first_stored = prop_ok (M.Memory.enqueue db first) in
      let second_stored = prop_ok (M.Memory.enqueue db second) in
      M.JobId.equal first_stored.M.Job.id second_stored.M.Job.id
      && M.JobId.equal first_stored.M.Job.id (M.JobId.from_string_unchecked "property-unique-first"))

let tests = [
  queue_json_roundtrips_payload;
  state_string_roundtrips;
  memory_fetches_by_priority;
  memory_unique_key_is_active_idempotence;
]

let main ~args = Test.Cli.main ~name:"suri_jobs_property_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
