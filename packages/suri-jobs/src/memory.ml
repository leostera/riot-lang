open Std
open Result.Syntax

module Vector = Collections.Vector

type t = {
  jobs: Job.stored Vector.t;
}

let create () = { jobs = Vector.create () }

let migrate _db = Ok ()

let vector_to_list vector = Array.to_list (Vector.to_array vector)

let update_by_id db job_id ~fn =
  let len = Vector.length db.jobs in
  let rec loop index =
    if index >= len then
      false
    else
      match Vector.get db.jobs ~at:index with
      | Some job when Job_id.equal job.Job.id job_id ->
          let _ = Vector.set db.jobs ~at:index ~value:(fn job) in
          true
      | Some _
      | None -> loop (index + 1)
  in
  loop 0

let find_by_id db job_id =
  List.find (vector_to_list db.jobs) ~fn:(fun job -> Job_id.equal job.Job.id job_id)

let find_active_unique db unique_key =
  List.find
    (vector_to_list db.jobs)
    ~fn:(fun job ->
      State.active job.Job.state && (
        match job.Job.unique_key with
        | Some value -> Unique_key.equal value unique_key
        | None -> false
      ))

let enqueue_without_unique db (request: 'payload Job.enqueue) =
  match find_by_id db request.Job.id with
  | Some existing -> Ok existing
  | None ->
      let stored = Job.stored_from_enqueue request in
      Vector.push db.jobs ~value:stored;
      Ok stored

let enqueue_with_unique db (request: 'payload Job.enqueue) unique_key =
  match find_active_unique db unique_key with
  | Some existing -> Ok existing
  | None -> enqueue_without_unique db request

let enqueue db (request: 'payload Job.enqueue) =
  match request.Job.unique_key with
  | Some unique_key -> enqueue_with_unique db request unique_key
  | None -> enqueue_without_unique db request

let enqueue_many db (requests: 'payload Job.enqueue list) =
  let rec loop acc = fun requests ->
    match requests with
    | [] -> Ok (List.reverse acc)
    | request :: rest ->
        let* stored = enqueue db request in
        loop (stored :: acc) rest
  in
  loop [] requests

let stale_execution cutoff (job: Job.stored) =
  match (job.Job.state, job.Job.locked_at) with
  | (State.Executing, Some locked_at) -> Clock.lte locked_at cutoff
  | _ -> false

let due now cutoff (job: Job.stored) =
  job.Job.attempt < job.Job.max_attempts
  && ((State.runnable job.Job.state && Clock.lte job.Job.scheduled_at now)
  || stale_execution cutoff job)

let compare_scheduled (left: Job.stored) (right: Job.stored) =
  match String.compare left.Job.scheduled_at right.Job.scheduled_at with
  | Order.EQ -> String.compare left.Job.inserted_at right.Job.inserted_at
  | order -> order

let compare_for_fetch (left: Job.stored) (right: Job.stored) =
  match Int.compare left.Job.priority right.Job.priority with
  | Order.EQ -> compare_scheduled left right
  | order -> order

let fetch db ?(stale_after_seconds = 900) queue ~limit ~locked_by =
  let now = Clock.now () in
  let stale_cutoff = Clock.before_seconds stale_after_seconds in
  let candidates =
    vector_to_list db.jobs
    |> List.filter
      ~fn:(fun (job: Job.stored) ->
        Queue_id.equal job.Job.queue (Queue.id queue) && due now stale_cutoff job)
    |> List.sort ~compare:compare_for_fetch
  in
  let rec take remaining acc = fun candidates ->
    match candidates with
    | [] -> Ok (List.reverse acc)
    | _ when remaining <= 0 -> Ok (List.reverse acc)
    | job :: rest ->
        let leased = {
          job with
          Job.state = State.Executing;
          Job.attempt = job.Job.attempt + 1;
          Job.attempted_at = Some now;
          Job.locked_by = Some locked_by;
          Job.locked_at = Some now;
          Job.last_error = None;
        }
        in
        let _ = update_by_id db job.Job.id ~fn:(fun _ -> leased) in
        let* typed = Job.decode queue leased in
        take (remaining - 1) (typed :: acc) rest
  in
  take limit [] candidates

let complete db (stored: Job.stored) =
  let now = Clock.now () in
  let _ =
    update_by_id
      db
      stored.Job.id
      ~fn:(fun job ->
        {
          job with
          Job.state = State.Completed;
          Job.completed_at = Some now;
          Job.locked_by = None;
          Job.locked_at = None;
          Job.last_error = None;
        })
  in
  Ok ()

let fail db (stored: Job.stored) ~error ~backoff_seconds =
  let now = Clock.now () in
  let next = Clock.after_seconds backoff_seconds in
  let _ =
    update_by_id
      db
      stored.Job.id
      ~fn:(fun job ->
        if job.Job.attempt >= job.Job.max_attempts then
          {
            job with
            Job.state = State.Discarded;
            Job.discarded_at = Some now;
            Job.last_error = Some error;
            Job.locked_by = None;
            Job.locked_at = None;
          }
        else
          {
            job with
            Job.state = State.Retryable;
            Job.scheduled_at = next;
            Job.last_error = Some error;
            Job.locked_by = None;
            Job.locked_at = None;
          })
  in
  Ok ()

let cancel db (stored: Job.stored) =
  let now = Clock.now () in
  let _ =
    update_by_id
      db
      stored.Job.id
      ~fn:(fun job ->
        {
          job with
          Job.state = State.Cancelled;
          Job.cancelled_at = Some now;
          Job.locked_by = None;
          Job.locked_at = None;
        })
  in
  Ok ()

let list db ~limit =
  let rec take remaining acc = fun candidates ->
    match candidates with
    | [] -> Ok (List.reverse acc)
    | _ when remaining <= 0 -> Ok (List.reverse acc)
    | job :: rest -> take (remaining - 1) (job :: acc) rest
  in
  vector_to_list db.jobs
  |> List.reverse
  |> take limit []

let get db ~job_id = Ok (find_by_id db job_id)

let state_counts db =
  Ok (List.fold_left
    (vector_to_list db.jobs)
    ~init:Fanout.empty
    ~fn:(fun status job -> Fanout.add job.Job.state status))

let fanout_status db ~fanout_id =
  let jobs =
    vector_to_list db.jobs
    |> List.filter
      ~fn:(fun (job: Job.stored) ->
        match job.Job.fanout_id with
        | Some value -> Fanout_id.equal value fanout_id
        | None -> false)
  in
  Ok (List.fold_left jobs ~init:Fanout.empty ~fn:(fun status job -> Fanout.add job.Job.state status))
