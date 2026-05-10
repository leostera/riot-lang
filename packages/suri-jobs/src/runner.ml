open Std
open Result.Syntax

type queue = Queue.packed

let current_db: Sqlx.Pool.t option Sync.Cell.t = Sync.Cell.create None

let configure db = Sync.Cell.set current_db (Some db)

let configured_db () =
  match Sync.Cell.get current_db with
  | Some db -> Ok db
  | None -> Error Error.Not_started

let start_db config =
  match Jobs_config.connect config with
  | Error _ as error -> error
  | Ok db -> (
      match Jobs_config.migrate config db with
      | Ok () -> Ok db
      | Error error ->
          Sqlx_backend.shutdown db;
          Error error
    )

let start_db_or_raise config =
  match start_db config with
  | Ok db -> db
  | Error error -> raise (Failure ("suri-jobs failed to start: " ^ Error.to_string error))

let submit
  (type ctx error job)
  ?id
  ?max_attempts
  ?priority
  ?scheduled_at
  ?unique_key
  ?fanout_id
  ?parent_job_id
  ?meta
  ?tags
  (queue_module: (module Queue.Intf with type ctx = ctx and type error = error and type job = job))
  job =
  let* db = configured_db () in
  let queue: (job, unit) Queue.t = Queue.from_submit_intf queue_module in
  let* request =
    Job.enqueue
      ?id
      ?max_attempts
      ?priority
      ?scheduled_at
      ?unique_key
      ?fanout_id
      ?parent_job_id
      ?meta
      ?tags
      queue
      job
  in
  let* stored = Sqlx_backend.enqueue db request in
  Ok stored.Job.id

type packed_work =
  | Work: 'job Ref.t * ('job, unit) Job.t option -> packed_work

type Message.t +=
  | QueueWorkerReady of {
      queue: Queue_id.t;
      locked_by: Worker_id.t;
      reply_to: Pid.t;
    }
  | QueueWork of {
      queue: Queue_id.t;
      locked_by: Worker_id.t;
      work: packed_work;
    }

let queue_name_from_config config = Queue_id.to_string config.Queue.Config.id

let worker_id queue_name index =
  Worker_id.from_string_unchecked (queue_name ^ ":" ^ Int.to_string index)

let parse_json_or_string value =
  match Data.Json.from_string value with
  | Ok json -> json
  | Error _ -> Data.Json.string value

let job_error_json ~kind ~queue_name ~job_id ~attempt ~detail =
  Data.Json.obj
    [
      ("kind", Data.Json.string kind);
      ("queue", Data.Json.string queue_name);
      ("job_id", Data.Json.string (Job_id.to_string job_id));
      ("attempt", Data.Json.int attempt);
      ("detail", detail);
    ]
  |> Data.Json.to_string

let encode_error queue queue_name (stored: Job.stored) error =
  match Serde_json.to_string queue.Queue.err_serializer error with
  | Ok value ->
      job_error_json
        ~kind:"queue_error"
        ~queue_name
        ~job_id:stored.Job.id
        ~attempt:stored.Job.attempt
        ~detail:(parse_json_or_string value)
  | Error serde_error ->
      job_error_json
        ~kind:"queue_error_encode_failed"
        ~queue_name
        ~job_id:stored.Job.id
        ~attempt:stored.Job.attempt
        ~detail:(Data.Json.obj
          [ ("message", Data.Json.string (Serde.Error.to_string serde_error)); ])

let exception_to_string = fun caught ->
  match caught with
  | Failure message -> "Failure: " ^ message
  | Invalid_argument message -> "Invalid_argument: " ^ message
  | Not_found -> "Not_found"
  | End_of_file -> "End_of_file"
  | Division_by_zero -> "Division_by_zero"
  | exn -> Exception.to_string exn

let safe_stderr message =
  try eprintln message with
  | _ -> ()

let complete_job db queue_name stored =
  try
    match Sqlx_backend.complete db stored with
    | Ok () -> ()
    | Error error ->
        Log.error
          ("suri-jobs queue "
          ^ queue_name
          ^ " failed to complete job "
          ^ Job_id.to_string stored.Job.id
          ^ ": "
          ^ Error.to_string error)
  with
  | exn ->
      Log.error
        ("suri-jobs queue "
        ^ queue_name
        ^ " complete raised for job "
        ^ Job_id.to_string stored.Job.id
        ^ ": "
        ^ exception_to_string exn)

let fail_job db queue_name stored ~error ~backoff_seconds =
  try
    match Sqlx_backend.fail db stored ~error ~backoff_seconds with
    | Ok () -> ()
    | Error fail_error ->
        Log.error
          ("suri-jobs queue "
          ^ queue_name
          ^ " failed to retry job "
          ^ Job_id.to_string stored.Job.id
          ^ ": "
          ^ Error.to_string fail_error)
  with
  | exn ->
      Log.error
        ("suri-jobs queue "
        ^ queue_name
        ^ " retry raised for job "
        ^ Job_id.to_string stored.Job.id
        ^ ": "
        ^ exception_to_string exn)

let run_job
  (type ctx error job)
  db
  (queue: (ctx, error, job) Queue.packed_queue)
  queue_name
  ctx
  (work: (job, unit) Job.t) =
  try
    match queue.handle_job ctx work.Job.args with
    | Ok next_ctx ->
        complete_job db queue_name work.Job.stored;
        next_ctx
    | Error error -> (
        let error_text = encode_error queue queue_name work.Job.stored error in
        match queue.handle_error ctx error with
        | Ok next_ctx ->
            fail_job
              db
              queue_name
              work.Job.stored
              ~error:error_text
              ~backoff_seconds:queue.config.Queue.Config.retry_backoff_seconds;
            next_ctx
        | Error handler_error ->
            let handler_error_text = encode_error queue queue_name work.Job.stored handler_error in
            Log.error
              ("suri-jobs queue "
              ^ queue_name
              ^ " error handler failed for job "
              ^ Job_id.to_string work.Job.stored.Job.id
              ^ ": "
              ^ handler_error_text);
            fail_job
              db
              queue_name
              work.Job.stored
              ~error:handler_error_text
              ~backoff_seconds:queue.config.Queue.Config.retry_backoff_seconds;
            ctx
      )
  with
  | exn ->
      let error_text =
        job_error_json
          ~kind:"job_exception"
          ~queue_name
          ~job_id:work.Job.stored.Job.id
          ~attempt:work.Job.stored.Job.attempt
          ~detail:(Data.Json.obj [ ("message", Data.Json.string (exception_to_string exn)); ])
      in
      let message =
        "suri-jobs queue "
        ^ queue_name
        ^ " job "
        ^ Job_id.to_string work.Job.stored.Job.id
        ^ " raised: "
        ^ exception_to_string exn
      in
      safe_stderr message;
      Log.error message;
      fail_job
        db
        queue_name
        work.Job.stored
        ~error:error_text
        ~backoff_seconds:queue.config.Queue.Config.retry_backoff_seconds;
      ctx

let select_work (type job) queue_id locked_by (job_ref: job Ref.t) = fun message ->
  let select (job: (job, unit) Job.t option) = Select job in
  match message with
  | QueueWork { queue; locked_by = worker; work = Work (received_ref, job) } ->
      if Queue_id.equal queue queue_id && Worker_id.equal worker locked_by then (
        match Ref.type_equal received_ref job_ref with
        | Some Type.Equal -> select job
        | None -> Skip
      ) else
        Skip
  | _ -> Skip

let request_work scheduler_ref queue_id queue_name locked_by job_ref =
  match !scheduler_ref with
  | None ->
      Log.warn ("suri-jobs queue " ^ queue_name ^ " worker has no scheduler pid yet");
      None
  | Some scheduler ->
      send scheduler (QueueWorkerReady { queue = queue_id; locked_by; reply_to = self () });
      (
        try receive
          ~selector:(select_work queue_id locked_by job_ref)
          ~timeout:(Time.Duration.from_secs 300)
          () with
        | Receive_timeout ->
            Log.debug ("suri-jobs queue " ^ queue_name ^ " timed out waiting for job scheduler response");
            None
      )

let start_worker
  (type ctx error job)
  db
  scheduler_ref
  (queue: (ctx, error, job) Queue.packed_queue)
  index =
  Actor.spawn
    (fun () ->
      let config = queue.config in
      let queue_id = config.Queue.Config.id in
      let queue_name = queue_name_from_config config in
      let locked_by = worker_id queue_name index in
      Log.info ("suri-jobs queue " ^ queue_name ^ " worker " ^ Int.to_string index ^ " started");
      let rec loop ctx =
        let next_ctx =
          try
            match request_work scheduler_ref queue_id queue_name locked_by queue.job_ref with
            | None ->
                sleep config.Queue.Config.poll_interval;
                ctx
            | Some work -> run_job db queue queue_name ctx work
          with
          | exn ->
              let message =
                "suri-jobs queue "
                ^ queue_name
                ^ " worker "
                ^ Int.to_string index
                ^ " raised outside job handling: "
                ^ exception_to_string exn
              in
              safe_stderr message;
              Log.error message;
              sleep config.Queue.Config.poll_interval;
              ctx
        in
        loop next_ctx
      in
      loop queue.ctx)

let fetch_one_job
  (type ctx error job)
  db
  (queue: (ctx, error, job) Queue.packed_queue)
  queue_name
  locked_by =
  try
    match Sqlx_backend.fetch
      db
      ~stale_after_seconds:queue.config.Queue.Config.stale_after_seconds
      queue.queue
      ~limit:1
      ~locked_by with
    | Error error ->
        Log.error ("suri-jobs queue " ^ queue_name ^ " fetch failed: " ^ Error.to_string error);
        None
    | Ok [] -> None
    | Ok (job :: _) -> Some job
  with
  | exn ->
      Log.error
        ("suri-jobs queue "
        ^ queue_name
        ^ " fetch raised for worker "
        ^ Worker_id.to_string locked_by
        ^ ": "
        ^ exception_to_string exn);
      None

let start_job_scheduler db (Queue.Packed queue) =
  Actor.spawn
    (fun () ->
      let config = queue.config in
      let queue_id = config.Queue.Config.id in
      let queue_name = queue_name_from_config config in
      Log.info ("suri-jobs queue " ^ queue_name ^ " job scheduler started");
      let rec loop () =
        (
          try
            let (locked_by, reply_to) =
              receive
                ~selector:(fun message ->
                  match message with
                  | QueueWorkerReady { queue; locked_by; reply_to } ->
                      if Queue_id.equal queue queue_id then
                        Select (locked_by, reply_to)
                      else
                        Skip
                  | _ -> Skip)
                ()
            in
            let job = fetch_one_job db queue queue_name locked_by in
            send
              reply_to
              (
                QueueWork {
                  queue = queue_id;
                  locked_by;
                  work = Work (queue.job_ref, job);
                }
              )
          with
          | exn ->
              let message =
                "suri-jobs queue " ^ queue_name ^ " job scheduler raised: " ^ exception_to_string exn
              in
              safe_stderr message;
              Log.error message;
              sleep (Time.Duration.from_secs 1)
        );
        loop ()
      in
      loop ())

let queue_supervisor_child_spec db (queue: queue) =
  let (Queue.Packed packed) = queue in
  let config = packed.config in
  let queue_name = queue_name_from_config config in
  let total = config.Queue.Config.concurrency in
  let scheduler_ref = ref None in
  let scheduler_child =
    Supervisor.child_spec
      ~id:(queue_name ^ "-job-scheduler")
      ~start:(fun () ->
        let pid = start_job_scheduler db queue in
        scheduler_ref := Some pid;
        pid)
      ~restart:Supervisor.Permanent
      ~shutdown:Supervisor.BrutalKill
      ()
  in
  let rec worker_specs index acc =
    if index > total then
      List.reverse acc
    else
      let id = queue_name ^ "-worker-" ^ Int.to_string index in
      let child =
        Supervisor.child_spec
          ~id
          ~start:(fun () ->
            start_worker db scheduler_ref packed index)
          ~restart:Supervisor.Permanent
          ~shutdown:Supervisor.BrutalKill
          ()
      in
      worker_specs (index + 1) (child :: acc)
  in
  Supervisor.child_spec
    ~id:(queue_name ^ "-queue-supervisor")
    ~start:(fun () ->
      Supervisor.to_pid
        (Supervisor.start_link
          ~strategy:Supervisor.OneForOne
          ~intensity:{ Supervisor.max_restarts = 10_000; window = Time.Duration.from_secs 60 }
          ~children:(scheduler_child :: worker_specs 1 [])
          ()))
    ~restart:Supervisor.Permanent
    ~shutdown:Supervisor.Infinity
    ~child_type:Supervisor.Supervisor
    ()

let child_specs db queues = List.map queues ~fn:(queue_supervisor_child_spec db)

let recover_queue db (Queue.Packed queue) =
  match Sqlx_backend.recover_executing db queue.queue with
  | Ok () -> ()
  | Error error -> raise (Failure ("suri-jobs failed to recover queue: " ^ Error.to_string error))

let start_link_with_db ~db queues =
  List.for_each queues ~fn:(recover_queue db);
  configure db;
  Supervisor.start_link
    ~strategy:Supervisor.OneForOne
    ~intensity:{ Supervisor.max_restarts = 10_000; window = Time.Duration.from_secs 60 }
    ~children:(child_specs db queues)
    ()

let start_link ~config queues =
  let db = start_db_or_raise config in
  start_link_with_db ~db queues

let start_link_with_config = start_link

let start_link queues =
  match Jobs_config.load () with
  | Ok config -> start_link_with_config ~config queues
  | Error error -> raise (Failure ("suri-jobs config error: " ^ Std.Config.error_to_string error))

let child_spec_with_config ?(id = "suri-jobs") ~config queues =
  Supervisor.child_spec
    ~id
    ~start:(fun () -> Supervisor.to_pid (start_link_with_config ~config queues))
    ~restart:Supervisor.Permanent
    ~shutdown:Supervisor.Infinity
    ~child_type:Supervisor.Supervisor
    ()

let child_spec ?(id = "suri-jobs") queues =
  Supervisor.child_spec
    ~id
    ~start:(fun () -> Supervisor.to_pid (start_link queues))
    ~restart:Supervisor.Permanent
    ~shutdown:Supervisor.Infinity
    ~child_type:Supervisor.Supervisor
    ()
