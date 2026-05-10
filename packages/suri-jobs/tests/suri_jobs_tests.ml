open Std
open Result.Syntax

module M = Suri_jobs
module Ser = Serde.Ser
module De = Serde.De
module Vector = Collections.Vector
module Testing = Suri.Testing
module Response = Suri.Response
module Testcontainers = Testcontainers

type issue_sync_payload = {
  repo_key: string;
  since: string option;
}

type issue_sync_field =
  | Field_repo_key
  | Field_since

type issue_sync_builder = {
  mutable repo_key: string option;
  mutable since: string option option;
}

let issue_sync_payload_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "repo_key" Ser.string (fun (payload: issue_sync_payload) -> payload.repo_key);
          Ser.field
            "since"
            (Ser.option Ser.string)
            (fun (payload: issue_sync_payload) -> payload.since);
        ]
    )

let issue_sync_payload_decode =
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
          ({ repo_key; since }: issue_sync_payload))

let issue_sync_queue_id = M.QueueId.from_string_unchecked "github.issue-sync"

let issue_sync_worker_id = M.WorkerId.from_string_unchecked "GithubIssueSync"

let test_worker_id = M.WorkerId.from_string_unchecked "test-worker"

let runner_worker_id = M.WorkerId.from_string_unchecked "worker"

let issue_sync_queue: (issue_sync_payload, int) M.Queue.t =
  M.Queue.make
    ~id:issue_sync_queue_id
    ~worker:issue_sync_worker_id
    ~encode:issue_sync_payload_encode
    ~decode:issue_sync_payload_decode
    ()

let payload ?since (repo_key: string) = ({ repo_key; since }: issue_sync_payload)

let expect_ok result =
  match result with
  | Ok value -> value
  | Error error -> panic (M.Error.to_string error)

let expect_string_ok result =
  match result with
  | Ok value -> value
  | Error error -> panic error

let expect condition message =
  if condition then
    Ok ()
  else
    Error message

let expect_response result =
  match result with
  | Ok response -> Ok response
  | Error error -> Error (Testing.response_error_to_string error)

let expect_status status response =
  match Testing.Expect.status status response with
  | Ok () -> Ok ()
  | Error error ->
      Error (Testing.Expect.error_to_string error ^ "; body: " ^ Response.(response.body))

let expect_contains body needle =
  expect (String.contains body needle) ("expected body to contain: " ^ needle ^ "\n\n" ^ body)

let unique_text prefix = prefix ^ "-" ^ UUID.to_string (UUID.v7_monotonic ())

type postgres_runtime =
  | PostgresSkipped
  | PostgresUnavailable of string
  | PostgresReady of string * Postgres.Config.t

let postgres_runtime_status = ref None

let postgres_container = ref None

let postgres_image () =
  Testcontainers.Generic_image.(make "postgres" "16-alpine"
  |> with_env_var ~name:"POSTGRES_USER" ~value:"postgres"
  |> with_env_var ~name:"POSTGRES_PASSWORD" ~value:"postgres"
  |> with_env_var ~name:"POSTGRES_DB" ~value:"suri_jobs_test"
  |> with_exposed_port ~port:5_432
  |> with_readiness_policy
    ~policy:(ReadinessPolicy.log
      ~message:"database system is ready to accept connections"
      ~duration:(Duration.of_secs 30)
      ~retry:60))

let url_host = fun host ->
  if String.contains host ":" then
    "[" ^ host ^ "]"
  else
    host

let postgres_config_from_container container =
  match Testcontainers.Container.host_port container ~port:5_432 with
  | Error error -> Error (Testcontainers.error_to_string error)
  | Ok address ->
      let postgres_url =
        "postgresql://postgres:postgres@"
        ^ url_host (Net.Addr.ip address)
        ^ ":"
        ^ Int.to_string (Net.Addr.port address)
        ^ "/suri_jobs_test"
      in
      (
        match Postgres.Config.from_string postgres_url with
        | Error error -> Error (Postgres.Config.parse_error_to_string error)
        | Ok config -> Ok (postgres_url, config)
      )

let check_postgres_available postgres_url postgres_config =
  match M.Sqlx_backend.connect ~pool_size:1 ~driver:(module Postgres.Driver) postgres_config with
  | Error error ->
      Error ("suri-jobs postgres container at "
      ^ postgres_url
      ^ " did not accept connections: "
      ^ M.Error.to_string error)
  | Ok db ->
      M.Sqlx_backend.shutdown db;
      Ok ()

let wait_for_postgres_available postgres_url postgres_config =
  let rec loop attempts last_error =
    if attempts <= 0 then
      Error last_error
    else
      match check_postgres_available postgres_url postgres_config with
      | Ok () -> Ok ()
      | Error error ->
          sleep (Time.Duration.from_millis 250);
          loop (attempts - 1) error
  in
  loop 80 ("suri-jobs postgres container at " ^ postgres_url ^ " did not become available")

let start_postgres_runtime () =
  if not (Testcontainers.docker_available ()) then
    PostgresSkipped
  else
    match Testcontainers.start (postgres_image ()) with
    | Error error ->
        PostgresUnavailable ("failed to start postgres test container: "
        ^ Testcontainers.error_to_string error)
    | Ok container ->
        postgres_container := Some container;
        (
          match postgres_config_from_container container with
          | Error error -> PostgresUnavailable error
          | Ok (postgres_url, config) ->
              match wait_for_postgres_available postgres_url config with
              | Ok () -> PostgresReady (postgres_url, config)
              | Error error -> PostgresUnavailable error
        )

let postgres_runtime () =
  match !postgres_runtime_status with
  | Some status -> status
  | None ->
      let status = start_postgres_runtime () in
      postgres_runtime_status := Some status;
      status

let teardown_postgres_container = fun () ->
  match !postgres_container with
  | None -> Ok ()
  | Some container ->
      postgres_container := None;
      Testcontainers.Container.remove container
      |> Result.map_err ~fn:Testcontainers.error_to_string

let with_postgres postgres_url postgres_config fn =
  (
    let jobs_config = M.Config.make ~pool_size:4 ~driver:(module Postgres.Driver) postgres_config in
    match M.Sqlx_backend.connect ~pool_size:4 ~driver:(module Postgres.Driver) postgres_config with
    | Error error ->
        Error ("suri-jobs postgres container at "
        ^ postgres_url
        ^ " failed to connect: "
        ^ M.Error.to_string error)
    | Ok db ->
        let result =
          match M.Sqlx_backend.migrate db with
          | Error error -> Error (M.Error.to_string error)
          | Ok () -> fn db jobs_config
        in
        M.Sqlx_backend.shutdown db;
        result
  )

let postgres_case name fn =
  if Testcontainers.docker_available () then
    Test.case
      ~size:Large
      name
      (fun ctx ->
        match postgres_runtime () with
        | PostgresSkipped -> Ok ()
        | PostgresUnavailable error -> Error error
        | PostgresReady (postgres_url, config) -> fn ctx postgres_url config)
  else
    Test.skip ~size:Large name (fun _ctx -> Ok ())

let wait_for_fanout db ~fanout_id ~completed ~discarded =
  let rec loop attempts =
    if attempts <= 0 then
      Error "timed out waiting for suri-jobs fanout"
    else
      let status = expect_ok (M.Sqlx_backend.fanout_status db ~fanout_id) in
      if status.M.Fanout.completed >= completed && status.M.Fanout.discarded >= discarded then
        Ok status
      else (
        sleep (Time.Duration.from_millis 50);
        loop (attempts - 1)
      )
  in
  loop 100

let jobs_result result = Result.map_err result ~fn:M.Error.to_string

let test_queue_config_defaults_to_available_parallelism = fun _ctx ->
  let config =
    M.Queue.Config.make
      ~id:(M.QueueId.from_string_unchecked (unique_text "suri-jobs-default-concurrency"))
      ()
  in
  Test.assert_equal
    ~expected:(Int.max 1 Thread.available_parallelism)
    ~actual:config.M.Queue.Config.concurrency;
  Ok ()

let test_ids_from_string_validate_input = fun _ctx ->
  match M.QueueId.from_string "github.issue-sync" with
  | Error error -> Error (M.QueueId.error_to_string error)
  | Ok queue_id ->
      Test.assert_equal ~expected:"github.issue-sync" ~actual:(M.QueueId.to_string queue_id);
      (
        match M.QueueId.from_string "   " with
        | Error _ -> ()
        | Ok _ -> panic "expected empty queue id to be rejected"
      );
      (
        match M.JobId.from_string "bad id with spaces" with
        | Error _ -> Ok ()
        | Ok _ -> Error "expected invalid job id to be rejected"
      )

let test_error_to_json_is_structured = fun _ctx ->
  let json = M.Error.to_json M.Error.Not_started in
  match Data.Json.get_field "kind" json with
  | Some value ->
      Test.assert_equal ~expected:(Some "not_started") ~actual:(Data.Json.get_string value);
      Ok ()
  | None -> Error "expected structured error kind"

let test_fanout_add_count_accumulates_without_looping = fun _ctx ->
  let status = M.Fanout.add_count M.State.Completed 50 M.Fanout.empty in
  Test.assert_equal ~expected:50 ~actual:status.M.Fanout.total;
  Test.assert_equal ~expected:50 ~actual:status.M.Fanout.completed;
  Test.assert_equal ~expected:0 ~actual:status.M.Fanout.discarded;
  Ok ()

let rec update_max atomic value =
  let current = Sync.Atomic.get atomic in
  if value > current then
    if not (Sync.Atomic.compare_and_set atomic current value) then
      update_max atomic value

let test_queue_roundtrips_payload = fun _ctx ->
  let encoded =
    expect_ok
      (M.Queue.encode_args
        issue_sync_queue
        (payload ~since:"2026-05-01T00:00:00Z" "leostera/hypekit.dev"))
  in
  Test.assert_true (String.contains encoded "repo_key");
  let decoded =
    expect_ok
      (M.Queue.decode_args issue_sync_queue ~job_id:(M.JobId.from_string_unchecked "job-1") encoded)
  in
  Test.assert_equal ~expected:"leostera/hypekit.dev" ~actual:decoded.repo_key;
  Test.assert_equal ~expected:(Some "2026-05-01T00:00:00Z") ~actual:decoded.since;
  Ok ()

let test_memory_fetch_decodes_through_typed_queue = fun _ctx ->
  let db = M.Memory.create () in
  expect_ok (M.Memory.migrate db);
  let request =
    expect_ok
      (M.Job.enqueue
        ~id:(M.JobId.from_string_unchecked "job-1")
        ~fanout_id:(M.FanoutId.from_string_unchecked "fan-1")
        issue_sync_queue
        (payload "leostera/hypekit.dev"))
  in
  let stored = expect_ok (M.Memory.enqueue db request) in
  Test.assert_equal ~expected:M.State.Available ~actual:stored.M.Job.state;
  let jobs = expect_ok (M.Memory.fetch db issue_sync_queue ~limit:1 ~locked_by:test_worker_id) in
  match jobs with
  | [ job ] ->
      Test.assert_equal ~expected:"leostera/hypekit.dev" ~actual:job.M.Job.args.repo_key;
      Test.assert_equal ~expected:M.State.Executing ~actual:job.M.Job.stored.M.Job.state;
      Test.assert_equal ~expected:1 ~actual:job.M.Job.stored.M.Job.attempt;
      Test.assert_equal
        ~expected:(Some "test-worker")
        ~actual:(Option.map job.M.Job.stored.M.Job.locked_by ~fn:M.WorkerId.to_string);
      Ok ()
  | _ -> Error "expected one fetched job"

let test_worker_composition_is_typed = fun _ctx ->
  let db = M.Memory.create () in
  let request =
    expect_ok
      (M.Job.enqueue
        ~id:(M.JobId.from_string_unchecked "job-typed")
        issue_sync_queue
        (payload "leostera/hypekit.dev"))
  in
  let _stored = expect_ok (M.Memory.enqueue db request) in
  let worker =
    M.Worker.make issue_sync_queue ~run:(fun job -> Ok (String.length job.M.Job.args.repo_key))
  in
  match expect_ok (M.Memory.fetch db (M.Worker.queue worker) ~limit:1 ~locked_by:runner_worker_id) with
  | [ job ] ->
      Test.assert_equal
        ~expected:(String.length "leostera/hypekit.dev")
        ~actual:(expect_ok (M.Worker.run worker job));
      Ok ()
  | _ -> Error "expected one fetched job"

let test_memory_retry_and_discard = fun _ctx ->
  let db = M.Memory.create () in
  let request =
    expect_ok
      (M.Job.enqueue
        ~id:(M.JobId.from_string_unchecked "job-retry")
        ~max_attempts:2
        ~fanout_id:(M.FanoutId.from_string_unchecked "fan-retry")
        issue_sync_queue
        (payload "leostera/hypekit.dev"))
  in
  let _stored = expect_ok (M.Memory.enqueue db request) in
  let first =
    match expect_ok (M.Memory.fetch db issue_sync_queue ~limit:1 ~locked_by:runner_worker_id) with
    | [ job ] -> job
    | _ -> panic "expected first attempt"
  in
  expect_ok (M.Memory.fail db first.M.Job.stored ~error:"rate limited" ~backoff_seconds:0);
  let retry_status =
    expect_ok (M.Memory.fanout_status db ~fanout_id:(M.FanoutId.from_string_unchecked "fan-retry"))
  in
  Test.assert_equal ~expected:1 ~actual:retry_status.M.Fanout.retryable;
  let second =
    match expect_ok (M.Memory.fetch db issue_sync_queue ~limit:1 ~locked_by:runner_worker_id) with
    | [ job ] -> job
    | _ -> panic "expected second attempt"
  in
  Test.assert_equal ~expected:2 ~actual:second.M.Job.stored.M.Job.attempt;
  Test.assert_equal ~expected:None ~actual:second.M.Job.stored.M.Job.last_error;
  expect_ok (M.Memory.fail db second.M.Job.stored ~error:"still rate limited" ~backoff_seconds:0);
  let discarded_status =
    expect_ok (M.Memory.fanout_status db ~fanout_id:(M.FanoutId.from_string_unchecked "fan-retry"))
  in
  Test.assert_equal ~expected:1 ~actual:discarded_status.M.Fanout.discarded;
  Ok ()

let test_memory_rescues_stale_execution = fun _ctx ->
  let db = M.Memory.create () in
  let request =
    expect_ok
      (M.Job.enqueue
        ~id:(M.JobId.from_string_unchecked "job-stale")
        issue_sync_queue
        (payload "leostera/hypekit.dev"))
  in
  let _stored = expect_ok (M.Memory.enqueue db request) in
  let first =
    match expect_ok (M.Memory.fetch db issue_sync_queue ~limit:1 ~locked_by:test_worker_id) with
    | [ job ] -> job
    | _ -> panic "expected first attempt"
  in
  Test.assert_equal ~expected:1 ~actual:first.M.Job.stored.M.Job.attempt;
  let still_locked =
    expect_ok (M.Memory.fetch db issue_sync_queue ~limit:1 ~locked_by:runner_worker_id)
  in
  Test.assert_equal ~expected:0 ~actual:(List.length still_locked);
  let rescued =
    expect_ok
      (M.Memory.fetch
        db
        ~stale_after_seconds:0
        issue_sync_queue
        ~limit:1
        ~locked_by:runner_worker_id)
  in
  match rescued with
  | [ job ] ->
      Test.assert_equal ~expected:2 ~actual:job.M.Job.stored.M.Job.attempt;
      Test.assert_equal
        ~expected:(Some "worker")
        ~actual:(Option.map job.M.Job.stored.M.Job.locked_by ~fn:M.WorkerId.to_string);
      Ok ()
  | _ -> Error "expected stale executing job to be rescued"

let test_memory_unique_key_returns_active_job = fun _ctx ->
  let db = M.Memory.create () in
  let first =
    expect_ok
      (M.Job.enqueue
        ~id:(M.JobId.from_string_unchecked "job-unique-1")
        ~unique_key:(M.UniqueKey.from_string_unchecked "repo:leostera/hypekit.dev")
        issue_sync_queue
        (payload "leostera/hypekit.dev"))
  in
  let second =
    expect_ok
      (M.Job.enqueue
        ~id:(M.JobId.from_string_unchecked "job-unique-2")
        ~unique_key:(M.UniqueKey.from_string_unchecked "repo:leostera/hypekit.dev")
        issue_sync_queue
        (payload "leostera/hypekit.dev"))
  in
  let first_stored = expect_ok (M.Memory.enqueue db first) in
  let second_stored = expect_ok (M.Memory.enqueue db second) in
  Test.assert_equal
    ~expected:(M.JobId.to_string first_stored.M.Job.id)
    ~actual:(M.JobId.to_string second_stored.M.Job.id);
  Ok ()

let test_memory_deterministic_job_id_is_idempotent_after_completion = fun _ctx ->
  let db = M.Memory.create () in
  let request =
    expect_ok
      (M.Job.enqueue
        ~id:(M.JobId.from_string_unchecked "sync-github-issues:leostera/hypekit.dev")
        ~unique_key:(M.UniqueKey.from_string_unchecked "sync-github-issues:leostera/hypekit.dev")
        issue_sync_queue
        (payload "leostera/hypekit.dev"))
  in
  let stored = expect_ok (M.Memory.enqueue db request) in
  expect_ok (M.Memory.complete db stored);
  let again = expect_ok (M.Memory.enqueue db request) in
  Test.assert_equal
    ~expected:(M.JobId.to_string stored.M.Job.id)
    ~actual:(M.JobId.to_string again.M.Job.id);
  Test.assert_equal ~expected:M.State.Completed ~actual:again.M.Job.state;
  Ok ()

let test_postgres_unique_key_returns_active_job = fun _ctx postgres_url postgres_config ->
  with_postgres
    postgres_url
    postgres_config
    (fun db _jobs_config ->
      let unique_key = M.UniqueKey.from_string_unchecked (unique_text "postgres-active-unique") in
      let first =
        expect_ok
          (M.Job.enqueue
            ~id:(M.JobId.from_string_unchecked (unique_text "postgres-active-first"))
            ~unique_key
            issue_sync_queue
            (payload "leostera/hypekit.dev"))
      in
      let second =
        expect_ok
          (M.Job.enqueue
            ~id:(M.JobId.from_string_unchecked (unique_text "postgres-active-second"))
            ~unique_key
            issue_sync_queue
            (payload "leostera/hypekit.dev"))
      in
      let first_stored = expect_ok (M.Sqlx_backend.enqueue db first) in
      let second_stored = expect_ok (M.Sqlx_backend.enqueue db second) in
      Test.assert_equal
        ~expected:(M.JobId.to_string first_stored.M.Job.id)
        ~actual:(M.JobId.to_string second_stored.M.Job.id);
      Ok ())

let test_postgres_unique_key_allows_fresh_job_after_completion = fun
  _ctx postgres_url postgres_config ->
  with_postgres
    postgres_url
    postgres_config
    (fun db _jobs_config ->
      let unique_key =
        M.UniqueKey.from_string_unchecked (unique_text "postgres-completed-unique")
      in
      let first =
        expect_ok
          (M.Job.enqueue
            ~id:(M.JobId.from_string_unchecked (unique_text "postgres-completed-first"))
            ~unique_key
            issue_sync_queue
            (payload "leostera/hypekit.dev"))
      in
      let first_stored = expect_ok (M.Sqlx_backend.enqueue db first) in
      expect_ok (M.Sqlx_backend.complete db first_stored);
      let second =
        expect_ok
          (M.Job.enqueue
            ~id:(M.JobId.from_string_unchecked (unique_text "postgres-completed-second"))
            ~unique_key
            issue_sync_queue
            (payload "leostera/hypekit.dev"))
      in
      let second_stored = expect_ok (M.Sqlx_backend.enqueue db second) in
      Test.assert_true
        (not
          (String.equal
            (M.JobId.to_string first_stored.M.Job.id)
            (M.JobId.to_string second_stored.M.Job.id)));
      Test.assert_equal ~expected:M.State.Available ~actual:second_stored.M.Job.state;
      Ok ())

let test_schema_declares_suri_jobs_migration = fun _ctx ->
  Test.assert_true (String.contains M.Schema.create_jobs_sql "create table if not exists suri_jobs");
  Test.assert_true (String.contains M.Schema.create_jobs_sql "suri_jobs_unique_key_active_idx");
  Test.assert_true (String.contains M.Schema.create_fetch_index_sql "suri_jobs_fetch_idx");
  Test.assert_true (String.contains M.Schema.create_jobs_sql "job_id text not null unique");
  Test.assert_equal ~expected:2 ~actual:(Vector.length M.Schema.migrations);
  match Sqlx.Migrate.Source.resolve (M.Schema.source ()) with
  | Error error -> Error (Sqlx.Migrate.error_to_string error)
  | Ok migrations ->
      Test.assert_equal ~expected:2 ~actual:(Vector.length migrations);
      let migration = Vector.get_unchecked migrations ~at:0 in
      Test.assert_equal
        ~expected:"1"
        ~actual:(Sqlx.Migrate.Version.to_string Sqlx.Migrate.Migration.(migration.version));
      let fetch_index_migration = Vector.get_unchecked migrations ~at:1 in
      Test.assert_equal
        ~expected:"2"
        ~actual:(Sqlx.Migrate.Version.to_string
          Sqlx.Migrate.Migration.(fetch_index_migration.version));
      Ok ()

let test_routes_expose_memory_dashboard = fun _ctx ->
  let db = M.Memory.create () in
  let request =
    expect_ok
      (M.Job.enqueue
        ~id:(M.JobId.from_string_unchecked "route-visible-job")
        ~fanout_id:(M.FanoutId.from_string_unchecked "route-visible-fanout")
        issue_sync_queue
        (payload "leostera/hypekit.dev"))
  in
  let stored = expect_ok (M.Memory.enqueue db request) in
  let app =
    Suri.Middleware.[
      router
        Suri.Middleware.Router.[ forward "/__jobs" (M.Routes.routes (M.Routes.memory_store db)); ];
    ]
  in
  let job_id = M.JobId.to_string stored.M.Job.id in
  let* html_response = expect_response (Testing.App.get app "/__jobs") in
  let* () = expect_status Net.Http.Status.Ok html_response in
  let* () = expect_contains Response.(html_response.body) "Suri Jobs" in
  let* () = expect_contains Response.(html_response.body) job_id in
  let* list_response = expect_response (Testing.App.get app "/__jobs/jobs") in
  let* () = expect_status Net.Http.Status.Ok list_response in
  let* () = expect_contains Response.(list_response.body) "\"count\":1" in
  let* show_response = expect_response (Testing.App.get app ("/__jobs/jobs/" ^ job_id ^ "/json")) in
  let* () = expect_status Net.Http.Status.Ok show_response in
  let* () = expect_contains Response.(show_response.body) "\"id\":\"route-visible-job\"" in
  let* missing_response = expect_response (Testing.App.get app "/__jobs/jobs/missing-job/json") in
  expect_status Net.Http.Status.NotFound missing_response

let test_queue_intf_handle_job_is_directly_testable = fun _ctx ->
  let module Queue = struct
    let config =
      M.Queue.Config.make
        ~id:issue_sync_queue_id
        ~worker:issue_sync_worker_id
        ~concurrency:3
        ~poll_interval:(Time.Duration.from_millis 10)
        ()

    type ctx = { handled: int }

    type error = string

    type job = issue_sync_payload

    let handle_job ctx (job: job) = Ok { handled = ctx.handled + String.length job.repo_key }

    let handle_error ctx _error = Ok ctx

    let job_serializer = issue_sync_payload_encode

    let job_deserializer = issue_sync_payload_decode

    let err_serializer = Ser.string

    let err_deserializer = De.string
  end in
  let initial = Queue.{ handled = 0 } in
  let packed = M.queue (module Queue) initial in
  ignore packed;
  match Queue.handle_job initial (payload "leostera/hypekit.dev") with
  | Error error -> Error error
  | Ok ctx ->
      Test.assert_equal ~expected:(String.length "leostera/hypekit.dev") ~actual:ctx.Queue.handled;
      Test.assert_equal ~expected:3 ~actual:Queue.config.M.Queue.Config.concurrency;
      Ok ()

let test_queue_config_clamps_runtime_flags = fun _ctx ->
  let id = M.QueueId.from_string_unchecked "config-clamps" in
  let config =
    M.Queue.Config.make ~id ~concurrency:0 ~stale_after_seconds:(-10) ~retry_backoff_seconds:(-20) ()
  in
  Test.assert_equal ~expected:1 ~actual:config.M.Queue.Config.concurrency;
  Test.assert_equal ~expected:0 ~actual:config.M.Queue.Config.stale_after_seconds;
  Test.assert_equal ~expected:0 ~actual:config.M.Queue.Config.retry_backoff_seconds;
  Test.assert_equal
    ~expected:"config-clamps"
    ~actual:(M.WorkerId.to_string config.M.Queue.Config.worker);
  Ok ()

let test_supervised_queue_runs_jobs_concurrently = fun _ctx postgres_url postgres_config ->
  with_postgres
    postgres_url
    postgres_config
    (fun db jobs_config ->
      let queue_id = M.QueueId.from_string_unchecked (unique_text "suri-jobs-concurrency") in
      let worker_id = M.WorkerId.from_string_unchecked (unique_text "SuriJobsConcurrency") in
      let fanout_id = M.FanoutId.from_string_unchecked (unique_text "fanout-concurrency") in
      let in_flight = Sync.Atomic.make 0 in
      let max_in_flight = Sync.Atomic.make 0 in
      let module Queue = struct
        let config =
          M.Queue.Config.make
            ~id:queue_id
            ~worker:worker_id
            ~concurrency:4
            ~poll_interval:(Time.Duration.from_millis 5)
            ~retry_backoff_seconds:0
            ()

        type ctx = unit

        type error = string

        type job = issue_sync_payload

        let handle_job ctx _job =
          let current = Sync.Atomic.fetch_and_add in_flight 1 + 1 in
          update_max max_in_flight current;
          sleep (Time.Duration.from_millis 150);
          ignore (Sync.Atomic.fetch_and_add in_flight (-1));
          Ok ctx

        let handle_error ctx _error = Ok ctx

        let job_serializer = issue_sync_payload_encode

        let job_deserializer = issue_sync_payload_decode

        let err_serializer = Ser.string

        let err_deserializer = De.string
      end in
      let supervisor = M.start_link_with_config ~config:jobs_config [ M.queue (module Queue) () ] in
      let rec submit_jobs n =
        if n > 4 then
          Ok ()
        else
          let* _job_id =
            jobs_result
              (M.submit
                ~id:(M.JobId.from_string_unchecked
                  (unique_text ("concurrency-job-" ^ Int.to_string n)))
                ~fanout_id
                (module Queue)
                (payload ("repo/" ^ Int.to_string n)))
          in
          submit_jobs (n + 1)
      in
      let result =
        let* () = submit_jobs 1 in
        let* status = wait_for_fanout db ~fanout_id ~completed:4 ~discarded:0 in
        Test.assert_equal ~expected:4 ~actual:status.M.Fanout.completed;
        Test.assert_true (Sync.Atomic.get max_in_flight > 1);
        Ok ()
      in
      Supervisor.stop supervisor;
      result)

let test_supervised_queue_retries_failed_jobs = fun _ctx postgres_url postgres_config ->
  with_postgres
    postgres_url
    postgres_config
    (fun db jobs_config ->
      let queue_id = M.QueueId.from_string_unchecked (unique_text "suri-jobs-retry") in
      let worker_id = M.WorkerId.from_string_unchecked (unique_text "SuriJobsRetry") in
      let fanout_id = M.FanoutId.from_string_unchecked (unique_text "fanout-retry") in
      let attempts = Sync.Atomic.make 0 in
      let module Queue = struct
        let config =
          M.Queue.Config.make
            ~id:queue_id
            ~worker:worker_id
            ~concurrency:1
            ~poll_interval:(Time.Duration.from_millis 5)
            ~retry_backoff_seconds:0
            ()

        type ctx = unit

        type error = string

        type job = issue_sync_payload

        let handle_job ctx _job =
          let attempt = Sync.Atomic.fetch_and_add attempts 1 + 1 in
          if attempt = 1 then
            Error "first attempt failed"
          else
            Ok ctx

        let handle_error ctx _error = Ok ctx

        let job_serializer = issue_sync_payload_encode

        let job_deserializer = issue_sync_payload_decode

        let err_serializer = Ser.string

        let err_deserializer = De.string
      end in
      let supervisor = M.start_link_with_config ~config:jobs_config [ M.queue (module Queue) () ] in
      let result =
        let* _job_id =
          jobs_result
            (M.submit
              ~id:(M.JobId.from_string_unchecked (unique_text "retry-job"))
              ~fanout_id
              (module Queue)
              (payload "repo/retry"))
        in
        let* status = wait_for_fanout db ~fanout_id ~completed:1 ~discarded:0 in
        Test.assert_equal ~expected:1 ~actual:status.M.Fanout.completed;
        Test.assert_true (Sync.Atomic.get attempts >= 2);
        Ok ()
      in
      Supervisor.stop supervisor;
      result)

let test_supervised_queue_retries_raised_exceptions = fun _ctx postgres_url postgres_config ->
  with_postgres
    postgres_url
    postgres_config
    (fun db jobs_config ->
      let queue_id = M.QueueId.from_string_unchecked (unique_text "suri-jobs-exception-retry") in
      let worker_id = M.WorkerId.from_string_unchecked (unique_text "SuriJobsExceptionRetry") in
      let fanout_id = M.FanoutId.from_string_unchecked (unique_text "fanout-exception-retry") in
      let attempts = Sync.Atomic.make 0 in
      let module Queue = struct
        let config =
          M.Queue.Config.make
            ~id:queue_id
            ~worker:worker_id
            ~concurrency:1
            ~poll_interval:(Time.Duration.from_millis 5)
            ~retry_backoff_seconds:0
            ()

        type ctx = unit

        type error = string

        type job = issue_sync_payload

        let handle_job ctx _job =
          let attempt = Sync.Atomic.fetch_and_add attempts 1 + 1 in
          if attempt = 1 then
            raise (Failure "boom")
          else
            Ok ctx

        let handle_error ctx _error = Ok ctx

        let job_serializer = issue_sync_payload_encode

        let job_deserializer = issue_sync_payload_decode

        let err_serializer = Ser.string

        let err_deserializer = De.string
      end in
      let supervisor = M.start_link_with_config ~config:jobs_config [ M.queue (module Queue) () ] in
      let result =
        let* _job_id =
          jobs_result
            (M.submit
              ~id:(M.JobId.from_string_unchecked (unique_text "exception-retry-job"))
              ~fanout_id
              (module Queue)
              (payload "repo/exception-retry"))
        in
        let* status = wait_for_fanout db ~fanout_id ~completed:1 ~discarded:0 in
        Test.assert_equal ~expected:1 ~actual:status.M.Fanout.completed;
        Test.assert_true (Sync.Atomic.get attempts >= 2);
        Ok ()
      in
      Supervisor.stop supervisor;
      result)

let tests =
  Test.[
    case
      "queue config defaults to available parallelism"
      test_queue_config_defaults_to_available_parallelism;
    case "ids from_string validate input" test_ids_from_string_validate_input;
    case "error to_json is structured" test_error_to_json_is_structured;
    case
      "fanout add_count accumulates without looping"
      test_fanout_add_count_accumulates_without_looping;
    case "queue roundtrips payload" test_queue_roundtrips_payload;
    case "memory fetch decodes through typed queue" test_memory_fetch_decodes_through_typed_queue;
    case "worker composition is typed" test_worker_composition_is_typed;
    case "memory retry and discard" test_memory_retry_and_discard;
    case "memory rescues stale execution" test_memory_rescues_stale_execution;
    case "memory unique key returns active job" test_memory_unique_key_returns_active_job;
    case
      "memory deterministic job id is idempotent after completion"
      test_memory_deterministic_job_id_is_idempotent_after_completion;
    postgres_case
      "postgres unique key returns active job"
      test_postgres_unique_key_returns_active_job;
    postgres_case
      "postgres unique key allows fresh job after completion"
      test_postgres_unique_key_allows_fresh_job_after_completion;
    case "schema declares suri jobs migration" test_schema_declares_suri_jobs_migration;
    case "routes expose memory dashboard" test_routes_expose_memory_dashboard;
    case
      "queue intf handle_job is directly testable"
      test_queue_intf_handle_job_is_directly_testable;
    case "queue config clamps runtime flags" test_queue_config_clamps_runtime_flags;
    postgres_case
      "supervised queue runs jobs concurrently"
      test_supervised_queue_runs_jobs_concurrently;
    postgres_case "supervised queue retries failed jobs" test_supervised_queue_retries_failed_jobs;
    postgres_case
      "supervised queue retries raised exceptions"
      test_supervised_queue_retries_raised_exceptions;
  ]

let main ~args =
  Test.Cli.main
    ~execution_mode:Test.Cli.Linear
    ~teardown:teardown_postgres_container
    ~name:"suri_jobs_tests"
    ~tests
    ~args
    ()

let () = Runtime.run ~main ~args:Env.args ()
