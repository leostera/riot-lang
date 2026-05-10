# Suri Jobs

`suri-jobs` provides typed, supervised background jobs for Suri applications.
Jobs are described by queue modules, serialized with Serde, stored durably, and
executed under Riot supervisors.

It supports:

- typed queue payloads and typed queue errors;
- in-memory queues for small tests;
- SQLx/PostgreSQL-backed durable queues;
- uniqueness keys for idempotent enqueueing;
- delayed jobs, retry backoff, stale execution recovery, and fanout status;
- `Supervisor.child_spec` integration for application supervision trees.

Example:

```ocaml
module EmailQueue = struct
  let config =
    Suri_jobs.Queue.Config.make
      ~id:(Suri_jobs.QueueId.from_string_unchecked "emails.welcome")
      ~concurrency:4
      ()

  type ctx = Suri_mailer.Mailer.t
  type error = string
  type job = string

  let handle_job mailer email =
    let delivery =
      Suri_mailer.Mailer.mail
        mailer
        ~to_:[ email ]
        ~subject:"Welcome"
        ~text:"Welcome to KaraokeCrowd"
        ()
    in
    match Suri_mailer.MessageDelivery.deliver_now delivery with
    | Ok () -> Ok mailer
    | Error error -> Error (Suri_mailer.Delivery.error_to_string error)

  let handle_error mailer _error = Ok mailer

  let job_serializer = Serde.Ser.string

  let job_deserializer = Serde.De.string

  let err_serializer = Serde.Ser.string

  let err_deserializer = Serde.De.string
end

let jobs =
  Suri_jobs.child_spec
    [ Suri_jobs.queue (module EmailQueue) mailer; ]
```

For a local or admin-only dashboard, start the supervised runtime and mount the
routes:

```ocaml
let jobs =
  Suri_jobs.Supervisor.start_link [ Suri_jobs.queue (module EmailQueue) mailer; ]
  |> Result.unwrap

let app =
  Suri.Middleware.[
    router [
      Suri.Middleware.Router.scope "/__admin" [
        Suri.Middleware.Router.forward "/jobs" (Suri_jobs.routes jobs);
      ];
      (* your app routes *)
    ];
  ]
```

The mounted scope exposes:

- `GET /__admin/jobs` for the HTML dashboard;
- `GET /__admin/jobs/jobs` for JSON summaries;
- `GET /__admin/jobs/jobs/:job_id` for an HTML job detail;
- `GET /__admin/jobs/jobs/:job_id/json` for JSON job detail.

Mount these routes behind your application's admin authentication middleware if
the server is reachable by users. The package routes are read-only and do not
perform authorization by themselves.

By default, `Suri_jobs.start_link` reads `SURI_JOBS_POSTGRES_URL`, runs the
package migrations into `suri_jobs_schema_migrations`, and stores jobs in the
`suri_jobs` table. Tests can use `Suri_jobs.Memory` directly, or configure a
real PostgreSQL database through `Suri_jobs.Config.make`.
