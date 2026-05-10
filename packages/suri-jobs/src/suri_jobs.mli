(** Typed, database-backed durable work queues. *)
open Std

module JobId: sig
  type t
  type error

  val from_string: string -> (t, error) result
  val from_string_unchecked: string -> t

  val create: unit -> t

  val to_string: t -> string

  val equal: t -> t -> bool

  val error_to_string: error -> string
end

module QueueId: sig
  type t
  type error

  val from_string: string -> (t, error) result
  val from_string_unchecked: string -> t

  val to_string: t -> string

  val equal: t -> t -> bool

  val error_to_string: error -> string
end

module WorkerId: sig
  type t
  type error

  val from_string: string -> (t, error) result
  val from_string_unchecked: string -> t

  val to_string: t -> string

  val equal: t -> t -> bool

  val error_to_string: error -> string
end

module FanoutId: sig
  type t
  type error

  val from_string: string -> (t, error) result
  val from_string_unchecked: string -> t

  val to_string: t -> string

  val equal: t -> t -> bool

  val error_to_string: error -> string
end

module UniqueKey: sig
  type t
  type error

  val from_string: string -> (t, error) result
  val from_string_unchecked: string -> t

  val to_string: t -> string

  val equal: t -> t -> bool

  val error_to_string: error -> string
end

module Error: sig
  type config_error =
    | Missing_env of string
    | Invalid_postgres_url of {
        env: string;
        message: string;
      }
    | Unsupported_backend of string

  type expected_field =
    | ExpectedText
    | ExpectedInt

  type missing_field =
    | FieldMissing of string
    | FieldTypeMismatch of {
        field: string;
        expected: expected_field;
        actual: string;
      }
    | JobRowMissing of JobId.t
    | ActiveUniqueKeyRowMissing of UniqueKey.t

  type t =
    | Encode_payload of {
        queue: QueueId.t;
        error: Serde.error;
      }
    | Decode_payload of {
        queue: QueueId.t;
        job_id: JobId.t;
        error: Serde.error;
      }
    | Invalid_state of string
    | Missing_field of missing_field
    | Not_started
    | Config of config_error
    | Sqlx of Sqlx.error
    | Migration of Sqlx.Migrate.error

  val to_string: t -> string

  val to_json: t -> Data.Json.t
end

module State: sig
  type t =
    | Available
    | Scheduled
    | Executing
    | Retryable
    | Completed
    | Cancelled
    | Discarded
    | Suspended

  val to_string: t -> string

  val from_string: string -> (t, Error.t) result

  val active: t -> bool

  val runnable: t -> bool
end

module Queue: sig
  module Config: sig
    type t = {
      id: QueueId.t;
      worker: WorkerId.t;
      concurrency: int;
      stale_after_seconds: int;
      poll_interval: Time.Duration.t;
      retry_backoff_seconds: int;
    }

    val make:
      id:QueueId.t ->
      ?worker:WorkerId.t ->
      ?concurrency:int ->
      ?stale_after_seconds:int ->
      ?poll_interval:Time.Duration.t ->
      ?retry_backoff_seconds:int ->
      unit ->
      t
  end

  type ('payload, 'result) t

  val make:
    id:QueueId.t ->
    worker:WorkerId.t ->
    encode:'payload Serde.Ser.t ->
    decode:'payload Serde.De.t ->
    unit ->
    ('payload, 'result) t

  val id: ('payload, 'result) t -> QueueId.t

  val worker: ('payload, 'result) t -> WorkerId.t

  val encode_args: ('payload, 'result) t -> 'payload -> (string, Error.t) result

  val decode_args: ('payload, 'result) t -> job_id:JobId.t -> string -> ('payload, Error.t) result

  module type Intf = sig
    val config: Config.t

    type ctx
    type error
    type job

    val handle_job: ctx -> job -> (ctx, error) result

    val handle_error: ctx -> error -> (ctx, error) result

    val job_serializer: job Serde.Ser.t

    val job_deserializer: job Serde.De.t

    val err_serializer: error Serde.Ser.t

    val err_deserializer: error Serde.De.t
  end

  type intf = (module Intf)
end

module Job: sig
  type stored = {
    id: JobId.t;
    queue: QueueId.t;
    worker: WorkerId.t;
    state: State.t;
    args: string;
    meta: string;
    tags: string;
    attempt: int;
    max_attempts: int;
    priority: int;
    unique_key: UniqueKey.t option;
    fanout_id: FanoutId.t option;
    parent_job_id: JobId.t option;
    locked_by: WorkerId.t option;
    locked_at: string option;
    inserted_at: string;
    scheduled_at: string;
    attempted_at: string option;
    completed_at: string option;
    discarded_at: string option;
    cancelled_at: string option;
    last_error: string option;
  }
  type ('payload, 'result) t = {
    stored: stored;
    args: 'payload;
  }
  type 'payload enqueue

  val enqueue:
    ?id:JobId.t ->
    ?max_attempts:int ->
    ?priority:int ->
    ?scheduled_at:string ->
    ?unique_key:UniqueKey.t ->
    ?fanout_id:FanoutId.t ->
    ?parent_job_id:JobId.t ->
    ?meta:string ->
    ?tags:string ->
    ('payload, 'result) Queue.t ->
    'payload ->
    ('payload enqueue, Error.t) result

  val decode: ('payload, 'result) Queue.t -> stored -> (('payload, 'result) t, Error.t) result
end

module Worker: sig
  type ('payload, 'result) t

  val make:
    ('payload, 'result) Queue.t ->
    run:(('payload, 'result) Job.t -> ('result, Error.t) result) ->
    ('payload, 'result) t

  val queue: ('payload, 'result) t -> ('payload, 'result) Queue.t

  val run: ('payload, 'result) t -> ('payload, 'result) Job.t -> ('result, Error.t) result
end

module Fanout: sig
  type status = {
    total: int;
    available: int;
    scheduled: int;
    executing: int;
    retryable: int;
    completed: int;
    cancelled: int;
    discarded: int;
    suspended: int;
  }

  val empty: status

  val add: State.t -> status -> status

  val add_count: State.t -> int -> status -> status
end

module Database: sig
  module type Intf = sig
    type t

    val migrate: t -> (unit, Error.t) result

    val enqueue: t -> 'payload Job.enqueue -> (Job.stored, Error.t) result

    val enqueue_many: t -> 'payload Job.enqueue list -> (Job.stored list, Error.t) result

    val fetch:
      t ->
      ?stale_after_seconds:int ->
      ('payload, 'result) Queue.t ->
      limit:int ->
      locked_by:WorkerId.t ->
      (('payload, 'result) Job.t list, Error.t) result

    val complete: t -> Job.stored -> (unit, Error.t) result

    val fail: t -> Job.stored -> error:string -> backoff_seconds:int -> (unit, Error.t) result

    val cancel: t -> Job.stored -> (unit, Error.t) result

    val list: t -> limit:int -> (Job.stored list, Error.t) result

    val get: t -> job_id:JobId.t -> (Job.stored option, Error.t) result

    val state_counts: t -> (Fanout.status, Error.t) result

    val fanout_status: t -> fanout_id:FanoutId.t -> (Fanout.status, Error.t) result
  end
end

type queue

val queue:
  (module Queue.Intf with type ctx = 'ctx and type error = 'error and type job = 'job) ->
  'ctx ->
  queue

val submit:
  ?id:JobId.t ->
  ?max_attempts:int ->
  ?priority:int ->
  ?scheduled_at:string ->
  ?unique_key:UniqueKey.t ->
  ?fanout_id:FanoutId.t ->
  ?parent_job_id:JobId.t ->
  ?meta:string ->
  ?tags:string ->
  (module Queue.Intf with type ctx = 'ctx and type error = 'error and type job = 'job) ->
  'job ->
  (JobId.t, Error.t) result

module Config: sig
  type t

  val spec: Std.Config.Spec.t

  val default: t

  val get: Std.Config.Spec.value -> (t, Std.Config.error) result

  val load: unit -> (t, Std.Config.error) result

  val make:
    ?pool_size:int ->
    ?pool_config:Sqlx.Config.t ->
    ?migration_config:Sqlx.Migrate.Config.t ->
    ?migration_source:Sqlx.Migrate.Source.t ->
    driver:(module Sqlx.Driver.Intf with type config = 'config) ->
    'config ->
    t
end

val child_spec: ?id:string -> queue list -> Std.Supervisor.child_spec

val child_spec_with_config:
  ?id:string ->
  config:Config.t ->
  queue list ->
  Std.Supervisor.child_spec

val start_link: queue list -> Std.Supervisor.t

val start_link_with_config: config:Config.t -> queue list -> Std.Supervisor.t

module Memory: sig
  include Database.Intf

  val create: unit -> t
end

module Schema: sig
  val create_jobs_sql: string

  val create_fetch_index_sql: string

  val migrations: Sqlx.Migrate.Migration.t Collections.Vector.t

  val source: unit -> Sqlx.Migrate.Source.t

  val migration_config: unit -> Sqlx.Migrate.Config.t

  val postgres_migration_config: unit -> Sqlx.Migrate.Config.t
end

module Sqlx_backend: sig
  include Database.Intf with type t = Sqlx.Pool.t

  val connect:
    ?pool_size:int ->
    ?pool_config:Sqlx.Config.t ->
    driver:(module Sqlx.Driver.Intf with type config = 'config) ->
    'config ->
    (t, Error.t) result

  val migrate_with:
    ?config:Sqlx.Migrate.Config.t ->
    ?source:Sqlx.Migrate.Source.t ->
    t ->
    (unit, Error.t) result

  val shutdown: t -> unit
end

module Routes: sig
  type store

  val memory_store: Memory.t -> store

  val unavailable_store: ?error:Error.t -> unit -> store

  val sqlx_store: Sqlx.Pool.t -> store

  val routes: store -> Suri.Middleware.Router.route list
end

module Supervisor: sig
  type t

  type start_error =
    | ConfigError of Std.Config.error
    | StartError of Error.t

  val start_error_to_string: start_error -> string

  val start_link: queue list -> (t, start_error) result

  val start_link_with_config: config:Config.t -> queue list -> (t, start_error) result

  val runtime: t -> Std.Supervisor.t

  val database: t -> Sqlx.Pool.t

  val stop: t -> unit

  val routes: t -> Suri.Middleware.Router.route list
end

val routes: Supervisor.t -> Suri.Middleware.Router.route list
