open Std
open Result.Syntax

module Config = struct
  type t = {
    id: Queue_id.t;
    worker: Worker_id.t;
    concurrency: int;
    stale_after_seconds: int;
    poll_interval: Time.Duration.t;
    retry_backoff_seconds: int;
  }

  let default_concurrency = Int.max 1 Thread.available_parallelism

  let make
    ~id
    ?worker
    ?(concurrency = default_concurrency)
    ?(stale_after_seconds = 900)
    ?(poll_interval = Time.Duration.from_secs 1)
    ?(retry_backoff_seconds = 60)
    () =
    let worker =
      match worker with
      | Some worker -> worker
      | None -> Worker_id.from_string_unchecked (Queue_id.to_string id)
    in
    {
      id;
      worker;
      concurrency = Int.max 1 concurrency;
      stale_after_seconds = Int.max 0 stale_after_seconds;
      poll_interval;
      retry_backoff_seconds = Int.max 0 retry_backoff_seconds;
    }
end

type ('payload, 'result) t = {
  id: Queue_id.t;
  worker: Worker_id.t;
  encode: 'payload Serde.Ser.t;
  decode: 'payload Serde.De.t;
}

let make ~id ~worker ~encode ~decode () = {
  id;
  worker;
  encode;
  decode;
}

let id queue = queue.id

let worker queue = queue.worker

let encode_args queue payload =
  match Serde_json.to_string queue.encode payload with
  | Ok value -> Ok value
  | Error error -> Error (Error.Encode_payload { queue = queue.id; error })

let decode_args queue ~job_id payload =
  match Serde_json.from_string queue.decode payload with
  | Ok value -> Ok value
  | Error error -> Error (Error.Decode_payload { queue = queue.id; job_id; error })

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

let from_intf
  (type ctx error job)
  (module Q : Intf with type ctx = ctx and type error = error and type job = job) =
  make
    ~id:Q.config.Config.id
    ~worker:Q.config.Config.worker
    ~encode:Q.job_serializer
    ~decode:Q.job_deserializer
    ()

let from_submit_intf
  (type ctx error job)
  (module Q : Intf with type ctx = ctx and type error = error and type job = job) =
  make
    ~id:Q.config.Config.id
    ~worker:Q.config.Config.worker
    ~encode:Q.job_serializer
    ~decode:Q.job_deserializer
    ()

type ('ctx, 'error, 'job) packed_queue = {
  config: Config.t;
  ctx: 'ctx;
  job_ref: 'job Ref.t;
  queue: ('job, unit) t;
  handle_job: 'ctx -> 'job -> ('ctx, 'error) result;
  handle_error: 'ctx -> 'error -> ('ctx, 'error) result;
  err_serializer: 'error Serde.Ser.t;
  err_deserializer: 'error Serde.De.t;
}

type packed =
  | Packed: ('ctx, 'error, 'job) packed_queue -> packed

let pack
  (type ctx error job)
  (intf: (module Intf with type ctx = ctx and type error = error and type job = job))
  (ctx: ctx) =
  let module Q = (val intf) in
  Packed {
    config = Q.config;
    ctx;
    job_ref = Ref.make ();
    queue = from_intf intf;
    handle_job = Q.handle_job;
    handle_error = Q.handle_error;
    err_serializer = Q.err_serializer;
    err_deserializer = Q.err_deserializer;
  }
