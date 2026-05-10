open Std

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
    locked_by:Worker_id.t ->
    (('payload, 'result) Job.t list, Error.t) result

  val complete: t -> Job.stored -> (unit, Error.t) result

  val fail: t -> Job.stored -> error:string -> backoff_seconds:int -> (unit, Error.t) result

  val cancel: t -> Job.stored -> (unit, Error.t) result

  val list: t -> limit:int -> (Job.stored list, Error.t) result

  val get: t -> job_id:Job_id.t -> (Job.stored option, Error.t) result

  val state_counts: t -> (Fanout.status, Error.t) result

  val fanout_status: t -> fanout_id:Fanout_id.t -> (Fanout.status, Error.t) result
end
