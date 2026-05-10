open Std
open Result.Syntax

type stored = {
  id: Job_id.t;
  queue: Queue_id.t;
  worker: Worker_id.t;
  state: State.t;
  args: string;
  meta: string;
  tags: string;
  attempt: int;
  max_attempts: int;
  priority: int;
  unique_key: Unique_key.t option;
  fanout_id: Fanout_id.t option;
  parent_job_id: Job_id.t option;
  locked_by: Worker_id.t option;
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

type 'payload enqueue = {
  id: Job_id.t;
  queue: Queue_id.t;
  worker: Worker_id.t;
  args: string;
  meta: string;
  tags: string;
  max_attempts: int;
  priority: int;
  scheduled_at: string;
  unique_key: Unique_key.t option;
  fanout_id: Fanout_id.t option;
  parent_job_id: Job_id.t option;
}

let enqueue
  ?id
  ?(max_attempts = 20)
  ?(priority = 0)
  ?scheduled_at
  ?unique_key
  ?fanout_id
  ?parent_job_id
  ?(meta = "{}")
  ?(tags = "[]")
  queue
  payload =
  let* args = Queue.encode_args queue payload in
  Ok {
    id = Option.unwrap_or ~default:(Job_id.create ()) id;
    queue = Queue.id queue;
    worker = Queue.worker queue;
    args;
    meta;
    tags;
    max_attempts;
    priority;
    scheduled_at = Option.unwrap_or ~default:(Clock.now ()) scheduled_at;
    unique_key;
    fanout_id;
    parent_job_id;
  }

let state_for_enqueue request =
  if Clock.lte request.scheduled_at (Clock.now ()) then
    State.Available
  else
    State.Scheduled

let stored_from_enqueue request =
  let now = Clock.now () in
  {
    id = request.id;
    queue = request.queue;
    worker = request.worker;
    state = state_for_enqueue request;
    args = request.args;
    meta = request.meta;
    tags = request.tags;
    attempt = 0;
    max_attempts = request.max_attempts;
    priority = request.priority;
    unique_key = request.unique_key;
    fanout_id = request.fanout_id;
    parent_job_id = request.parent_job_id;
    locked_by = None;
    locked_at = None;
    inserted_at = now;
    scheduled_at = request.scheduled_at;
    attempted_at = None;
    completed_at = None;
    discarded_at = None;
    cancelled_at = None;
    last_error = None;
  }

let decode queue (stored: stored) =
  let* args = Queue.decode_args queue ~job_id:stored.id stored.args in
  Ok ({ stored; args }: ('payload, 'result) t)
