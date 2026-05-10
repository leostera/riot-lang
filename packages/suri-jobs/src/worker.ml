open Std

type ('payload, 'result) t = {
  queue: ('payload, 'result) Queue.t;
  run: ('payload, 'result) Job.t -> ('result, Error.t) result;
}

let make queue ~run = { queue; run }

let queue worker = worker.queue

let run worker job = worker.run job
