open Std

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

let empty = {
  total = 0;
  available = 0;
  scheduled = 0;
  executing = 0;
  retryable = 0;
  completed = 0;
  cancelled = 0;
  discarded = 0;
  suspended = 0;
}

let add state status =
  let status = { status with total = status.total + 1 } in
  match state with
  | State.Available -> { status with available = status.available + 1 }
  | State.Scheduled -> { status with scheduled = status.scheduled + 1 }
  | State.Executing -> { status with executing = status.executing + 1 }
  | State.Retryable -> { status with retryable = status.retryable + 1 }
  | State.Completed -> { status with completed = status.completed + 1 }
  | State.Cancelled -> { status with cancelled = status.cancelled + 1 }
  | State.Discarded -> { status with discarded = status.discarded + 1 }
  | State.Suspended -> { status with suspended = status.suspended + 1 }

let add_count state count status =
  if count <= 0 then
    status
  else
    let status = { status with total = status.total + count } in
    match state with
    | State.Available -> { status with available = status.available + count }
    | State.Scheduled -> { status with scheduled = status.scheduled + count }
    | State.Executing -> { status with executing = status.executing + count }
    | State.Retryable -> { status with retryable = status.retryable + count }
    | State.Completed -> { status with completed = status.completed + count }
    | State.Cancelled -> { status with cancelled = status.cancelled + count }
    | State.Discarded -> { status with discarded = status.discarded + count }
    | State.Suspended -> { status with suspended = status.suspended + count }
