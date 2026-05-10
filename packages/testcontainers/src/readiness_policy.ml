open Std

type condition =
  | Running
  | Log of string
  | Healthcheck
  | Delay

type t = {
  condition: condition;
  duration: Time.Duration.t;
  retry: int;
}

let clamp_retry = fun retry -> Int.max 1 retry

let make = fun ~duration ~retry -> { condition = Running; duration; retry = clamp_retry retry }

let log = fun ~message ~duration ~retry -> {
  condition = Log message;
  duration;
  retry = clamp_retry retry;
}

let healthcheck = fun ~duration ~retry -> {
  condition = Healthcheck;
  duration;
  retry = clamp_retry retry;
}

let delay = fun ~duration -> { condition = Delay; duration; retry = 1 }

let condition = fun policy -> policy.condition

let duration = fun policy -> policy.duration

let retry = fun policy -> policy.retry

let interval = fun policy -> Time.Duration.div policy.duration (clamp_retry policy.retry)
