open Std

type t =
  | Available
  | Scheduled
  | Executing
  | Retryable
  | Completed
  | Cancelled
  | Discarded
  | Suspended

let to_string = fun state ->
  match state with
  | Available -> "available"
  | Scheduled -> "scheduled"
  | Executing -> "executing"
  | Retryable -> "retryable"
  | Completed -> "completed"
  | Cancelled -> "cancelled"
  | Discarded -> "discarded"
  | Suspended -> "suspended"

let from_string = fun state ->
  match state with
  | "available" -> Ok Available
  | "scheduled" -> Ok Scheduled
  | "executing" -> Ok Executing
  | "retryable" -> Ok Retryable
  | "completed" -> Ok Completed
  | "cancelled" -> Ok Cancelled
  | "discarded" -> Ok Discarded
  | "suspended" -> Ok Suspended
  | value -> Error (Error.Invalid_state value)

let active = fun state ->
  match state with
  | Available
  | Scheduled
  | Executing
  | Retryable -> true
  | Completed
  | Cancelled
  | Discarded
  | Suspended -> false

let runnable = fun state ->
  match state with
  | Available
  | Scheduled
  | Retryable -> true
  | Executing
  | Completed
  | Cancelled
  | Discarded
  | Suspended -> false
