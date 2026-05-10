open Std

type binding = { key: string; value: string; line: int }

type existing =
  | PreserveExisting
  | OverwriteExisting

type missing =
  | SkipMissing
  | FailMissing

type error =
  | ReadError of {
      path: Std.Path.t;
      reason: string;
    }
  | ParseError of { line: int; message: string }

let error_to_string = fun error ->
  match error with
  | ReadError { path; reason } -> "failed to read " ^ Std.Path.to_string path ^ ": " ^ reason
  | ParseError { line; message } -> "line " ^ Int.to_string line ^ ": " ^ message
