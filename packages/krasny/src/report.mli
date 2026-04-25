open Std

type event =
  | Start of { mode: Runner.run_mode; concurrency: int }
  | File of Runner.file_result
  | Summary of Runner.summary

val event_to_json: root:Path.t -> event -> Data.Json.t

val write_text_file_result: writer:IO.Writer.t -> root:Path.t -> Runner.file_result -> unit IO.result

val write_text_summary: writer:IO.Writer.t -> mode:Runner.run_mode -> Runner.summary -> unit IO.result

val write_json_event: writer:IO.Writer.t -> root:Path.t -> event -> unit IO.result
