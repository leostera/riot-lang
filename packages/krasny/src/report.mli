open Std

type event =
  | Start of { mode: Runner.run_mode; concurrency: int; }
  | File of Runner.file_result
  | Summary of Runner.summary
val write_text_file_result:
  writer:('dst, 'err) IO.Writer.t -> root:Path.t -> Runner.file_result -> (unit, 'err) result

val write_text_summary:
  writer:('dst, 'err) IO.Writer.t -> mode:Runner.run_mode -> Runner.summary -> (unit, 'err) result

val write_json_event: writer:('dst, 'err) IO.Writer.t -> root:Path.t -> event -> (unit, 'err) result
