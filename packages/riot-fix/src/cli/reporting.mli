open Std

val print_text_result: Runner.mode -> Runner.file_result -> unit

val print_text_summary: Runner.mode -> Runner.summary -> unit

val print_json_event: Data.Json.t -> unit
