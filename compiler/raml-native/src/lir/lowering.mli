open Std.Data

type pass_snapshot = {
  name: string;
  program: Types.Program.t;
}
type trace = {
  initial: Types.Program.t;
  passes: pass_snapshot list;
  final: Types.Program.t;
}
val trace_to_json: trace -> Json.t

val lower_program_with_trace: Mir.Program.t -> trace

val lower_program: Mir.Program.t -> Types.Program.t
