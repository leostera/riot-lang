open Std
open Std.Data

type kind =
  | Implementation
  | Interface
type t = {
  relpath: Path.t;
  unit_name: string;
  kind: kind;
  source_bytes: int;
  source_lines: int;
  nonempty_lines: int;
  has_trailing_newline: bool;
}
val from_source: relpath:Path.t -> source:string -> (t, string) result

val to_json: t -> Json.t
