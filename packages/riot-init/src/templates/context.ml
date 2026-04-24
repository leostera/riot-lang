open Std

type error = string

type t = {
  on_event: Types.event -> unit;
  target_dir: Path.t;
  workspace_name: string;
  package_name: string;
  is_library: bool;
}
