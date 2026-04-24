open Std

type error = string

type t = {
  on_event: Riot_init_types.event -> unit;
  target_dir: Path.t;
  workspace_name: string;
  package_name: string;
  is_library: bool;
}
