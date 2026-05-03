open Std
open Std.Data

(** Package-provided command metadata *)
type t = {
  name: string;
  (* demo *)
  description: string;
  (* "Run a minttea TUI demo" *)
  package_name: Package_name.t;
  (* minttea *)
  package_path: Path.t;
  (* packages/minttea *)
  command_module: string;
  (* Demo_cmd *)
  command_source: Path.t;
  (* packages/minttea/src/demo_cmd.ml *)
  command_binary: Path.t;
  (* _build/debug/out/minttea/Demo_cmd *)
}

val is_built: t -> bool

(** Check if the command binary exists *)
val status_string: t -> string

(** Human-readable status: "ready" or "not built" *)
val parse_from_toml: Toml.value list -> package_name:Package_name.t -> package_path:Path.t -> t list

(**
   Parse [[command]] declarations from TOML.

   Expected format:
   [[command]]
   name = "demo"
   help = "Run a minttea TUI demo"
   path = "src/demo_cmd.ml"
*)
(* Note: discover_all and find_by_name are in Workspace module to avoid circular dependency *)
val to_json: t -> Json.t

(** Serialize for caching/debugging *)
