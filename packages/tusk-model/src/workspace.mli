open Std

type t = {
  root : Path.t;
  target_dir_root : Path.t;
  packages : Package.t list;
  profile_overrides : (string * Package.profile_override) list;
}

type manifest = {
  members : Path.t list;
  dependencies : Package.dependency list;
  profile_overrides : (string * Package.profile_override) list;
  target_dir : string option;
}

val of_toml : Std.Data.Toml.value -> (manifest, string) result

val make :
  root:Path.t ->
  packages:Package.t list ->
  ?profile_overrides:(string * Package.profile_override) list ->
  ?target_dir:string ->
  unit ->
  t

val project_id : t -> string
(** Get a unique project identifier for the workspace by replacing / with - in
    the root path *)

val server_port : t -> int
(** Get a unique port number for the workspace server based on workspace root
    path. Returns a port in the dynamic/private range (49152-65535) *)

val discover_commands : t -> Package_command.t list
(** Discover all package commands in the workspace by collecting commands from all packages *)

val find_command : t -> string -> Package_command.t option
(** Find a command by name in the workspace *)
