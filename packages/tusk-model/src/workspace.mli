open Std

type t = { root : Path.t; target_dir_root : Path.t; packages : Package.t list }

type manifest = {
  members : Path.t list;
  dependencies : Package.dependency list;
}

val of_toml : Std.Data.Toml.value -> (manifest, string) result

val make : root:Path.t -> packages:Package.t list -> t

val project_id : t -> string
(** Get a unique project identifier for the workspace by replacing / with - in
    the root path *)

val server_port : t -> int
(** Get a unique port number for the workspace server based on workspace root
    path. Returns a port in the dynamic/private range (49152-65535) *)
