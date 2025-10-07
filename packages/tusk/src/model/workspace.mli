open Std

type dependency = { name : string; version : string }

(* The manifest of a single package in a workspace *)
type package = {
  name : string;
  path : Path.t;
  relative_path : Path.t;
  dependencies : dependency list;
}

(* The manifest of a workspace *)
type t = { root : Path.t; target_dir_root : Path.t; packages : package list }

module Package : sig
  val hash :
    (module Std.Crypto.Hasher.Intf with type state = 'state) ->
    'state ->
    package ->
    unit
end

val load : root:Path.t -> (t, Error.t) result
(** Load a workspace starting at [root]. *)

val scan : Path.t -> (t, Error.t) result
(** Scans a directory and its parents until it finds a workspace root, then
    loads it *)

val project_id : t -> string
(** Get a unique project identifier for the workspace by replacing / with - in the root path *)
