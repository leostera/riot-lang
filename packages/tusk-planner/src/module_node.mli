open Std
open Tusk_model
module G = Std.Graph.SimpleGraph

type kind =
  | ML of Module.t
  | MLI of Module.t
  | C
  | H
  | Other of string
  | Root
  | Library of { name : string; includes : Path.t list }
  | Binary of {
      name : string;
      source : Path.t;
      libraries : Path.t list;
      includes : Path.t list;
    }

type file =
  | Concrete of Path.t
  | Generated of { path : Path.t; contents : string }

type t = { file : file; mutable open_modules : t G.node list; kind : kind }

val file_to_string : file -> string
val make_ml : Module.t -> file -> t
val make_mli : Module.t -> file -> t
val make_c : Path.t -> t
val make_h : Path.t -> t
val make_root : unit -> t
val make_library : name:string -> includes:Path.t list -> t

val make_binary :
  name:string ->
  source:Path.t ->
  libraries:Path.t list ->
  includes:Path.t list ->
  t

val set_open_modules : t -> t G.node list -> unit
