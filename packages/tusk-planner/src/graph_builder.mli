open Std
open Tusk_model

module G = Std.Graph.SimpleGraph

type config = {
  root : Path.t;
  source_dir : Path.t;
  namespace : string;
  package : Package.t;
  toolchain : Toolchains.toolchain;
  workspace : Workspace.t;
}

type t = {
  config : config;
  graph : Module_node.t G.t;
  registry : Module_registry.t;
  entries : Module_scanner.entry list;
}

val create : config -> t
val wire_dependencies : t -> Path.t -> unit
val add_library_node : t -> name:string -> includes:Path.t list -> unit
val add_binary_node : t -> name:string -> source:Path.t -> libraries:Path.t list -> includes:Path.t list -> unit  
val graph : t -> Module_node.t G.t
val registry : t -> Module_registry.t
