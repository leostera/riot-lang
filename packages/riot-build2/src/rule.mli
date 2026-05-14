open Std

type ocaml_source = {
  build: Goal.build_package;
  source: Source_analysis.key;
  module_name: Riot_model.Module_name.t;
}
type c_object = {
  build: Goal.build_package;
  source: Path.t;
  output: Path.t;
}
type ocaml_generated = {
  build: Goal.build_package;
  action: Action_execution.t;
}

val ocaml_source:
  build:Goal.build_package ->
  package:Riot_model.Package_name.t ->
  Riot_planner.Module_graph.source_analysis_task ->
  ocaml_source

val c_object: build:Goal.build_package -> source:Path.t -> c_object

val ocaml_generated: build:Goal.build_package -> Action_execution.t -> ocaml_generated

val c_object_output: Path.t -> Path.t

val is_interface_source: Riot_planner.Module_graph.source_analysis_task -> bool

val is_implementation_source: Riot_planner.Module_graph.source_analysis_task -> bool
