type t = Std.Pid.t

module Model = Model
module Config = Config
module Service = Service
module Schema = Schema
module Analyzer = Analyzer
module Indexer = Indexer

val start_link : ?data_dir:string -> unit -> t
val child_spec : id:string -> Config.t -> Std.Supervisor.child_spec
val add_package : t -> name:string -> path:Std.Path.t -> unit
val add_module :
  t ->
  package_name:Model.Package_name.t ->
  source_file:Std.Path.t ->
  module_name:Model.Module_name.t ->
  unit
val get_symbol : t -> Model.Symbol.reference -> Model.Symbol.t option
