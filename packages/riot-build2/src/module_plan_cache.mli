open Std

type payload

val create_cache: store:Riot_store.Store.t -> payload Graph_cache.t

val payload_of_plan: Module_plan.t -> payload

val action_executions:
  package:Riot_model.Package.t ->
  profile:Riot_model.Profile.t ->
  target:Riot_model.Target.t ->
  toolchain:Riot_toolchain.t ->
  sandbox_dir:Path.t ->
  payload ->
  (Action_execution.t list, Error.t) result
