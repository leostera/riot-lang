open Std

type payload

val create_cache: store:Riot_store.Store.t -> payload Graph_cache.t

val payload_of_plan: Module_plan.t -> payload
