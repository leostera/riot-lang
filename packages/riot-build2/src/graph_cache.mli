open Std

type 'value t

val create:
  store:Riot_store.Store.t ->
  namespace:Riot_store.Store.node_payload_namespace ->
  serialize:'value Serde.Ser.t ->
  deserialize:'value Serde.De.t ->
  'value t

val get: 'value t -> Crypto.hash -> ('value, Error.t) result option

val put: 'value t -> Crypto.hash -> 'value -> (unit, Error.t) result
