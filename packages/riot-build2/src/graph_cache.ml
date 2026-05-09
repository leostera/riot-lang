open Std

type 'value t = {
  store: Riot_store.Store.t;
  namespace: Riot_store.Store.node_payload_namespace;
  serialize: 'value Serde.Ser.t;
  deserialize: 'value Serde.De.t;
}

let create = fun ~store ~namespace ~serialize ~deserialize ->
  { store; namespace; serialize; deserialize }

let get = fun t hash ->
  match Riot_store.Store.load_node_payload t.store ~namespace:t.namespace ~hash with
  | None -> None
  | Some payload ->
      Some (
        Serde_json.from_string t.deserialize payload
        |> Result.map_err
          ~fn:(fun error ->
            Error.GraphCacheDecodeFailed {
              namespace = t.namespace;
              reason = Serde.Error.to_string error;
            })
      )

let put = fun t hash value ->
  match Serde_json.to_string t.serialize value with
  | Error error ->
      Error (
        Error.GraphCacheEncodeFailed {
          namespace = t.namespace;
          reason = Serde.Error.to_string error;
        }
      )
  | Ok payload ->
      Riot_store.Store.save_node_payload t.store ~namespace:t.namespace ~hash ~payload
      |> Result.map_err
        ~fn:(fun error ->
          Error.StoreFailed {
            package = None;
            reason = Riot_store.Store.error_message error;
          })
