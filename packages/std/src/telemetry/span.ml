open Global

module Attributes = struct
  module Map = Collections.TypedKeyHashMap

  type 'a key = 'a Map.key

  type binding = Map.binding =
    | Binding: 'a key * 'a -> binding

  type t = Map.t

  let create = Map.create

  let key = Map.key

  let of_list = Map.from_list

  let copy = fun attributes -> Map.from_list (Map.values attributes)

  let get = fun attributes ~key -> Map.get attributes ~key

  let insert = fun attributes ~key ~value ->
    let _ = Map.insert attributes ~key ~value in
    attributes

  let remove = fun attributes ~key -> Map.remove attributes ~key

  let has_key = fun attributes ~key -> Map.has_key attributes ~key

  let length = Map.length

  let is_empty = Map.is_empty
end

type id = Uuid.t

type attribute = Attributes.binding

type attributes = Attributes.t

type status =
  | Succeeded
  | Failed of exn

type t = {
  id: id;
  parent_id: id option;
  name: string;
  attributes: attributes;
  started_at: Time.Instant.t;
}

type lifecycle =
  | Started of t
  | Completed of {
      span: t;
      completed_at: Time.Instant.t;
      duration: Time.Duration.t;
      status: status;
    }

let emit_lifecycle = ref (fun _event -> ())

let set_emitter = fun fn -> emit_lifecycle := fn

let id = fun span -> span.id

let id_to_string = fun id -> Uuid.to_string id

let equal_id = Uuid.equal

let parent_id = fun span -> span.parent_id

let name = fun span -> span.name

let attributes = fun span -> Attributes.copy span.attributes

let get_attribute = fun span ~key -> Attributes.get span.attributes ~key

let started_at = fun span -> span.started_at

let fresh_id = Uuid.v7

let start = fun ?span ?attributes name ->
  let attributes =
    match attributes with
    | Some attributes -> Attributes.copy attributes
    | None -> Attributes.create ()
  in
  let span = {
    id = fresh_id ();
    parent_id = Option.map span ~fn:id;
    name;
    attributes;
    started_at = Time.Instant.now ();
  }
  in
  !emit_lifecycle (Started span);
  span

let finish = fun ?(status = Succeeded) span ->
  let completed_at = Time.Instant.now () in
  let duration = Time.Instant.saturating_duration_since ~earlier:span.started_at completed_at in
  !emit_lifecycle
    (
      Completed {
        span;
        completed_at;
        duration;
        status;
      }
    )
