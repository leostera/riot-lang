type 'a key = {
  witness: 'a Ref.t;
}

type any_key =
  | Key: 'a key -> any_key

type binding =
  | Binding: 'a key * 'a -> binding

type 'value entry =
  | Occupied of 'value
  | Vacant

type t = {
  values: (any_key, binding) Hashmap.t;
}

let create = fun () -> { values = Hashmap.create () }

let with_capacity = fun ~size -> { values = Hashmap.with_capacity ~size }

let key = fun () -> { witness = Ref.make () }

let equal_key = fun left right -> Ref.equal left.witness right.witness

let cast_binding: type a. a key -> binding -> a option = fun key (Binding (stored_key, value)) ->
  Ref.cast
    stored_key.witness
    key.witness
    value

let key_of_binding = fun (Binding (key, _value)) -> Key key

let from_list = fun bindings ->
  let map = with_capacity ~size:(List.length bindings) in
  List.for_each
    bindings
    ~fn:(fun binding ->
      let _ = Hashmap.insert map.values ~key:(key_of_binding binding) ~value:binding in
      ());
  map

let insert = fun map ~key ~value ->
  match Hashmap.insert map.values ~key:(Key key) ~value:(Binding (key, value)) with
  | Some previous -> cast_binding key previous
  | None -> None

let get = fun map ~key ->
  match Hashmap.get map.values ~key:(Key key) with
  | Some value -> cast_binding key value
  | None -> None

let remove = fun map ~key ->
  match Hashmap.remove map.values ~key:(Key key) with
  | Some value -> cast_binding key value
  | None -> None

let has_key = fun map ~key -> Hashmap.has_key map.values ~key:(Key key)

let length = fun map -> Hashmap.length map.values

let is_empty = fun map -> Hashmap.is_empty map.values

let clear = fun map -> Hashmap.clear map.values

let keys = fun map -> Hashmap.keys map.values

let values = fun map -> Hashmap.values map.values

let for_each = fun map ~fn -> Hashmap.for_each map.values ~fn

let fold_left = fun map ~init ~fn -> Hashmap.fold_left map.values ~init ~fn

let to_list = fun map -> Hashmap.to_list map.values

let entry = fun map ~key ->
  match get map ~key with
  | Some value -> Occupied value
  | None -> Vacant

let iter = fun map -> Hashmap.iter map.values

let mut_iter = fun map -> Hashmap.mut_iter map.values
