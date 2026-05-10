open Std

module Array = Collections.Array
module Vector = Collections.Vector
module Ser = Serde.Ser

open Std.Result.Syntax

type state = {
  mutable value: Cbor_value.t option;
}

let set = fun state value -> state.value <- Some value

let child_state = fun () -> { value = None }

let encode_error = fun message -> raise (Serde.Encode_error (`Msg message))

let expect_value = fun state kind ->
  match state.value with
  | Some value -> value
  | None -> encode_error ("CBOR encoder produced no value for " ^ kind)

let rec encode_list: 'value. state -> 'value Ser.t -> 'value vec -> unit = fun
  state encode values ->
  let items = ref [] in
  Vector.for_each
    values
    ~fn:(fun value ->
      let child = child_state () in
      encode.run backend child value;
      items := expect_value child "array element" :: !items);
  set state (Cbor_value.Array (List.rev !items))

and encode_array: 'value. state -> 'value Ser.t -> 'value array -> unit = fun state encode values ->
  let items = ref [] in
  for index = 0 to Array.length values - 1 do
    let child = child_state () in
    encode.run
      backend
      child
      (Array.get_unchecked values ~at:index);
    items := expect_value child "array element" :: !items
  done;
  set state (Cbor_value.Array (List.rev !items))

and encode_map: 'value. state -> 'value Ser.t -> (string * 'value) vec -> unit = fun
  state encode values ->
  let items = ref [] in
  Vector.for_each
    values
    ~fn:(fun (name, value) ->
      let child = child_state () in
      encode.run backend child value;
      items := (name, expect_value child ("map entry '" ^ name ^ "'")) :: !items);
  set state (Cbor_value.Map (List.rev !items))

and encode_record: 'value. state -> 'value Ser.fields -> 'value -> unit = fun state fields value ->
  let items = ref [] in
  for index = 0 to Array.length fields - 1 do
    match Array.get_unchecked fields ~at:index with
    | Ser.Field (name, encode, get) ->
        let child = child_state () in
        encode.run backend child (get value);
        items := (name, expect_value child ("field '" ^ name ^ "'")) :: !items
  done;
  set state (Cbor_value.Map (List.rev !items))

and encode_variant: 'value. state -> 'value Ser.variant_cases -> 'value -> unit = fun
  state cases value ->
  let rec loop index =
    if Int.equal index (Array.length cases) then
      raise (Serde.Encode_error `invalid_tag)
    else
      match Array.get_unchecked cases ~at:index with
      | Ser.Unit (tag, matches) ->
          if matches value then
            set state (Cbor_value.Text tag)
          else
            loop (index + 1)
      | Ser.Newtype (tag, encode, unwrap) ->
          match unwrap value with
          | Some payload ->
              let child = child_state () in
              encode.run backend child payload;
              set state (Cbor_value.Map [ (tag, expect_value child "variant payload"); ])
          | None -> loop (index + 1)
  in
  loop 0

and backend: state Ser.backend = {
  bool = (fun state value -> set state (Cbor_value.Bool value));
  string = (fun state value -> set state (Cbor_value.Text value));
  int = (fun state value -> set state (Cbor_value.Int (Int64.from_int value)));
  int32 = (fun state value -> set state (Cbor_value.Int (Int64.from_int32 value)));
  int64 = (fun state value -> set state (Cbor_value.Int value));
  float = (fun state value -> set state (Cbor_value.Float value));
  null = (fun state -> set state Cbor_value.Null);
  option =
    (fun state encode value ->
      match value with
      | None -> set state Cbor_value.Null
      | Some payload -> encode.run backend state payload);
  list = encode_list;
  array = encode_array;
  map = encode_map;
  record = encode_record;
  variant = encode_variant;
}

let to_string = fun encode value ->
  let state = child_state () in
  let* () = Ser.run encode backend state value in
  match state.value with
  | Some value -> Wire.to_string value
  | None -> Error (`Msg "serde-cbor encoder produced no top-level value")

let to_writer = fun encode writer value ->
  let state = child_state () in
  let* () = Ser.run encode backend state value in
  match state.value with
  | Some value -> Wire.to_writer writer value
  | None -> Error (`Msg "serde-cbor encoder produced no top-level value")
