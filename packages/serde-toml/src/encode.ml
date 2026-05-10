open Std
open Std.Result.Syntax

module Array = Collections.Array
module Vector = Collections.Vector
module Ser = Serde.Ser

type state = {
  mutable value: Toml_value.t option;
  allow_omit: bool;
}

let encode_error = fun message -> raise (Serde.Encode_error (`Msg message))

let expect_value = fun state kind ->
  match state.value with
  | Some value -> value
  | None -> encode_error ("TOML cannot omit " ^ kind ^ " values in this position")

let child_state = fun ?(allow_omit = false) () -> { value = None; allow_omit }

let set = fun state value -> state.value <- Some value

let rec encode_list: 'value. state -> 'value Serde.Ser.t -> 'value vec -> unit = fun
  state encode values ->
  let items = ref [] in
  Vector.for_each
    values
    ~fn:(fun value ->
      let child = child_state () in
      encode.run backend child value;
      items := expect_value child "array element" :: !items);
  set state (Toml_value.Array (List.rev !items))

and encode_array: 'value. state -> 'value Serde.Ser.t -> 'value array -> unit = fun
  state encode values ->
  let items = ref [] in
  for index = 0 to Array.length values - 1 do
    let child = child_state () in
    encode.run
      backend
      child
      (Array.get_unchecked values ~at:index);
    items := expect_value child "array element" :: !items
  done;
  set state (Toml_value.Array (List.rev !items))

and encode_map: 'value. state -> 'value Serde.Ser.t -> (string * 'value) vec -> unit = fun
  state encode values ->
  let items = ref [] in
  Vector.for_each
    values
    ~fn:(fun (name, value) ->
      let child = child_state ~allow_omit:true () in
      encode.run backend child value;
      match child.value with
      | Some map_value -> items := (name, map_value) :: !items
      | None -> ());
  set state (Toml_value.Table (List.rev !items))

and encode_record: 'value. state -> 'value Serde.Ser.fields -> 'value -> unit = fun
  state fields value ->
  let items = ref [] in
  for index = 0 to Array.length fields - 1 do
    match Array.get_unchecked fields ~at:index with
    | Ser.Field (name, encode, get) ->
        let child = child_state ~allow_omit:true () in
        encode.run backend child (get value);
        match child.value with
        | Some field_value -> items := (name, field_value) :: !items
        | None -> ()
  done;
  set state (Toml_value.Table (List.rev !items))

and encode_variant: 'value. state -> 'value Serde.Ser.variant_cases -> 'value -> unit = fun
  state cases value ->
  let rec loop index =
    if Int.equal index (Array.length cases) then
      raise (Serde.Encode_error `invalid_tag)
    else
      match Array.get_unchecked cases ~at:index with
      | Ser.Unit (tag, matches) ->
          if matches value then
            set state (Toml_value.String tag)
          else
            loop (index + 1)
      | Ser.Newtype (tag, encode, unwrap) ->
          match unwrap value with
          | Some payload ->
              let child = child_state () in
              encode.run backend child payload;
              set state (Toml_value.Table [ (tag, expect_value child "variant payload"); ])
          | None -> loop (index + 1)
  in
  loop 0

and backend: state Ser.backend = {
  bool = (fun state value -> set state (Toml_value.Bool value));
  string = (fun state value -> set state (Toml_value.String value));
  int = (fun state value -> set state (Toml_value.Int (Int64.from_int value)));
  int32 = (fun state value -> set state (Toml_value.Int (Int64.from_int32 value)));
  int64 = (fun state value -> set state (Toml_value.Int value));
  float = (fun state value -> set state (Toml_value.Float value));
  null = (fun state -> set state (Toml_value.Table []));
  option =
    (fun state encode value ->
      match value with
      | None ->
          if state.allow_omit then
            state.value <- None
          else
            encode_error "TOML has no null representation for option values in this position"
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
  | Some document -> Render.to_string document
  | None -> Render.to_string (Toml_value.Table [])

let to_writer = fun encode writer value ->
  let* encoded = to_string encode value in
  let buffer = IO.Buffer.from_string encoded in
  match IO.write_all writer ~from:buffer with
  | Ok () -> Ok ()
  | Error err -> Error (`Io_error err)
