open Std

module Array = Collections.Array
module Vector = Collections.Vector
module De = Serde.De

open Std.Result.Syntax

type state = {
  mutable current: Bson_value.t;
}

let error = fun message -> raise (Serde.Decode_error (`Msg message))

let invalid_field_type = fun kind -> error ("unexpected BSON value while decoding " ^ kind)

let expect_document = fun __tmp1 ->
  match __tmp1 with
  | Bson_value.Document items -> items
  | _ -> invalid_field_type "document"

let expect_array = fun __tmp1 ->
  match __tmp1 with
  | Bson_value.Array items -> items
  | _ -> invalid_field_type "array"

let with_current = fun state value fn ->
  let previous = state.current in
  state.current <- value;
  let result =
    try fn () with
    | exn ->
        state.current <- previous;
        raise exn
  in
  state.current <- previous;
  result

let int_of_int64 = fun value ->
  try Int64.to_int value with
  | _ -> error "decoded BSON integer does not fit in OCaml int"

let int32_of_int64 = fun value ->
  if (
    match Int64.compare value (Int64.from_int32 Int32.min_int) with
    | Order.LT -> true
    | Order.EQ
    | Order.GT -> false
  ) || (
    match Int64.compare value (Int64.from_int32 Int32.max_int) with
    | Order.GT -> true
    | Order.LT
    | Order.EQ -> false
  ) then
    error "decoded BSON integer does not fit in int32"
  else
    Int64.to_int32 value

let int64_of_value = fun __tmp1 ->
  match __tmp1 with
  | Bson_value.Int32 value -> Int64.from_int32 value
  | Bson_value.Int64 value -> value
  | _ -> invalid_field_type "integer"

let map_singleton = fun __tmp1 ->
  match __tmp1 with
  | [ (key, value) ] -> Some (key, value)
  | _ -> None

let rec option_backend: 'value. state -> 'value De.t -> 'value option = fun state decode ->
  match state.current with
  | Bson_value.Null -> None
  | value -> Some (with_current state value (fun () -> decode.run backend state))

and list_backend: 'value. state -> 'value De.t -> 'value vec = fun state decode ->
  let values = expect_array state.current in
  let result = Vector.with_capacity ~size:(List.length values) in
  List.for_each
    values
    ~fn:(fun value ->
      Vector.push
        result
        ~value:(with_current state value (fun () -> decode.run backend state)));
  result

and array_backend: 'value. state -> 'value De.t -> 'value array = fun state decode ->
  let values = expect_array state.current in
  let items = ref [] in
  List.for_each
    values
    ~fn:(fun value ->
      items := with_current state value (fun () -> decode.run backend state) :: !items);
  Array.from_list (List.rev !items)

and record_backend:
  'field 'acc 'value. state ->
  fields:'field De.Fields.t ->
  init:'acc ->
  step:('acc -> 'field option -> 'acc) ->
  finish:('acc -> 'value) ->
  'value = fun state ~fields ~init ~step ~finish ->
  let acc = ref init in
  List.for_each
    (expect_document state.current)
    ~fn:(fun (key, field_value) ->
      let tag = De.Fields.match_slice fields key ~offset:0 ~length:(String.length key) in
      acc := with_current state field_value (fun () -> step !acc tag));
  finish !acc

and record_mut_backend:
  'field 'builder 'value. state ->
  fields:'field De.Fields.t ->
  create:(unit -> 'builder) ->
  step:('builder -> 'field option -> unit) ->
  finish:('builder -> 'value) ->
  'value = fun state ~fields ~create ~step ~finish ->
  let builder = create () in
  List.for_each
    (expect_document state.current)
    ~fn:(fun (key, field_value) ->
      let tag = De.Fields.match_slice fields key ~offset:0 ~length:(String.length key) in
      with_current state field_value (fun () -> step builder tag));
  finish builder

and variant_backend: 'value. state -> 'value De.compiled_variant_cases -> 'value = fun
  state cases ->
  let rec find_unit tag index =
    if Int.equal index (Array.length cases) then
      raise (Serde.Decode_error `invalid_tag)
    else
      match Array.get_unchecked cases ~at:index with
      | De.Unit (case_tag, result) ->
          if String.equal tag case_tag then
            result
          else
            find_unit tag (index + 1)
      | De.Newtype _ -> find_unit tag (index + 1)
  in
  let rec find_newtype tag payload index =
    if Int.equal index (Array.length cases) then
      raise (Serde.Decode_error `invalid_tag)
    else
      match Array.get_unchecked cases ~at:index with
      | De.Unit (case_tag, result) ->
          if String.equal tag case_tag && payload = Bson_value.Null then
            result
          else
            find_newtype tag payload (index + 1)
      | De.Newtype (case_tag, decode, wrap) ->
          if String.equal tag case_tag then
            with_current state payload (fun () -> wrap (decode.run backend state))
          else
            find_newtype tag payload (index + 1)
  in
  match state.current with
  | Bson_value.String tag -> find_unit tag 0
  | Bson_value.Document items -> (
      match map_singleton items with
      | Some (tag, payload) -> find_newtype tag payload 0
      | None -> invalid_field_type "variant"
    )
  | _ -> invalid_field_type "variant"

and backend: state De.backend = {
  bool =
    (fun state ->
      match state.current with
      | Bson_value.Bool value -> value
      | _ -> invalid_field_type "bool");
  string =
    (fun state ->
      match state.current with
      | Bson_value.String value -> value
      | _ -> invalid_field_type "string");
  int = (fun state -> int_of_int64 (int64_of_value state.current));
  int32 = (fun state -> int32_of_int64 (int64_of_value state.current));
  int64 = (fun state -> int64_of_value state.current);
  float =
    (fun state ->
      match state.current with
      | Bson_value.Double value -> value
      | Bson_value.Int32 value -> Int32.to_float value
      | Bson_value.Int64 value -> Int64.to_float value
      | _ -> invalid_field_type "float");
  skip_any = (fun _state -> ());
  option = option_backend;
  list = list_backend;
  array = array_backend;
  record = record_backend;
  record_mut = record_mut_backend;
  variant = variant_backend;
}

let from_string = fun decode input ->
  let* document = Wire.from_string input in
  De.run decode backend { current = document }

let from_reader = fun decode reader ->
  let* document = Wire.from_reader reader in
  De.run decode backend { current = document }
