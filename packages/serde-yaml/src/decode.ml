open Std
open Std.Result.Syntax

module Array = Collections.Array
module Vector = Collections.Vector
module De = Serde.De

type state = {
  mutable current: Yaml_value.t;
}

let error = fun message -> raise (Serde.Decode_error (`Msg message))

let invalid_field_type = fun kind -> error ("unexpected YAML value while decoding " ^ kind)

let expect_int64 = fun __tmp1 ->
  match __tmp1 with
  | Yaml_value.Int value -> value
  | _ -> invalid_field_type "integer"

let expect_map = fun __tmp1 ->
  match __tmp1 with
  | Yaml_value.Map items -> items
  | _ -> invalid_field_type "mapping"

let expect_seq = fun __tmp1 ->
  match __tmp1 with
  | Yaml_value.Seq items -> items
  | _ -> invalid_field_type "sequence"

let int_of_int64 = fun value ->
  try Int64.to_int value with
  | _ -> error "decoded YAML integer does not fit in OCaml int"

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
    error "decoded YAML integer does not fit in int32"
  else
    Int64.to_int32 value

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

let map_singleton = fun __tmp1 ->
  match __tmp1 with
  | [ (key, value) ] -> Some (key, value)
  | _ -> None

let rec option_backend: 'value. state -> 'value De.t -> 'value option = fun state decode ->
  match state.current with
  | Yaml_value.Null -> None
  | value -> Some (with_current state value (fun () -> decode.run backend state))

and list_backend: 'value. state -> 'value De.t -> 'value vec = fun state decode ->
  let values = expect_seq state.current in
  let result = Vector.with_capacity ~size:(List.length values) in
  List.for_each
    values
    ~fn:(fun value ->
      Vector.push
        result
        ~value:(with_current state value (fun () -> decode.run backend state)));
  result

and array_backend: 'value. state -> 'value De.t -> 'value array = fun state decode ->
  let values = expect_seq state.current in
  let items = ref [] in
  List.for_each
    values
    ~fn:(fun value ->
      items := with_current state value (fun () -> decode.run backend state) :: !items);
  Array.from_list (List.rev !items)

and map_backend: 'value. state -> 'value De.t -> (string * 'value) vec = fun state decode ->
  let values = expect_map state.current in
  let result = Vector.with_capacity ~size:(List.length values) in
  List.for_each
    values
    ~fn:(fun (key, value) ->
      Vector.push
        result
        ~value:(key, with_current state value (fun () -> decode.run backend state)));
  result

and record_backend:
  'field 'acc 'value. state ->
  fields:'field De.Fields.t ->
  init:'acc ->
  step:('acc -> 'field option -> 'acc) ->
  finish:('acc -> 'value) ->
  'value = fun state ~fields ~init ~step ~finish ->
  let acc = ref init in
  List.for_each
    (expect_map state.current)
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
    (expect_map state.current)
    ~fn:(fun (key, field_value) ->
      let tag = De.Fields.match_slice fields key ~offset:0 ~length:(String.length key) in
      with_current state field_value (fun () -> step builder tag));
  finish builder

and variant_backend: 'value. state -> 'value De.compiled_variant_cases -> 'value = fun
  state cases ->
  let rec find_unit tag payload index =
    if Int.equal index (Array.length cases) then
      raise (Serde.Decode_error `invalid_tag)
    else
      match Array.get_unchecked cases ~at:index with
      | De.Unit (case_tag, result) ->
          if String.equal tag case_tag then
            match payload with
            | None
            | Some Yaml_value.Null -> result
            | _ -> find_unit tag payload (index + 1)
          else
            find_unit tag payload (index + 1)
      | De.Newtype _ -> find_unit tag payload (index + 1)
  in
  let rec find_newtype tag payload index =
    if Int.equal index (Array.length cases) then
      raise (Serde.Decode_error `invalid_tag)
    else
      match Array.get_unchecked cases ~at:index with
      | De.Unit (case_tag, result) ->
          if String.equal tag case_tag then
            match payload with
            | Yaml_value.Null -> result
            | _ -> find_newtype tag payload (index + 1)
          else
            find_newtype tag payload (index + 1)
      | De.Newtype (case_tag, decode, wrap) ->
          if String.equal tag case_tag then
            with_current state payload (fun () -> wrap (decode.run backend state))
          else
            find_newtype tag payload (index + 1)
  in
  match state.current with
  | Yaml_value.String tag -> find_unit tag None 0
  | Yaml_value.Tagged (tag, payload) -> (
      match payload with
      | Yaml_value.Null -> (
          try find_unit tag (Some Yaml_value.Null) 0 with
          | Serde.Decode_error `invalid_tag -> find_newtype tag payload 0
        )
      | _ -> find_newtype tag payload 0
    )
  | Yaml_value.Map items -> (
      match map_singleton items with
      | Some (tag, payload) -> find_newtype tag payload 0
      | None -> invalid_field_type "variant"
    )
  | _ -> invalid_field_type "variant"

and backend: state De.backend = {
  bool =
    (fun state ->
      match state.current with
      | Yaml_value.Bool value -> value
      | _ -> invalid_field_type "bool");
  string =
    (fun state ->
      match state.current with
      | Yaml_value.String value -> value
      | _ -> invalid_field_type "string");
  int =
    (fun state ->
      expect_int64 state.current
      |> int_of_int64);
  int32 =
    (fun state ->
      expect_int64 state.current
      |> int32_of_int64);
  int64 = (fun state -> expect_int64 state.current);
  float =
    (fun state ->
      match state.current with
      | Yaml_value.Float value -> value
      | Yaml_value.Int value -> Int64.to_float value
      | _ -> invalid_field_type "float");
  skip_any = (fun _state -> ());
  option = option_backend;
  list = list_backend;
  array = array_backend;
  map = map_backend;
  record = record_backend;
  record_mut = record_mut_backend;
  variant = variant_backend;
}

let from_string = fun decode input ->
  let* document = Parse.parse_document input in
  De.run decode backend { current = document }

let from_reader = fun decode reader ->
  let buffer = IO.Buffer.create ~size:256 in
  match IO.read_to_end reader ~into:buffer with
  | Ok _ -> from_string decode (IO.Buffer.contents buffer)
  | Error err -> Error (`Io_error err)
