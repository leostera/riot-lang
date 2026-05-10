open Std
open Std.Result.Syntax

module Array = Collections.Array
module Vector = Collections.Vector
module De = Serde.De
module Document = Parse.Builder

type state = {
  mutable current: Document.value;
}

let error = fun message -> raise (Serde.Decode_error (`Msg message))

let invalid_field_type = fun kind -> error ("unexpected TOML value while decoding " ^ kind)

let expect_int64 = fun __tmp1 ->
  match __tmp1 with
  | Document.Int value -> value
  | _ -> invalid_field_type "integer"

let expect_table = fun __tmp1 ->
  match __tmp1 with
  | Document.Table items -> items
  | _ -> invalid_field_type "table"

let expect_array = fun __tmp1 ->
  match __tmp1 with
  | Document.Array _ as value -> value
  | Document.Array_of_tables _ as value -> value
  | _ -> invalid_field_type "array"

let int_of_int64 = fun value ->
  try Int64.to_int value with
  | _ -> error "decoded TOML integer does not fit in OCaml int"

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
    error "decoded TOML integer does not fit in int32"
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

let rec option_backend: 'value. state -> 'value De.t -> 'value option = fun state decode ->
  Some (decode.run backend state)

and list_backend: 'value. state -> 'value De.t -> 'value vec = fun state decode ->
  let values = expect_array state.current in
  let result = Vector.with_capacity ~size:(Document.array_len values) in
  Document.array_iter
    (fun value ->
      Vector.push
        result
        ~value:(with_current state value (fun () -> decode.run backend state)))
    values;
  result

and array_backend: 'value. state -> 'value De.t -> 'value array = fun state decode ->
  let values = expect_array state.current in
  let items = ref [] in
  Document.array_iter
    (fun value -> items := with_current state value (fun () -> decode.run backend state) :: !items)
    values;
  Array.from_list (List.rev !items)

and map_backend: 'value. state -> 'value De.t -> (string * 'value) vec = fun state decode ->
  let values = expect_table state.current in
  let result = Vector.create () in
  Document.iter_table
    values
    (fun key value ->
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
  Document.iter_table
    (expect_table state.current)
    (fun key field_value ->
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
  Document.iter_table
    (expect_table state.current)
    (fun key field_value ->
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
          if String.equal tag case_tag && (
            match payload with
            | Document.Table table -> Document.table_is_empty table
            | _ -> false
          ) then
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
  | Document.String tag -> find_unit tag 0
  | Document.Table table -> (
      match Document.table_singleton table with
      | Some (tag, payload) -> find_newtype tag payload 0
      | None -> invalid_field_type "variant"
    )
  | _ -> invalid_field_type "variant"

and backend: state De.backend = {
  bool =
    (fun state ->
      match state.current with
      | Document.Bool value -> value
      | _ -> invalid_field_type "bool");
  string =
    (fun state ->
      match state.current with
      | Document.String value -> value
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
      | Document.Float value -> value
      | Document.Int value -> Int64.to_float value
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
  let open Result in
  let* document = Parse.parse_document input in
  De.run decode backend { current = Document.Table document }

let from_reader = fun decode reader ->
  let buffer = IO.Buffer.create ~size:256 in
  match IO.read_to_end reader ~into:buffer with
  | Ok _ -> from_string decode (IO.Buffer.contents buffer)
  | Error err -> Error (`Io_error err)
