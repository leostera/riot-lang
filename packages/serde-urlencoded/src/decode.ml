open Std

module Array = Collections.Array
module HashMap = Collections.HashMap
module Vector = Collections.Vector
module De = Serde.De

type grouped_field = {
  key: string;
  values: string array;
}

type grouped_field_acc = {
  key: string;
  values: string Vector.t;
}

type context =
  | Top_level
  | Field_values of string array

type state = {
  fields: grouped_field array;
  mutable root_consumed: bool;
  mutable context: context;
}

let invalid_field_type = fun () -> raise (Serde.Decode_error `invalid_field_type)

let raise_error = fun message -> raise (Serde.Decode_error (`Msg message))

let unsupported_top_level = fun kind -> raise_error ("unsupported top-level value type: " ^ kind)

let unsupported_nested = fun kind -> raise_error ("unsupported nested form value type: " ^ kind)

let parse_fields = fun input ->
  let pairs =
    Net.Uri.Query.parse input
    |> List.filter ~fn:(fun (key, value) -> not (String.equal key "" && String.equal value ""))
  in
  let groups = Vector.create () in
  let indices = HashMap.create () in
  List.for_each
    pairs
    ~fn:(fun (key, value) ->
      match HashMap.get indices ~key with
      | Some index ->
          let group = Vector.get_unchecked groups ~at:index in
          Vector.push group.values ~value
      | None ->
          let index = Vector.len groups in
          let values = Vector.with_capacity ~size:4 in
          Vector.push values ~value;
          Vector.push groups ~value:({ key; values }: grouped_field_acc);
          ignore (HashMap.insert indices ~key ~value:index));
  Array.init
    ~count:(Vector.len groups)
    ~fn:(fun index ->
      let group = Vector.get_unchecked groups ~at:index in
      ({ key = group.key; values = Vector.to_array group.values }: grouped_field))

let with_values = fun state values fn ->
  let prev = state.context in
  state.context <- Field_values values;
  let result =
    try fn () with
    | exn ->
        state.context <- prev;
        raise exn
  in
  state.context <- prev;
  result

let expect_single_value = fun state kind ->
  match state.context with
  | Top_level -> unsupported_top_level kind
  | Field_values values ->
      if not (Int.equal (Array.length values) 1) then
        invalid_field_type ();
      Array.get_unchecked values ~at:0

let state_of_value = fun value -> {
  fields = [||];
  root_consumed = true;
  context = Field_values [|value|];
}

let parse_bool = fun __tmp1 ->
  match __tmp1 with
  | "true" -> true
  | "false" -> false
  | _ -> invalid_field_type ()

let parse_int = fun value ->
  try Int.from_string value with
  | _ -> invalid_field_type ()

let parse_int32 = fun value ->
  try Int32.from_string value with
  | _ -> invalid_field_type ()

let parse_int64 = fun value ->
  try Int64.from_string value with
  | _ -> invalid_field_type ()

let parse_float = fun value ->
  try Float.from_string value with
  | _ -> invalid_field_type ()

let rec backend: state De.backend = {
  bool = (fun state -> parse_bool (expect_single_value state "bool"));
  string = (fun state -> expect_single_value state "string");
  int = (fun state -> parse_int (expect_single_value state "int"));
  int32 = (fun state -> parse_int32 (expect_single_value state "int32"));
  int64 = (fun state -> parse_int64 (expect_single_value state "int64"));
  float = (fun state -> parse_float (expect_single_value state "float"));
  skip_any =
    (fun state ->
      match state.context with
      | Top_level -> state.root_consumed <- true
      | Field_values _ -> ());
  option =
    (fun state decode ->
      match state.context with
      | Top_level ->
          state.root_consumed <- true;
          if Int.equal (Array.length state.fields) 0 then
            None
          else
            Some (decode.run backend state)
      | Field_values _ -> Some (decode.run backend state));
  list =
    (fun state decode ->
      match state.context with
      | Top_level -> unsupported_top_level "sequence"
      | Field_values values ->
          let result = Vector.with_capacity ~size:(Array.length values) in
          for index = 0 to Array.length values - 1 do
            let value = Array.get_unchecked values ~at:index in
            Vector.push result ~value:(decode.run backend (state_of_value value))
          done;
          result);
  array =
    (fun state decode ->
      match state.context with
      | Top_level -> unsupported_top_level "array"
      | Field_values values ->
          Array.init
            ~count:(Array.length values)
            ~fn:(fun index ->
              let value = Array.get_unchecked values ~at:index in
              decode.run backend (state_of_value value)));
  dict =
    (fun state decode ->
      match state.context with
      | Field_values _ -> unsupported_nested "dict"
      | Top_level ->
          state.root_consumed <- true;
          let result = Vector.with_capacity ~size:(Array.length state.fields) in
          for index = 0 to Array.length state.fields - 1 do
            let field = Array.get_unchecked state.fields ~at:index in
            Vector.push
              result
              ~value:(
                field.key,
                with_values state field.values (fun () -> decode.run backend state)
              )
          done;
          result);
  record =
    (fun state ~fields ~init ~step ~finish ->
      match state.context with
      | Field_values _ -> unsupported_nested "record"
      | Top_level ->
          state.root_consumed <- true;
          let acc = ref init in
          for index = 0 to Array.length state.fields - 1 do
            let field = Array.get_unchecked state.fields ~at:index in
            let tag =
              De.Fields.match_slice fields field.key ~offset:0 ~length:(String.length field.key)
            in
            acc := with_values state field.values (fun () -> step !acc tag)
          done;
          finish !acc);
  record_mut =
    (fun state ~fields ~create ~step ~finish ->
      match state.context with
      | Field_values _ -> unsupported_nested "record"
      | Top_level ->
          state.root_consumed <- true;
          let builder = create () in
          for index = 0 to Array.length state.fields - 1 do
            let field = Array.get_unchecked state.fields ~at:index in
            let tag =
              De.Fields.match_slice fields field.key ~offset:0 ~length:(String.length field.key)
            in
            with_values state field.values (fun () -> step builder tag)
          done;
          finish builder);
  variant =
    (fun state cases ->
      let value = expect_single_value state "variant" in
      let rec loop index saw_payload_case =
        if Int.equal index (Array.length cases) then
          if saw_payload_case then
            unsupported_nested "payload variant"
          else
            raise (Serde.Decode_error `invalid_tag)
        else
          match Array.get_unchecked cases ~at:index with
          | De.Unit (tag, result) ->
              if String.equal value tag then
                result
              else
                loop (index + 1) saw_payload_case
          | De.Newtype _ -> loop (index + 1) true
      in
      loop 0 false);
}

let finish_decode = fun state result ->
  match result with
  | Error _ -> result
  | Ok value ->
      if state.root_consumed || Int.equal (Array.length state.fields) 0 then
        Ok value
      else
        Error (`Msg "unsupported top-level value for application/x-www-form-urlencoded input")

let from_string = fun decode input ->
  let state = { fields = parse_fields input; root_consumed = false; context = Top_level } in
  De.run decode backend state
  |> finish_decode state

let from_reader = fun decode reader ->
  let buffer = IO.Buffer.create ~size:128 in
  match IO.read_to_end reader ~into:buffer with
  | Ok _ -> from_string decode (IO.Buffer.contents buffer)
  | Error err -> Error (`Io_error err)
